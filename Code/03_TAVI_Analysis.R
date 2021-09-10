library(survival)

#call the TAVI data cleaning script:
source(here::here("Scripts", "01_TAVI_Data_Cleaning.R"))

###-----------------------------------------------------------------------------------------------------------
# Apply inclusion criteria to TAVI data
###-----------------------------------------------------------------------------------------------------------

TAVI_Analysis_Cohort <- TAVI_data_clean %>%
  filter(XAGE_AT_OP >= 18) %>% #only look at adults
  filter(!is.na(X1_03_NHS_NUMBER)) %>% #remove cases with missing NHS number since used to link with mortality data
  filter(!is.na(X7_06_PROCEDURE_URGENCY)) #remove cases with missing procedure urgency

#Given the data is 'live', impose a cut-off at the 30th Nov 2020 (date of last whole month at time analysis was undertaken:
TAVI_Analysis_Cohort <- TAVI_Analysis_Cohort %>%
  filter(X7_01_DATE_AND_TIME_OF_OPERATION <= dmy("30/11/2020"))

MaxMonth <- month(dmy("30/11/2020")) #Define the maximum month number observed in 2020 and included in analysis


###-----------------------------------------------------------------------------------------------------------
# Link with the Civil Registration of Death data
###-----------------------------------------------------------------------------------------------------------
Mortality_data <- Mortality_data %>%
  #convert all numeric variables into integer:
  mutate_at(.vars = c("DEC_AGEC"),
            as.numeric) %>%
  #turn all other variables into factors
  mutate_if(is.character, as.factor) %>%
  as_tibble()

Mortality_data$REG_DATE_OF_DEATH <- ymd(as.character(Mortality_data$REG_DATE_OF_DEATH))
Mortality_data$REG_DATE <- ymd(as.character(Mortality_data$REG_DATE))

# #Check that each unique NHS number only has one recorded data of death:
# Mortality_data %>%
#   filter(!is.na(DEC_CONF_NHS_NUMBER)) %>%
#   filter(DEC_CONF_NHS_NUMBER %in% TAVI_Analysis_Cohort$X1_03_NHS_NUMBER) %>%
#   group_by(DEC_CONF_NHS_NUMBER) %>%
#   summarise("uni" = n_distinct(REG_DATE_OF_DEATH), .groups = "drop") %>%
#   ungroup() %>%
#   summarise(min(uni),
#             median(uni),
#             max(uni))

TAVI_Analysis_Cohort <- TAVI_Analysis_Cohort %>%
  left_join(Mortality_data %>%
              filter(!is.na(DEC_CONF_NHS_NUMBER)) %>%
              rename("X1_03_NHS_NUMBER" = DEC_CONF_NHS_NUMBER), 
            by = "X1_03_NHS_NUMBER")
#Set a mortality flag:
TAVI_Analysis_Cohort$Mortality_Flag <- ifelse(is.na(TAVI_Analysis_Cohort$REG_DATE_OF_DEATH),
                                              0,
                                              1)
#set administrative censoring date to be the maximum date of death observed in ONS, which
#will be approximate date of last 'pull' of ONS death data:
TAVI_Analysis_Cohort$REG_DATE_OF_DEATH[
  which(is.na(TAVI_Analysis_Cohort$REG_DATE_OF_DEATH))] <- max(Mortality_data$REG_DATE_OF_DEATH[-which(as.character(Mortality_data$REG_DATE_OF_DEATH) == "9999-09-19")], na.rm = T)

#Calculate time to death/censoring:
TAVI_Analysis_Cohort$Time_To_Death <- as.numeric(TAVI_Analysis_Cohort$REG_DATE_OF_DEATH - 
                                                   date(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION))
TAVI_Analysis_Cohort$Time_To_Death[which(TAVI_Analysis_Cohort$Time_To_Death < 0)] <- NA

###-----------------------------------------------------------------------------------------------------------
# Calculate the UK TAVI Risk prediction model for all patients within the data
###-----------------------------------------------------------------------------------------------------------

uk_TAVI_CPM <- TAVI_Analysis_Cohort %>%
  #Select the relevant predictors:
  select(X5_01_HEIGHT, X5_02_WEIGHT,
         X5_031_CRITICAL_PRE_OPERATIVE_STATUS_V4,
         X3_09_EXTRACARDIAC_ARTERIOPATHY,
         X1_07_SEX,
         X1_08_ETHNIC_ORIGIN, X3_03_CREATININE,
         XAGE_AT_OP,
         X7_10_DELIVERY_APPROACH,
         X6_012_PA_SYSTOLIC_PRESSURE_MMHG,
         X4_021_BALLOON_AORTIC_VALVULOPLASTY_PRIOR_TO_DATE_OF_TAVI,
         X3_06_HISTORY_OF_PULMONARY_DISEASE,
         X3_11_2_Sinus,
         X7_06_PROCEDURE_URGENCY,
         X5_052_KATZ,
         X3_091_POOR_MOBILITY) %>%
  #Assume missing values are "risk factor absent" or mean imputation for continuous variables:
  mutate_if(is.numeric, .funs = ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate(X5_031_CRITICAL_PRE_OPERATIVE_STATUS_V4 = fct_explicit_na(X5_031_CRITICAL_PRE_OPERATIVE_STATUS_V4,
                                                                   na_level = "0. No"),
         X3_09_EXTRACARDIAC_ARTERIOPATHY = fct_explicit_na(X3_09_EXTRACARDIAC_ARTERIOPATHY,
                                                           na_level = "0. No"),
         X1_07_SEX = fct_explicit_na(X1_07_SEX,
                                     na_level = "1. Male"),
         X1_08_ETHNIC_ORIGIN = fct_explicit_na(X1_08_ETHNIC_ORIGIN,
                                               na_level = "1. White"),
         X7_10_DELIVERY_APPROACH = fct_explicit_na(X7_10_DELIVERY_APPROACH,
                                                   na_level = "1. Femoral - percutaneous"),
         X4_021_BALLOON_AORTIC_VALVULOPLASTY_PRIOR_TO_DATE_OF_TAVI = fct_explicit_na(
           X4_021_BALLOON_AORTIC_VALVULOPLASTY_PRIOR_TO_DATE_OF_TAVI,
           na_level = "0. No"),
         X3_06_HISTORY_OF_PULMONARY_DISEASE = fct_explicit_na(X3_06_HISTORY_OF_PULMONARY_DISEASE,
                                                              na_level = "0. No pulmonary disease"),
         X3_11_2_Sinus = fct_explicit_na(X3_11_2_Sinus,
                                         na_level = "1. Yes"),
         X7_06_PROCEDURE_URGENCY = fct_explicit_na(X7_06_PROCEDURE_URGENCY,
                                                   na_level = "1. Elective"),
         X3_091_POOR_MOBILITY = fct_explicit_na(X3_091_POOR_MOBILITY,
                                                na_level = "0. No")) %>%
  #Define predictor codings:
  transmute("BMI" = ((X5_02_WEIGHT)/(X5_01_HEIGHT^2))-27.3,
            "BMI.sq" = BMI^2,
            "Critical_PreOp" = ifelse(X5_031_CRITICAL_PRE_OPERATIVE_STATUS_V4 == "1. Yes",
                                      1,
                                      0),
            "Extracardiac_Arteriopathy" = ifelse(X3_09_EXTRACARDIAC_ARTERIOPATHY == "1. Yes",
                                                 1,
                                                 0),
            "Female" = ifelse(X1_07_SEX == "2. Female", 1, 0),
            "eGFR" = ifelse(X1_07_SEX == "2. Female",
                            ifelse(X1_08_ETHNIC_ORIGIN == "8. Other",
                                   (175*((X3_03_CREATININE/88.4)^-1.154)*(XAGE_AT_OP^(-0.203))*(0.742)*(1.212)),
                                   (175*((X3_03_CREATININE/88.4)^-1.154)*(XAGE_AT_OP^(-0.203))*(0.742))),
                            ifelse(X1_08_ETHNIC_ORIGIN == "8. Other",
                                   (175*((X3_03_CREATININE/88.4)^-1.154)*(XAGE_AT_OP^(-0.203))*(1.212)),
                                   (175*((X3_03_CREATININE/88.4)^-1.154)*(XAGE_AT_OP^(-0.203))))),
            "floor_eGFR" = floor(eGFR/5),
            "Age" = XAGE_AT_OP - 81.25,
            "NonTF_Access" = ifelse(X7_10_DELIVERY_APPROACH == "1. Femoral - percutaneous" |
                                      X7_10_DELIVERY_APPROACH == "2. Femoral - surgical",
                                    0,
                                    1),
            "PA_Over_60" = ifelse(X6_012_PA_SYSTOLIC_PRESSURE_MMHG >= 60,
                                  1,
                                  0),
            "PriorBAV" = ifelse(X4_021_BALLOON_AORTIC_VALVULOPLASTY_PRIOR_TO_DATE_OF_TAVI == "1. Yes",
                                1, 0),
            "PulmonaryDisease" = ifelse(X3_06_HISTORY_OF_PULMONARY_DISEASE == "0. No pulmonary disease",
                                        0, 1),
            "Sinus" = ifelse(X3_11_2_Sinus == "1. Yes", 1, 0),
            "NonElective" = ifelse(X7_06_PROCEDURE_URGENCY == "1. Elective", 0, 1),
            "KATZ" = 6 - X5_052_KATZ,
            "PoorMobility" = ifelse(X3_091_POOR_MOBILITY == "0. No", 0, 1)
            ) %>%
  #Calculate predicted risks:
  mutate("UK_TAVI_CPM_LP" = -3.6119 + (-0.0257*BMI) + (0.0011*BMI.sq) + (0.5914*Critical_PreOp) +
           (0.1912*Extracardiac_Arteriopathy) + (0.1393*Female) + (-0.0342*floor_eGFR) +
           (0.0115*Age) + (0.5436*NonTF_Access) + (0.1867*PA_Over_60) + (0.2469*PriorBAV) +
           (0.214*PulmonaryDisease) + (-0.1798*Sinus) + (0.3719*NonElective) + (0.2362*KATZ) +
           (0.6302*PoorMobility),
         
         "UK_TAVI_CPM_PR"  = exp(UK_TAVI_CPM_LP) / (1+exp(UK_TAVI_CPM_LP)))

##Append the calculated risk onto the full dataset
TAVI_Analysis_Cohort <- TAVI_Analysis_Cohort %>%
  mutate("UK_TAVI_CPM_PR" = ifelse(uk_TAVI_CPM$UK_TAVI_CPM_PR == 0,
                                   0.00001,
                                   uk_TAVI_CPM$UK_TAVI_CPM_PR))





###-----------------------------------------------------------------------------------------------------------
# Mortality outcomes
###-----------------------------------------------------------------------------------------------------------

Mort_Model <- coxph(Surv(Time_To_Death, Mortality_Flag) ~ COVID19Period + TAVR_CPM_LP,
                    data = TAVI_Analysis_Cohort %>%
                      filter(month(X7_01_DATE_AND_TIME_OF_OPERATION) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                      mutate("COVID19Period" = factor(ifelse(year(X7_01_DATE_AND_TIME_OF_OPERATION) == 2020,
                                                             "DuringC19",
                                                             "PreC19"),
                                                      levels = c("PreC19", "DuringC19")),
                             
                             "TAVR_CPM_LP" = log(UK_TAVI_CPM_PR / (1 - UK_TAVI_CPM_PR))) %>%
                      #cap the mortality at 30 days to account for censoring in late-2020 data:
                      mutate(Mortality_Flag = ifelse(Time_To_Death <= 30 & Mortality_Flag == 1,
                                                     1,
                                                     ifelse(Time_To_Death > 30 & Mortality_Flag == 1,
                                                            0,
                                                            0)),
                             Time_To_Death = ifelse(Time_To_Death > 30, 30, Time_To_Death)))
# cox.zph(Mort_Model)

TAVR_Mortality_ModelOutput <- Mort_Model %>%
  broom::tidy(exponentiate = TRUE,
              conf.int = TRUE)



###-----------------------------------------------------------------------------------------------------------
# Length of Stay outcomes
###-----------------------------------------------------------------------------------------------------------

# TAVI_Analysis_Cohort %>%
#   mutate(Discharge_Alive = ifelse(pmin(X10_01_DATE_OF_DISCHARGE_OR_DEATH,
#                                        REG_DATE_OF_DEATH) == REG_DATE_OF_DEATH &
#                                     Mortality_Flag == 1,
#                                   0, 1),
#          LOS = as.numeric(pmin(X10_01_DATE_OF_DISCHARGE_OR_DEATH,
#                                REG_DATE_OF_DEATH) - date(X7_01_DATE_AND_TIME_OF_OPERATION))) %>%
#   filter(!is.na(LOS),
#          LOS >= 0) %>%
#   filter(month(X7_01_DATE_AND_TIME_OF_OPERATION) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
#   mutate("COVID19Period" = factor(ifelse(year(X7_01_DATE_AND_TIME_OF_OPERATION) == 2020,
#                                          "DuringC19",
#                                          "PreC19"),
#                                   levels = c("PreC19", "DuringC19")))  %>% 
#   group_by(COVID19Period) %>% 
#   summarise(quantile(LOS, 0.25),
#             median(LOS),
#             quantile(LOS, 0.75))

LOS_Model <- with(survSplit(Surv(LOS, Discharge_Alive) ~.,
                            data = TAVI_Analysis_Cohort %>%
                              mutate(Discharge_Alive = ifelse(pmin(X10_01_DATE_OF_DISCHARGE_OR_DEATH,
                                                                   REG_DATE_OF_DEATH) == REG_DATE_OF_DEATH &
                                                                Mortality_Flag == 1,
                                                              0, 1),
                                     LOS = as.numeric(pmin(X10_01_DATE_OF_DISCHARGE_OR_DEATH,
                                                           REG_DATE_OF_DEATH) - date(X7_01_DATE_AND_TIME_OF_OPERATION))) %>%
                              filter(!is.na(LOS),
                                     LOS >= 0) %>%
                              filter(month(X7_01_DATE_AND_TIME_OF_OPERATION) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                              mutate(LOS = ifelse(LOS==0, 0.0001, LOS),
                                     "COVID19Period" = factor(ifelse(year(X7_01_DATE_AND_TIME_OF_OPERATION) == 2020,
                                                                     "DuringC19",
                                                                     "PreC19"),
                                                              levels = c("PreC19", "DuringC19")),
                                     "TAVR_CPM_LP" = log(UK_TAVI_CPM_PR / (1 - UK_TAVI_CPM_PR))),
                            cut = c(1,2), 
                            episode = "timecat") %>%
                    mutate("timecat" = factor(ifelse(timecat == 1,
                                                     "0-1 Days",
                                                     ifelse(timecat == 2,
                                                            "1-2 Days",
                                                            "2+ Days")))),
                  coxph(Surv(tstart, LOS, Discharge_Alive) ~ strata(timecat) + 
                          strata(timecat):COVID19Period + 
                          strata(timecat):TAVR_CPM_LP))

TAVR_LOS_ModelOutput <- LOS_Model %>%
  broom::tidy(exponentiate = TRUE,
              conf.int = TRUE) 
