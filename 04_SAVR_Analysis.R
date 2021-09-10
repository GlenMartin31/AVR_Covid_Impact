library(survival)

#call the ACS data cleaning script:
source(here::here("Scripts", "02_ACS_Data_Cleaning.R"))


###-----------------------------------------------------------------------------------------------------------
# Apply inclusion criteria to ACS data
###-----------------------------------------------------------------------------------------------------------

ACS_Analysis_Cohort <- ACS_data_clean %>%
  filter(X_AGE_AT_OP >= 18) %>% #only look at adults
  filter(!is.na(X_1_03_NHS_NUMBER)) %>% #remove cases with missing NHS number since used to link with mortality data
  filter(!is.na(X_OPERATIVE_URGENCY)) #remove cases with missing procedure urgency

#Given the data is 'live', impose a cut-off at the 30th Nov 2020 (date of last whole month at time analysis was undertaken:
ACS_Analysis_Cohort <- ACS_Analysis_Cohort %>%
  filter(X_DATE_AND_TIME_OF_OPERATION <= dmy("30/11/2020"))

MaxMonth <- month(dmy("30/11/2020")) #Define the maximum month number observed in 2020 and included in analysis


###-----------------------------------------------------------------------------------------------------------
# Apply inclusion criteria to ACS:
#Define surgical groups in the ACS dataset (isolated AVR, AVR+CABG, or AVR+other)
###-----------------------------------------------------------------------------------------------------------

SAVR_Analysis_Cohort <- ACS_Analysis_Cohort %>%
  #Do some data cleaning on missing data within the variables used to define AVR - all "risk factor absent":
  mutate(X_CABG = fct_explicit_na(X_CABG,
                                  na_level = "0. No"),
         X_VALVE = fct_explicit_na(X_VALVE,
                                   na_level = "0. No"),
         X_MAJOR_AORTIC = fct_explicit_na(X_MAJOR_AORTIC,
                                          na_level = "0. No"),
         X_OTHER_CARDIAC_PROCS = fct_explicit_na(X_OTHER_CARDIAC_PROCS,
                                                 na_level = "0. No")
  ) %>%
  #Subset the rows that were recorded as having a valve procedure:
  filter(X_VALVE == "1. Yes") %>%
  #Define those that had an aortic valve replacement:
  mutate("AVR" = factor(ifelse(is.na(X_AORTIC_VALVE_IMPLANT_TYPE),
                               "No",
                               ifelse(X_AORTIC_VALVE_IMPLANT_TYPE == "2. Mechanical" |
                                        X_AORTIC_VALVE_IMPLANT_TYPE == "3. Biological" |
                                        X_AORTIC_VALVE_IMPLANT_TYPE == "4. Homograft" |
                                        X_AORTIC_VALVE_IMPLANT_TYPE == "5. Autograft",
                                      "Yes",
                                      "No")),
                        levels = c("No", "Yes"))) %>%
  #Restrict only to those that had an aortic valve replacement:
  filter(AVR == "Yes") %>%
  #Remove anyone still recorded as having only a repair (note 3.47 should remove these pts. anyway since
  #the NICOR data dictionary states: "3.47 identifies the type of valve being used for replacement; in the
  #case of the aortic valve this would only be chosen if a valve was implanted but not if options 3, 4, or 6 were
  #selected for variable 3.43"):
  filter(X_AORTIC_VALVE_PROCEDURE == "1. Replacement") %>% #n=26 removed (for datacut 30/06/2020)
  #Define the surgical groups of isolated AVR, AVR+CABG, or AVR+other:
  mutate("Surgical_Group" = factor(ifelse(X_CABG == "1. Yes",
                                          "AVR_CABG",
                                          ifelse(is.na(X_MITRAL_VALVE_PROCEDURE) &
                                                   is.na(X_TRICUSPID_VALVE_PROCEDURE) &
                                                   is.na(X_PULMONARY_VALVE_PROCEDURE) &
                                                   X_OTHER_CARDIAC_PROCS == "0. No" &
                                                   X_MAJOR_AORTIC == "0. No",
                                                 "Isolated_AVR",
                                                 "AVR_Other")),
                                   levels = c("Isolated_AVR", "AVR_CABG", "AVR_Other")))


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

SAVR_Analysis_Cohort <- SAVR_Analysis_Cohort %>%
  left_join(Mortality_data %>%
              filter(!is.na(DEC_CONF_NHS_NUMBER)) %>%
              rename("X_1_03_NHS_NUMBER" = DEC_CONF_NHS_NUMBER), 
            by = "X_1_03_NHS_NUMBER")
#Set a mortality flag:
SAVR_Analysis_Cohort$Mortality_Flag <- ifelse(is.na(SAVR_Analysis_Cohort$REG_DATE_OF_DEATH),
                                              0,
                                              1)
#set administrative censoring date to be the maximum date of death observed in ONS, which
#will be approximate date of last 'pull' of ONS death data:
SAVR_Analysis_Cohort$REG_DATE_OF_DEATH[
  which(is.na(SAVR_Analysis_Cohort$REG_DATE_OF_DEATH))] <- max(Mortality_data$REG_DATE_OF_DEATH[-which(as.character(Mortality_data$REG_DATE_OF_DEATH) == "9999-09-19")], na.rm = T)

#Calculate time to death/censoring:
SAVR_Analysis_Cohort$Time_To_Death <- as.numeric(SAVR_Analysis_Cohort$REG_DATE_OF_DEATH - 
                                                   date(SAVR_Analysis_Cohort$X_DATE_AND_TIME_OF_OPERATION))
SAVR_Analysis_Cohort$Time_To_Death[which(SAVR_Analysis_Cohort$Time_To_Death < 0)] <- NA

###-----------------------------------------------------------------------------------------------------------
# Calculate the Logistic cEuroScore for all patients within the data
#see http://www.euroscore.org/logisticEuroSCORE.htm and http://www.euroscore.org/logistic.pdf
###-----------------------------------------------------------------------------------------------------------
LogisticEuroScore <- SAVR_Analysis_Cohort %>%
  #Select the relevant predictors:
  select(X_AGE_AT_OP,
         X_SEX,
         X_ACTUAL_CREATININE_AT_SURGERY,
         X_EXTRACARDIAC_ARTERIOPATHY,
         X_HISTORY_OF_PULMONARY_DISEASE,
         X_HISTORY_OF_NEUROLOGICAL_DIS,
         X_HISTORY_OF_NEUROLOGICAL_DYSFN,
         X_PREVIOUS_CARDIAC_SURGERY,
         X_INTERVAL_SURGERY_AND_LAST_MI, 
         X_EJECTION_FRACTION_CATEGORY,
         X_PA_SYSTOLIC,
         X_NAT_AORTIC_VALVE_PATH, X_NAT_MITRAL_VALVE_PATH, X_NAT_TRICUSPID_VALVE_PATH, X_NAT_PULMONARY_VALVE_PATH,
         X_ANGINA_STATUS_PRE_SURGERY,
         X_OPERATIVE_URGENCY,
         
         X_RENAL_FUNCTION_DIALYSIS, X_PRE_OPERATIVE_HEART_RHYTHM, X_IV_INOTROPES_PRIOR_TO_ANAESTH, 
         X_VENTILATED_PRE_OPERATION, X_CARDIOGENIC_SHOCK_PRE_OP,
         
         X_OTHER_ACTUAL_CARDIAC_PROCS,
         
         X_AORTIC_PATH_ASCENDING_SEG1, X_AORTIC_PROC_ASCENDING_SEG2,
         X_AORTIC_PATH_ARCH_SEG1, X_AORTIC_PROC_ARCH_SEG2,
         X_AORTIC_PATH_DESC_AORTA_SEG1, X_AORTIC_PROC_DESC_AORTA_SEG2) %>%
  #Assume missing values are "risk factor absent" or mean imputation for continuous variables:
  mutate_if(is.numeric, .funs = ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate(X_SEX = fct_explicit_na(X_SEX,
                                 na_level = "1. Male"),
         X_EXTRACARDIAC_ARTERIOPATHY = fct_explicit_na(X_EXTRACARDIAC_ARTERIOPATHY,
                                                       na_level = "0. No"),
         X_HISTORY_OF_PULMONARY_DISEASE = fct_explicit_na(X_HISTORY_OF_PULMONARY_DISEASE,
                                                          na_level = "0. No chronic pulmonary disease"),
         X_HISTORY_OF_NEUROLOGICAL_DIS = fct_explicit_na(X_HISTORY_OF_NEUROLOGICAL_DIS,
                                                         na_level = "0. No history of neurological disease"),
         X_HISTORY_OF_NEUROLOGICAL_DYSFN = fct_explicit_na(X_HISTORY_OF_NEUROLOGICAL_DYSFN,
                                                           na_level = "0. No"),
         X_PREVIOUS_CARDIAC_SURGERY = fct_explicit_na(X_PREVIOUS_CARDIAC_SURGERY,
                                                      na_level = "0. No"),
         X_INTERVAL_SURGERY_AND_LAST_MI = fct_explicit_na(X_INTERVAL_SURGERY_AND_LAST_MI,
                                                          na_level = "0. No previous MI"),
         X_EJECTION_FRACTION_CATEGORY = fct_explicit_na(X_EJECTION_FRACTION_CATEGORY,
                                                        na_level = "1. Good (LVEF > 50%)"),
         X_ANGINA_STATUS_PRE_SURGERY = fct_explicit_na(X_ANGINA_STATUS_PRE_SURGERY,
                                                       na_level = "0. No angina"),
         X_OPERATIVE_URGENCY = fct_explicit_na(X_OPERATIVE_URGENCY,
                                               na_level = "1. Elective"),
         X_RENAL_FUNCTION_DIALYSIS = fct_explicit_na(X_RENAL_FUNCTION_DIALYSIS,
                                                     na_level = "0. None"),
         X_PRE_OPERATIVE_HEART_RHYTHM = fct_explicit_na(X_PRE_OPERATIVE_HEART_RHYTHM,
                                                        na_level = "0. Sinus rhythm"),
         X_IV_INOTROPES_PRIOR_TO_ANAESTH = fct_explicit_na(X_IV_INOTROPES_PRIOR_TO_ANAESTH,
                                                           na_level = "0. No"),
         X_VENTILATED_PRE_OPERATION = fct_explicit_na(X_VENTILATED_PRE_OPERATION,
                                                      na_level = "0. No"),
         X_CARDIOGENIC_SHOCK_PRE_OP = fct_explicit_na(X_CARDIOGENIC_SHOCK_PRE_OP,
                                                      na_level = "0. No")) %>%
  #Define predictor codings:
  transmute("Age" = ifelse(X_AGE_AT_OP < 60, 
                           1,
                           X_AGE_AT_OP - 58), #age is 1 if age<60, increases by one point thereafter (e.g. <=59, then 1, age=60 means 2, age 61 means 3, and so on)
            "Female" = ifelse(X_SEX == "2. Female",
                              1,
                              0),
            "Creat" = ifelse(X_ACTUAL_CREATININE_AT_SURGERY > 200,
                             1,
                             0),
            "ECA" = ifelse(X_EXTRACARDIAC_ARTERIOPATHY == "1. Yes",
                           1,
                           0),
            "Pulmonary" = ifelse(X_HISTORY_OF_PULMONARY_DISEASE == "1. COAD/emphysema or Asthma",
                                 1,
                                 0),
            "Neuro" = ifelse(X_HISTORY_OF_NEUROLOGICAL_DIS == "0. No history of neurological disease" &
                                   X_HISTORY_OF_NEUROLOGICAL_DYSFN == "0. No",
                                 0,
                                 1),
            "PreOp" = ifelse(X_PREVIOUS_CARDIAC_SURGERY == "1. Yes",
                             1,
                             0),
            "RecentMI" = ifelse(X_INTERVAL_SURGERY_AND_LAST_MI == "0. No previous MI" |
                                  X_INTERVAL_SURGERY_AND_LAST_MI == "5. MI > 90 days",
                                0,
                                1),
            "ModerateLVEF" = ifelse(X_EJECTION_FRACTION_CATEGORY == "2. Moderate (LVEF 31 - 50%)",
                                    1,
                                    0),
            "PoorLVEF" = ifelse(X_EJECTION_FRACTION_CATEGORY == "3. Poor (LVEF < 30%)",
                                1,
                                0),
            "PASystol" = ifelse(X_PA_SYSTOLIC > 60,
                                1,
                                0),
            "ActiveEndocarditis" = ifelse(is.na((str_detect(as.character(X_NAT_AORTIC_VALVE_PATH),
                                                            pattern = "3. Active infective endocarditis") |
                                                   str_detect(as.character(X_NAT_MITRAL_VALVE_PATH),
                                                              pattern = "3. Active infective endocarditis") |
                                                   str_detect(as.character(X_NAT_TRICUSPID_VALVE_PATH),
                                                              pattern = "3. Active infective endocarditis") |
                                                   str_detect(as.character(X_NAT_PULMONARY_VALVE_PATH),
                                                              pattern = "3. Active infective endocarditis"))),
                                          0,
                                          ifelse((str_detect(as.character(X_NAT_AORTIC_VALVE_PATH),
                                                             pattern = "3. Active infective endocarditis") |
                                                    str_detect(as.character(X_NAT_MITRAL_VALVE_PATH),
                                                               pattern = "3. Active infective endocarditis") |
                                                    str_detect(as.character(X_NAT_TRICUSPID_VALVE_PATH),
                                                               pattern = "3. Active infective endocarditis") |
                                                    str_detect(as.character(X_NAT_PULMONARY_VALVE_PATH),
                                                               pattern = "3. Active infective endocarditis"))==TRUE,
                                                 1,
                                                 0)),
            "UnstableAngina" = ifelse((X_ANGINA_STATUS_PRE_SURGERY == "3. Marked limitation of ordinary physical activity" |
                                         X_ANGINA_STATUS_PRE_SURGERY == "4. Symptoms at rest or minimal activity") &
                                        (X_OPERATIVE_URGENCY == "2. Urgent" |
                                           X_OPERATIVE_URGENCY == "3. Emergency"),
                                      1,
                                      0),
            "Emergency" = ifelse(X_OPERATIVE_URGENCY == "3. Emergency" |
                                   X_OPERATIVE_URGENCY == "4. Salvage" ,
                                 1,
                                 0),
            "CriticalPreOp" = ifelse(X_RENAL_FUNCTION_DIALYSIS == "3. No dialysis but Pre-operative acute renal failure (anuria or oliguria < 10ml/hour)" |
                                       X_PRE_OPERATIVE_HEART_RHYTHM == "3. Ventricular fibrillation or ventricular tachycardia" |
                                       X_IV_INOTROPES_PRIOR_TO_ANAESTH == "1. Yes" |
                                       X_VENTILATED_PRE_OPERATION == "1. Yes" |
                                       X_CARDIOGENIC_SHOCK_PRE_OP == "1. Yes",
                                     1,
                                     0),
            "SeptalRupture" = ifelse(is.na(X_OTHER_ACTUAL_CARDIAC_PROCS), 0,
                                     ifelse(str_detect(X_OTHER_ACTUAL_CARDIAC_PROCS,
                                                       pattern = "2. Acquired VSD") == TRUE,
                                            1,
                                            0)),
            "OtherThanIsolatedCABG" = 1,
            "SurgeryThoracicAorta" = ifelse(!is.na(X_AORTIC_PROC_ASCENDING_SEG2) |
                                              !is.na(X_AORTIC_PROC_ARCH_SEG2) |
                                              !is.na(X_AORTIC_PROC_DESC_AORTA_SEG2),
                                            1,
                                            0)) %>%
  #Calculate predicted risks:
  mutate("LES_LP" = -4.789594 + (0.0666354*Age) + (0.3304052*Female) + (0.4931341*Pulmonary) + 
           (0.6558917*ECA) + (0.841626*Neuro) + (1.002625*PreOp) + (0.6521653*Creat) + 
           (1.101265*ActiveEndocarditis) + (0.9058132*CriticalPreOp) + (0.5677075*UnstableAngina) +
           (0.4191643*ModerateLVEF) + (1.094443*PoorLVEF) + (0.5460218*RecentMI) + (0.7676924*PASystol) +
           (0.7127953*Emergency) + (0.5420364*OtherThanIsolatedCABG) + (1.159787*SurgeryThoracicAorta) +
           (1.462009*SeptalRupture),
         
         "LES_PR"  = exp(LES_LP) / (1+exp(LES_LP)))

##Append the calculated risk onto the full dataset
SAVR_Analysis_Cohort <- SAVR_Analysis_Cohort %>%
  mutate("LES_PR" = LogisticEuroScore$LES_PR)




###-----------------------------------------------------------------------------------------------------------
# Mortality outcomes
###-----------------------------------------------------------------------------------------------------------

Mort_Model <- coxph(Surv(Time_To_Death, Mortality_Flag) ~ strata(Surgical_Group):COVID19Period + 
                      strata(Surgical_Group):LES_LP + strata(Surgical_Group),
                    data = SAVR_Analysis_Cohort %>%
                      filter(month(X_DATE_AND_TIME_OF_OPERATION) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                      mutate("COVID19Period" = factor(ifelse(year(X_DATE_AND_TIME_OF_OPERATION) == 2020,
                                                             "DuringC19",
                                                             "PreC19"),
                                                      levels = c("PreC19", "DuringC19")),
                             
                             "LES_LP" = log(LES_PR / (1 - LES_PR))) %>%
                      #cap the mortality at 30 days to account for censoring in late-2020 data:
                      mutate(Mortality_Flag = ifelse(Time_To_Death <= 30 & Mortality_Flag == 1,
                                                     1,
                                                     ifelse(Time_To_Death > 30 & Mortality_Flag == 1,
                                                            0,
                                                            0)),
                             Time_To_Death = ifelse(Time_To_Death > 30, 30, Time_To_Death)))

# cox.zph(Mort_Model)

SAVR_Mortality_ModelOutput <- Mort_Model %>%
  broom::tidy(exponentiate = TRUE,
              conf.int = TRUE)




###-----------------------------------------------------------------------------------------------------------
# Length of Stay outcomes
###-----------------------------------------------------------------------------------------------------------

# SAVR_Analysis_Cohort %>%
#   mutate(Discharge_Alive = ifelse(pmin(X_DATE_DISCHARGE_OR_HOSP_DEATH,
#                                        REG_DATE_OF_DEATH) == REG_DATE_OF_DEATH &
#                                     Mortality_Flag == 1,
#                                   0, 1),
#          LOS = as.numeric(pmin(X_DATE_DISCHARGE_OR_HOSP_DEATH,
#                                REG_DATE_OF_DEATH) - date(X_DATE_AND_TIME_OF_OPERATION))) %>%
#   filter(!is.na(LOS),
#          LOS >= 0) %>%
#   filter(month(X_DATE_AND_TIME_OF_OPERATION) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
#   mutate("COVID19Period" = factor(ifelse(year(X_DATE_AND_TIME_OF_OPERATION) == 2020,
#                                          "DuringC19",
#                                          "PreC19"),
#                                   levels = c("PreC19", "DuringC19")))  %>% 
#   group_by(COVID19Period, Surgical_Group) %>% 
#   summarise(quantile(LOS, 0.25),
#             median(LOS),
#             quantile(LOS, 0.75))

LOS_Model <- with(SAVR_Analysis_Cohort %>%
                    mutate(Discharge_Alive = ifelse(pmin(X_DATE_DISCHARGE_OR_HOSP_DEATH,
                                                         REG_DATE_OF_DEATH) == REG_DATE_OF_DEATH &
                                                      Mortality_Flag == 1,
                                                    0, 1),
                           LOS = as.numeric(pmin(X_DATE_DISCHARGE_OR_HOSP_DEATH,
                                                 REG_DATE_OF_DEATH) - date(X_DATE_AND_TIME_OF_OPERATION))) %>%
                    filter(!is.na(LOS),
                           LOS >= 0) %>%
                    filter(month(X_DATE_AND_TIME_OF_OPERATION) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                    mutate("COVID19Period" = factor(ifelse(year(X_DATE_AND_TIME_OF_OPERATION) == 2020,
                                                           "DuringC19",
                                                           "PreC19"),
                                                    levels = c("PreC19", "DuringC19")),
                           "LES_LP" = log(LES_PR / (1 - LES_PR))),
                  coxph(Surv(LOS, Discharge_Alive) ~ strata(Surgical_Group) + 
                          strata(Surgical_Group):COVID19Period + strata(Surgical_Group):LES_LP))
# cox.zph(LOS_Model)

SAVR_LOS_ModelOutput <- LOS_Model %>%
  broom::tidy(exponentiate = TRUE,
              conf.int = TRUE) 
