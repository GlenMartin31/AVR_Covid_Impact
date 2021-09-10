#Call the final analysis script (which in turn calls all analysis, data loading and cleaning scripts):
source(here::here("Scripts", "05_Negative_Binomial_Models.R"))

###-----------------------------------------------------------------------------------------------------------
# Table: create baseline summary table
###-----------------------------------------------------------------------------------------------------------

TableOneData <- SAVR_Analysis_Cohort %>%
  #Recode come of the factor levels so that the SAVR and TAVR datasets match (for the purposes of a joined table 1)
  mutate(X_INTERVAL_SURGERY_AND_LAST_MI = fct_collapse(X_INTERVAL_SURGERY_AND_LAST_MI,
                                                       "0. No" = c("0. No previous MI"),
                                                       "1. Yes" = c("1. MI < 6 hours",
                                                                    "2. MI 6-24 hours",
                                                                    "3. MI 1-30 days",
                                                                    "4. MI 31-90 days",
                                                                    "5. MI > 90 days")),
         X_DYSPNOEA_STATUS_PRE_SURGERY = fct_collapse(X_DYSPNOEA_STATUS_PRE_SURGERY,
                                                      "Class I" = "1. No limitation of physical activity",
                                                      "Class II" = "2. Slight limitation of ordinary physical activity",
                                                      "Class III or IV" = c("3. Marked limitation of ordinary physical activity",
                                                                           "4. Symptoms at rest or minimal activity")),
         X_ANGINA_STATUS_PRE_SURGERY = fct_collapse(X_ANGINA_STATUS_PRE_SURGERY,
                                                    "No angina" = "0. No angina",
                                                    "Class I" = "1. No limitation of physical activity",
                                                    "Class II" = "2. Slight limitation of ordinary activity",
                                                    "Class III or IV" = c("3. Marked limitation of ordinary physical activity",
                                                                          "4. Symptoms at rest or minimal activity")),
         X_PREVIOUS_PCI = fct_collapse(X_PREVIOUS_PCI,
                                       "0. No" = c("0. No previous PCI"),
                                       "1. Yes" = c("1. PCI < 24 hours before surgery",
                                                    "2. PCI > 24 hours before surgery; same admission",
                                                    "3. PCI > 24 hours before surgery; previous admission")),
         X_RENAL_FUNCTION_DIALYSIS = fct_collapse(X_RENAL_FUNCTION_DIALYSIS,
                                                  "0. No" = c("0. None",
                                                              "3. No dialysis but Pre-operative acute renal failure (anuria or oliguria < 10ml/hour)"),
                                                  "1. Yes" = c("1. Dialysis for acute renal failure: onset within 6 weeks of cardiac surgery",
                                                               "2. Dialysis for chronic renal failure: onset more than 6 weeks prior to cardiac surgery")),
         X_HISTORY_OF_NEUROLOGICAL_DIS = fct_collapse(X_HISTORY_OF_NEUROLOGICAL_DIS,
                                                      "0. No" = c("0. No history of neurological disease"),
                                                      "1. Yes" = c("1. TIA",
                                                                   "2. CVA with full recovery",
                                                                   "3. CVA with residual deficit")),
         X_DIABETES_MANAGEMENT = fct_collapse(X_DIABETES_MANAGEMENT,
                                              "Not Diabetic" = c("0. Not Diabetic"),
                                              "Diabetic" = c("1. Diet",
                                                             "2. Oral therapy",
                                                             "3. Insulin")),
         X_EXTENT_OF_CORONARY_VESSEL_DIS = fct_collapse(X_EXTENT_OF_CORONARY_VESSEL_DIS,
                                                        "No vessel with >50% diameter stenosis" = "0. No vessel with >50% diameter stenosis",
                                                        "One or more vessel with >50% diameter stenosis" = c("1. One vessel with >50% diameter stenosis",
                                                                                                             "2. Two vessels with >50% diameter stenosis",
                                                                                                             "3. Three vessels with >50% diameter stenosis")),
         
         X_OPERATIVE_URGENCY = fct_collapse(X_OPERATIVE_URGENCY,
                                            "Elective" = c("1. Elective"),
                                            "Non-Elective" = c("2. Urgent",
                                                               "3. Emergency",
                                                               "4. Salvage")),
         
         "Proc_Date" = X_DATE_AND_TIME_OF_OPERATION,
         LES_PR = LES_PR*100) %>%
  #Select the variables that we want to summarise (common to both datasets)
  select("Age, years" = X_AGE_AT_OP,
         "Sex" = X_SEX,
         "CCS Angina status" = X_ANGINA_STATUS_PRE_SURGERY,
         "NYHA" = X_DYSPNOEA_STATUS_PRE_SURGERY,
         "Previous MI" = X_INTERVAL_SURGERY_AND_LAST_MI,
         "Previous PCI" = X_PREVIOUS_PCI,
         "Previous Cardiac Surgery" = X_PREVIOUS_CARDIAC_SURGERY,
         "Diabetes" = X_DIABETES_MANAGEMENT,
         "Smoking Status" = X_CIGARETTE_SMOKING_HISTORY,
         "Creatinine, umol/L" = X_ACTUAL_CREATININE_AT_SURGERY,
         "History of neurological disease" = X_HISTORY_OF_NEUROLOGICAL_DIS,
         "Extracardiac Arteriopathy" = X_EXTRACARDIAC_ARTERIOPATHY,
         "Atrial Fibrillation or Flutter" = X_Atrial_Fib_Flutter,
         "Extent of coronary vessel disease" = X_EXTENT_OF_CORONARY_VESSEL_DIS,
         "PA Systolic" = X_PA_SYSTOLIC,
         "LV Function" = X_EJECTION_FRACTION_CATEGORY,
         "Height, m" = X_HEIGHT,
         "Weight, kg" = X_WEIGHT,
         "Procedure urgency" = X_OPERATIVE_URGENCY,
         
         Proc_Date,
         "Logistic EuroSCORE" = LES_PR,
         Surgical_Group) %>%
  #Stack the SAVR and TAVI cohorts together:
  bind_rows(TAVI_Analysis_Cohort %>%
              #Recode come of the factor levels so that the SAVR and TAVR datasets match (for the purposes of a joined table 1)
              mutate(X3_05_PREVIOUS_MI_AND_INTERVAL_BETWEEN_PROCEDURE_AND_LAST_MI = fct_collapse(X3_05_PREVIOUS_MI_AND_INTERVAL_BETWEEN_PROCEDURE_AND_LAST_MI,
                                                                   "0. No" = c("0. No previous MI"),
                                                                   "1. Yes" = c("1. MI < 6 hours",
                                                                                "2. MI 6-24 hours",
                                                                                "3. MI 1-30 days",
                                                                                "4. MI 31-90 days",
                                                                                "5. MI > 90 days")),
                     X5_05_NYHA_DYSPNOEA_STATUS_PRE_PROCEDURE_STABLE_ONLY = fct_collapse(X5_05_NYHA_DYSPNOEA_STATUS_PRE_PROCEDURE_STABLE_ONLY,
                                                                  "Class I" = "1. No limitation of physical activity",
                                                                  "Class II" = "2. Slight limitation of ordinary physical activity",
                                                                  "Class III or IV" = c("3. Marked limitation of ordinary physical activity",
                                                                                       "4. Symptoms at rest or minimal activity")),
                     X5_04_CCS_ANGINA_STATUS_PRE_PROCEDURE_STABLE_ONLY = fct_collapse(X5_04_CCS_ANGINA_STATUS_PRE_PROCEDURE_STABLE_ONLY,
                                                                                      "No angina" = "0. No angina",
                                                                                      "Class I" = "1. No limitation of physical activity",
                                                                                      "Class II" = "2. Slight limitation of ordinary activity",
                                                                                      "Class III or IV" = c("3. Marked limitation of ordinary physical activity",
                                                                                                            "4. Symptoms at rest or minimal activity")),
                     X4_03_PREVIOUS_PCI = fct_collapse(X4_03_PREVIOUS_PCI,
                                                       "0. No" = c("0. No"),
                                                       "1. Yes" = c("1. Yes - previous standalone PCI (NOT as part of staged or hybrid procedure)",
                                                                    "2. Yes - as part of a staged or hybrid procedure")),
                     X4_01_PREVIOUS_CARDIAC_SURGERY = factor(ifelse(X4_01_PREVIOUS_CARDIAC_SURGERY == "0. No",
                                                                    "0. No",
                                                                    "1. Yes")),
                     X3_01_DIABETES = fct_collapse(X3_01_DIABETES,
                                                   "Not Diabetic" = c("0. Not Diabetic"),
                                                   "Diabetic" = c("1. Diabetes (dietary control)",
                                                                  "2. Diabetes (oral medicine)",
                                                                  "3. Diabetes (insulin)",
                                                                  "4. Newly diagnosed diabetes")),
                     X3_08_HISTORY_OF_NEUROLOGICAL_DISEASE = fct_collapse(X3_08_HISTORY_OF_NEUROLOGICAL_DISEASE,
                                                                          "0. No" = c("0. No history of neurological disease"),
                                                                          "1. Yes" = c("1. TIA or RIND",
                                                                                       "2. CVA with full recovery",
                                                                                       "3. CVA with residual deficit",
                                                                                       "4. Other history of neurological dysfunction")),
                     X6_08_LV_FUNCTION = fct_collapse(X6_08_LV_FUNCTION,
                                                      "1. Good (LVEF > 50%)" = "1. Good (LVEF >=50%)",
                                                      "2. Moderate (LVEF 31 - 50%)" = "2. Fair (LVEF = 30-49%)",
                                                      "3. Poor (LVEF < 30%)" = "3. Poor (LVEF <30%)"),
                     X6_09_EXTENT_OF_CORONARY_VESSEL_DISEASE_IGNORING_LMS = fct_collapse(X6_09_EXTENT_OF_CORONARY_VESSEL_DISEASE_IGNORING_LMS,
                                                                    "No vessel with >50% diameter stenosis" = "0. No vessel with >50% diameter stenosis",
                                                                    "One or more vessel with >50% diameter stenosis" = c("1. One vessel with >50% diameter stenosis",
                                                                                                                         "2. Two vessels with >50% diameter stenosis",
                                                                                                                         "3. Three vessels with >50% diameter stenosis")),
                     
                     X7_06_PROCEDURE_URGENCY = fct_collapse(X7_06_PROCEDURE_URGENCY,
                                                            "Elective" = c("1. Elective"),
                                                            "Non-Elective" = c("2. Urgent",
                                                                               "3. Emergency",
                                                                               "4. Salvage")),
                     
                     Proc_Date = X7_01_DATE_AND_TIME_OF_OPERATION,
                     UK_TAVI_CPM_PR = UK_TAVI_CPM_PR*100) %>%
              #Select the variables that we want to summarise (common to both datasets)
              select("Age, years" = XAGE_AT_OP,
                     "Sex" = X1_07_SEX,
                     "CCS Angina status" = X5_04_CCS_ANGINA_STATUS_PRE_PROCEDURE_STABLE_ONLY,
                     "NYHA" = X5_05_NYHA_DYSPNOEA_STATUS_PRE_PROCEDURE_STABLE_ONLY,
                     "Previous MI" = X3_05_PREVIOUS_MI_AND_INTERVAL_BETWEEN_PROCEDURE_AND_LAST_MI,
                     "Previous PCI" = X4_03_PREVIOUS_PCI,
                     "Previous Cardiac Surgery" = X4_01_PREVIOUS_CARDIAC_SURGERY,
                     "Diabetes" = X3_01_DIABETES,
                     "Smoking Status" = X3_02_SMOKING_STATUS,
                     "Creatinine, umol/L" = X3_03_CREATININE,
                     "History of neurological disease" = X3_08_HISTORY_OF_NEUROLOGICAL_DISEASE,
                     "Extracardiac Arteriopathy" = X3_09_EXTRACARDIAC_ARTERIOPATHY,
                     "Atrial Fibrillation or Flutter" = X3_11_1_Atrial_Fib_Flutter,
                     "Extent of coronary vessel disease" =X6_09_EXTENT_OF_CORONARY_VESSEL_DISEASE_IGNORING_LMS,
                     "PA Systolic" = X6_012_PA_SYSTOLIC_PRESSURE_MMHG,
                     "LV Function" = X6_08_LV_FUNCTION,
                     "Height, m" = X5_01_HEIGHT,
                     "Weight, kg" = X5_02_WEIGHT,
                     "Procedure urgency" = X7_06_PROCEDURE_URGENCY,
                     
                     Proc_Date,
                     "UK TAVR CPM" = UK_TAVI_CPM_PR) %>%
              mutate("Surgical_Group" = "TAVR")) %>%
  mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                 "AVR+CABG" = "AVR_CABG",
                                                 "AVR+Other" = "AVR_Other",
                                                 "Isolated AVR" = "Isolated_AVR"),
                                      c("Isolated AVR", "AVR+CABG",
                                        "AVR+Other", "TAVR")))

#Overall table, by surgical group
TAVR_SAVR_TableOne <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                   names(TableOneData) == "Proc_Date")],
                                               data = TableOneData,
                                               factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                 sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                 ],
                                               strata = "Surgical_Group",
                                               addOverall = FALSE,
                                               test = FALSE)

#Baseline table by Pre-During-C19 group, stratified by surgical group
Isolated_SAVR_TableOne_Temporal <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                                names(TableOneData) == "Proc_Date")],
                                                            data = TableOneData %>%
                                                              filter(Surgical_Group == "Isolated AVR") %>%
                                                              filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                              mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                                     "During-Covid-19",
                                                                                                     "Pre-Covid-19"),
                                                                                              levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                            factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                              sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                            ],
                                                            strata = "COVID19Period",
                                                            addOverall = FALSE,
                                                            test = TRUE)

CABG_SAVR_TableOne_Temporal <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                            names(TableOneData) == "Proc_Date")],
                                                        data = TableOneData %>%
                                                          filter(Surgical_Group == "AVR+CABG") %>%
                                                          filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                          mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                                 "During-Covid-19",
                                                                                                 "Pre-Covid-19"),
                                                                                          levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                        factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                          sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                        ],
                                                        strata = "COVID19Period",
                                                        addOverall = FALSE,
                                                        test = TRUE)

Other_SAVR_TableOne_Temporal <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                             names(TableOneData) == "Proc_Date")],
                                                         data = TableOneData %>%
                                                           filter(Surgical_Group == "AVR+Other") %>%
                                                           filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                           mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                                  "During-Covid-19",
                                                                                                  "Pre-Covid-19"),
                                                                                           levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                         factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                           sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                         ],
                                                         strata = "COVID19Period",
                                                         addOverall = FALSE,
                                                         test = TRUE)

TAVR_TableOne_Temporal <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                       names(TableOneData) == "Proc_Date")],
                                                   data = TableOneData %>%
                                                     filter(Surgical_Group == "TAVR") %>%
                                                     filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                     mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                            "During-Covid-19",
                                                                                            "Pre-Covid-19"),
                                                                                     levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                   factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                     sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                   ],
                                                   strata = "COVID19Period",
                                                   addOverall = FALSE,
                                                   test = TRUE)


###-----------------------------------------------------------------------------------------------------------
# Figure: number of TAVI and SAVR procedures through time
###-----------------------------------------------------------------------------------------------------------

Figure_trends <- SAVR_Analysis_Cohort %>% 
  select(X_DATE_AND_TIME_OF_OPERATION, X_OPERATIVE_URGENCY, Surgical_Group) %>%
  transmute("Procedure_Urgency" = fct_collapse(X_OPERATIVE_URGENCY,
                                               "Elective" = c("1. Elective"),
                                               "Non-Elective" = c("2. Urgent",
                                                                  "3. Emergency",
                                                                  "4. Salvage")),
            Month = month(X_DATE_AND_TIME_OF_OPERATION),
            Year = year(X_DATE_AND_TIME_OF_OPERATION),
            Surgical_Group = Surgical_Group) %>%
  filter(!is.na(Procedure_Urgency)) %>%
  group_by(Month, Year, Procedure_Urgency, Surgical_Group) %>%
  count() %>%
  ungroup() %>%
  unite(Month_Year, Month, Year, sep = "/") %>%
  mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                               "month") - days(1)) %>%
  bind_rows(TAVI_Analysis_Cohort %>%
              select(X7_01_DATE_AND_TIME_OF_OPERATION, X7_06_PROCEDURE_URGENCY) %>%
              transmute("Procedure_Urgency" = fct_collapse(X7_06_PROCEDURE_URGENCY,
                                                           "Elective" = c("1. Elective"),
                                                           "Non-Elective" = c("2. Urgent",
                                                                              "3. Emergency",
                                                                              "4. Salvage")),
                        Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                        Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
              filter(!is.na(Procedure_Urgency)) %>%
              group_by(Month, Year, Procedure_Urgency) %>%
              count() %>%
              ungroup() %>%
              unite(Month_Year, Month, Year, sep = "/") %>%
              mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                                           "month") - days(1),
                     "Surgical_Group" = "TAVR")) %>%
  mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                 "AVR+CABG" = "AVR_CABG",
                                                 "AVR+Other" = "AVR_Other",
                                                 "Isolated AVR" = "Isolated_AVR"),
                                      c("Isolated AVR", "AVR+CABG",
                                        "AVR+Other", "TAVR"))) %>%
  ggplot(aes(x = Date, y = n, group = Procedure_Urgency, colour = Procedure_Urgency)) +
  geom_line() +
  facet_wrap(~Surgical_Group) +
  geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
  geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
  xlab("Date (month/Year)") + ylab("Number of Procedures per Month") +
  theme_bw(base_size = 12) +
  scale_color_discrete(name = "Procedure Urgency") +
  scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
               limits = as.Date(c('2017/01/01', '2020/12/31'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top")



###-----------------------------------------------------------------------------------------------------------
# Figure: number of TAVIs/SAVRs now vs. historic
###-----------------------------------------------------------------------------------------------------------

Figure_trends_annual_comparison <- ggpubr::ggarrange(
  SAVR_Analysis_Cohort %>% 
    select(X_DATE_AND_TIME_OF_OPERATION, Surgical_Group) %>%
    transmute(Month = month(X_DATE_AND_TIME_OF_OPERATION),
              Year = year(X_DATE_AND_TIME_OF_OPERATION),
              Surgical_Group = Surgical_Group) %>%
    group_by(Month, Year, Surgical_Group) %>%
    count() %>%
    ungroup() %>%
    mutate(Grouped_Years = factor(ifelse(Year == 2020, 
                                         "2020", 
                                         paste(min(Year),
                                               "2019", sep = "-")))) %>%
    group_by(Grouped_Years, Month, Surgical_Group) %>%
    summarise("Min_Count" = min(n),
              "Mean_Count" = mean(n),
              "Max_Count" = max(n),
              .groups = "drop") %>%
    bind_rows(TAVI_Analysis_Cohort %>%
                select(X7_01_DATE_AND_TIME_OF_OPERATION) %>%
                transmute(Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                          Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
                group_by(Month, Year) %>%
                count() %>%
                ungroup() %>%
                mutate(Grouped_Years = factor(ifelse(Year == 2020, 
                                                     "2020", 
                                                     paste(min(Year),
                                                           "2019", sep = "-")))) %>%
                group_by(Grouped_Years, Month) %>%
                summarise("Min_Count" = min(n),
                          "Mean_Count" = mean(n),
                          "Max_Count" = max(n),
                          .groups = "drop") %>%
                mutate("Surgical_Group" = "TAVR")) %>%
    mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                   "AVR+CABG" = "AVR_CABG",
                                                   "AVR+Other" = "AVR_Other",
                                                   "Isolated AVR" = "Isolated_AVR"),
                                        c("Isolated AVR", "AVR+CABG",
                                          "AVR+Other", "TAVR"))) %>%
    filter(Month <= MaxMonth) %>% #cap the time at the latest month in 2020 that was analysed
    ggplot(aes(x = Month, y = Mean_Count, group = Grouped_Years)) +
    facet_wrap(~Surgical_Group) +
    geom_line(aes(colour = Grouped_Years)) +
    geom_ribbon(aes(ymin = Min_Count, ymax = Max_Count, x = Month), alpha = 0.2) +
    scale_x_continuous(breaks = seq(from = 1, to = MaxMonth, by = 1)) +
    scale_color_discrete(name = "Time Period") +
    xlab("Month") + ylab("Procedures per Month (n)") +
    theme_bw(base_size = 12) +
    theme(legend.position="top")
  ,
  # percentage change in average monthly activity between historic years and 2020:
  SAVR_Analysis_Cohort %>% 
    filter(month(X_DATE_AND_TIME_OF_OPERATION) %in% c(1:MaxMonth)) %>% 
    select(X_DATE_AND_TIME_OF_OPERATION, Surgical_Group) %>%
    transmute(Month = month(X_DATE_AND_TIME_OF_OPERATION),
              Year = year(X_DATE_AND_TIME_OF_OPERATION),
              Surgical_Group = Surgical_Group) %>%
    group_by(Month, Year, Surgical_Group) %>%
    count() %>%
    ungroup() %>%
    mutate(Grouped_Years = factor(ifelse(Year == 2020, 
                                         "2020", 
                                         paste(min(Year),
                                               "2019", sep = "-")))) %>%
    group_by(Grouped_Years, Month, Surgical_Group) %>%
    summarise("Mean_Count" = mean(n),
              .groups = "drop") %>%
    pivot_wider(id_cols = c("Month", "Surgical_Group"),
                names_from = "Grouped_Years",
                values_from = "Mean_Count") %>%
    mutate("Difference" = `2017-2019` - `2020`,
           "Percentage_Change" = (Difference/`2017-2019`)*100) %>%
    bind_rows(TAVI_Analysis_Cohort %>%
                filter(month(X7_01_DATE_AND_TIME_OF_OPERATION) %in% c(1:MaxMonth)) %>% 
                select(X7_01_DATE_AND_TIME_OF_OPERATION) %>%
                transmute(Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                          Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
                group_by(Month, Year) %>%
                count() %>%
                ungroup() %>%
                mutate(Grouped_Years = factor(ifelse(Year == 2020, 
                                                     "2020", 
                                                     paste(min(Year),
                                                           "2019", sep = "-")))) %>%
                group_by(Grouped_Years, Month) %>%
                summarise("Mean_Count" = mean(n),
                          .groups = "drop") %>%
                mutate("Surgical_Group" = "TAVR") %>%
                pivot_wider(id_cols = c("Month", "Surgical_Group"),
                            names_from = "Grouped_Years",
                            values_from = "Mean_Count") %>%
                mutate("Difference" = `2017-2019` - `2020`,
                       "Percentage_Change" = (Difference/`2017-2019`)*100)) %>%
    mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                   "AVR+CABG" = "AVR_CABG",
                                                   "AVR+Other" = "AVR_Other",
                                                   "Isolated AVR" = "Isolated_AVR"),
                                        c("Isolated AVR", "AVR+CABG",
                                          "AVR+Other", "TAVR"))) %>%
    ggplot(aes(x = Month, y = Percentage_Change, group = Surgical_Group)) +
    facet_wrap(~Surgical_Group) +
    geom_line() +
    scale_x_continuous(breaks = seq(from = 1, to = MaxMonth, by = 1)) +
    scale_y_continuous(breaks = seq(from = -25, to = 100, by = 25)) +
    xlab("Month") + ylab("Percentage Change") +
    theme_bw(base_size = 12)
  ,
  ncol = 1,
  labels = c("A", "B")
)

Max_Percentage_Drop <- SAVR_Analysis_Cohort %>% 
  filter(month(X_DATE_AND_TIME_OF_OPERATION) %in% c(1:MaxMonth)) %>% 
  select(X_DATE_AND_TIME_OF_OPERATION, Surgical_Group) %>%
  transmute(Month = month(X_DATE_AND_TIME_OF_OPERATION),
            Year = year(X_DATE_AND_TIME_OF_OPERATION),
            Surgical_Group = Surgical_Group) %>%
  group_by(Month, Year, Surgical_Group) %>%
  count() %>%
  ungroup() %>%
  mutate(Grouped_Years = factor(ifelse(Year == 2020, 
                                       "2020", 
                                       paste(min(Year),
                                             "2019", sep = "-")))) %>%
  group_by(Grouped_Years, Month, Surgical_Group) %>%
  summarise("Mean_Count" = mean(n),
            .groups = "drop") %>%
  pivot_wider(id_cols = c("Month", "Surgical_Group"),
              names_from = "Grouped_Years",
              values_from = "Mean_Count") %>%
  mutate("Difference" = `2017-2019` - `2020`,
         "Percentage_Change" = (Difference/`2017-2019`)*100) %>%
  bind_rows(TAVI_Analysis_Cohort %>%
              filter(month(X7_01_DATE_AND_TIME_OF_OPERATION) %in% c(1:MaxMonth)) %>% 
              select(X7_01_DATE_AND_TIME_OF_OPERATION) %>%
              transmute(Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                        Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
              group_by(Month, Year) %>%
              count() %>%
              ungroup() %>%
              mutate(Grouped_Years = factor(ifelse(Year == 2020, 
                                                   "2020", 
                                                   paste(min(Year),
                                                         "2019", sep = "-")))) %>%
              group_by(Grouped_Years, Month) %>%
              summarise("Mean_Count" = mean(n),
                        .groups = "drop") %>%
              mutate("Surgical_Group" = "TAVR") %>%
              pivot_wider(id_cols = c("Month", "Surgical_Group"),
                          names_from = "Grouped_Years",
                          values_from = "Mean_Count") %>%
              mutate("Difference" = `2017-2019` - `2020`,
                     "Percentage_Change" = (Difference/`2017-2019`)*100)) %>%
  group_by(Surgical_Group) %>%
  summarise("Max_Drop" = max(Percentage_Change),
            .groups = "drop")




###-----------------------------------------------------------------------------------------------------------
# Figure: number of TAVI and SAVR procedures through time vs. predicted counts
###-----------------------------------------------------------------------------------------------------------

#Calculate difference
PredictedDrop <- NB_Count_Predictions %>%
  mutate("Difference" = n - Predicted_Counts,
         "Difference_Upper" = n - Lower,
         "Difference_Lower" = n - Upper) %>%
  unite(Month_Year, Month, Year, sep = "/") %>%
  mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                               "month") - days(1)) %>%
  mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                 "AVR+CABG" = "AVR_CABG",
                                                 "AVR+Other" = "AVR_Other",
                                                 "Isolated AVR" = "Isolated_AVR"),
                                      c("Isolated AVR", "AVR+CABG",
                                        "AVR+Other", "TAVR")),
         
         "Difference_CI" = paste(round(Difference), " (", 
                                 round(Difference_Lower), ", ", 
                                 round(Difference_Upper), ")", sep = ""))

Figure_trends_vs_Predicted_plot <- cowplot::plot_grid(
  ggplot(PredictedDrop) +
    facet_wrap(~Surgical_Group) +
    geom_line(aes(x = Date, y = n, color = "n")) +
    geom_line(aes(x = Date, y = Predicted_Counts, color = "Predicted_Counts")) +
    geom_ribbon(aes(x = Date, ymin = Lower, ymax = Upper), alpha = 0.2, colour = NA, fill = "red") +
    geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
    geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
    xlab("Date (month/Year)") + ylab("Number of Procedures \n per Month") +
    theme_bw(base_size = 12) +
    theme(legend.position="top") +
    scale_color_manual(name = "",
                       values = c("blue", "red"),
                       breaks = c("n", "Predicted_Counts"),
                       labels = c("Observed", "Predicted")) +
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
                 limits = as.Date(c('2017/01/01', '2020/12/31'))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ,
  ggplot(PredictedDrop, aes(x = Date, y = Difference)) +
    facet_wrap(~Surgical_Group) +
    geom_line() +
    geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
    geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
    xlab("Date (month/Year)") + ylab("Observed minus Expected \n procedures per Month") +
    theme_bw(base_size = 12) +
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
                 limits = as.Date(c('2017/01/01', '2020/12/31'))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ,
  ncol = 1,
  labels = c("A", "B")
)


###-----------------------------------------------------------------------------------------------------------
# Table: cumulative expected drop in cases
###-----------------------------------------------------------------------------------------------------------

Table_CumulativeDrop <- PredictedDrop %>%
  filter(month(Date) %in% c(3:MaxMonth), year(Date) == 2020) %>%
  group_by(Surgical_Group) %>%
  summarise("Cumulative_Diff" = sum(Difference),
            "Cumulative_Diff_Lower" = sum(Difference_Lower),
            "Cumulative_Diff_Upper" = sum(Difference_Upper),
            .groups = "drop") %>%
  ungroup() %>%
  bind_rows(
    summarise_all(., ~if(is.numeric(.)){sum(.)}else{"Total"})
  )


###-----------------------------------------------------------------------------------------------------------
# Figures: changes in mean risk through time
###-----------------------------------------------------------------------------------------------------------
Figure_Risk_Profiles <- SAVR_Analysis_Cohort %>%
  select(X_DATE_AND_TIME_OF_OPERATION, X_OPERATIVE_URGENCY, Surgical_Group, LES_PR) %>%
  transmute("Procedure_Urgency" = fct_collapse(X_OPERATIVE_URGENCY,
                                               "Elective" = c("1. Elective"),
                                               "Non-Elective" = c("2. Urgent",
                                                                  "3. Emergency",
                                                                  "4. Salvage")),
            Month = month(X_DATE_AND_TIME_OF_OPERATION),
            Year = year(X_DATE_AND_TIME_OF_OPERATION),
            Surgical_Group = Surgical_Group,
            LES_PR = LES_PR) %>%
  filter(!is.na(Procedure_Urgency)) %>%
  group_by(Month, Year, Procedure_Urgency, Surgical_Group) %>%
  summarise("MeanRisk" = mean(LES_PR)*100,
            .groups = "drop") %>%
  ungroup() %>%
  unite(Month_Year, Month, Year, sep = "/") %>%
  mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")),
                               "month") - days(1)) %>%
  bind_rows(TAVI_Analysis_Cohort %>%
              select(X7_01_DATE_AND_TIME_OF_OPERATION, X7_06_PROCEDURE_URGENCY, UK_TAVI_CPM_PR) %>%
              transmute("Procedure_Urgency" = fct_collapse(X7_06_PROCEDURE_URGENCY,
                                                           "Elective" = c("1. Elective"),
                                                           "Non-Elective" = c("2. Urgent",
                                                                              "3. Emergency",
                                                                              "4. Salvage")),
                        Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                        Year = year(X7_01_DATE_AND_TIME_OF_OPERATION),
                        UK_TAVI_CPM_PR = UK_TAVI_CPM_PR) %>%
              filter(!is.na(Procedure_Urgency)) %>%
              group_by(Month, Year, Procedure_Urgency) %>%
              summarise("MeanRisk" = mean(UK_TAVI_CPM_PR)*100,
                        .groups = "drop") %>%
              ungroup() %>%
              unite(Month_Year, Month, Year, sep = "/") %>%
              mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")),
                                           "month") - days(1),
                     "Surgical_Group" = "TAVR")) %>%
  mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                 "AVR+CABG" = "AVR_CABG",
                                                 "AVR+Other" = "AVR_Other",
                                                 "Isolated AVR" = "Isolated_AVR"),
                                      c("Isolated AVR", "AVR+CABG",
                                        "AVR+Other", "TAVR"))) %>%
  ggplot(aes(x = Date, y = MeanRisk, group = Procedure_Urgency, colour = Procedure_Urgency)) +
  geom_line() +
  geom_smooth(formula = 'y ~ x', method = "loess", linetype = "dashed") +
  facet_wrap(~Surgical_Group, scales = "free") +
  geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
  geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
  xlab("Date (month/Year)") + ylab("Mean Risk per Month") +
  theme_bw(base_size = 12) +
  scale_color_discrete(name = "Procedure Urgency") +
  scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
               limits = as.Date(c('2017/01/01', '2020/12/31'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top")



###-----------------------------------------------------------------------------------------------------------
# Figures: monthly procedural activity stratified by risk group
###-----------------------------------------------------------------------------------------------------------
#
Cut_offs <- as.numeric(quantile(TAVI_Analysis_Cohort$UK_TAVI_CPM_PR[which(
  year(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION) ==
    min(year(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION)) #take first year of data
)]))
Cut_offs[1] <- 0
Cut_offs[length(Cut_offs)] <- 1
Figure_TAVR_Activity_by_RiskGroup <- TAVI_Analysis_Cohort %>%
  mutate("RiskGroup" = cut(UK_TAVI_CPM_PR,
                           breaks = Cut_offs,
                           labels = paste("Risk Group", 1:(length(Cut_offs)-1), sep = " "))) %>%
  group_by("Year" = year(X7_01_DATE_AND_TIME_OF_OPERATION),
           "Month" = month(X7_01_DATE_AND_TIME_OF_OPERATION),
           RiskGroup) %>%
  count() %>%
  unite(Month_Year, Month, Year, sep = "/") %>%
  mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                               "month") - days(1)) %>%
  ggplot(aes(x = Date, y = n, group = RiskGroup)) +
  facet_wrap(~RiskGroup) +
  geom_line() +
  geom_smooth(formula = 'y ~ x', method = "loess", linetype = "dashed") +
  geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
  geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
  xlab("Date (month/Year)") + ylab("Monthly TAVR Cases") +
  theme_bw(base_size = 12) +
  scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
               limits = as.Date(c('2017/01/01', '2020/12/31'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


SAVR_RiskGroup.fnc <- function(Proc, ylab_name) {
  Subset_SAVR <- SAVR_Analysis_Cohort %>%
    filter(Surgical_Group == Proc)
  
  Cut_offs <- as.numeric(quantile(Subset_SAVR$LES_PR[which(
    year(Subset_SAVR$X_DATE_AND_TIME_OF_OPERATION) ==
      min(year(Subset_SAVR$X_DATE_AND_TIME_OF_OPERATION)) #take first year of data
  )]))
  Cut_offs[1] <- 0
  Cut_offs[length(Cut_offs)] <- 1
  
  Fig <- Subset_SAVR %>%
    mutate("RiskGroup" = cut(LES_PR,
                             breaks = Cut_offs,
                             labels = paste("Risk Group", 1:(length(Cut_offs)-1), sep = " "))) %>%
    group_by("Year" = year(X_DATE_AND_TIME_OF_OPERATION),
             "Month" = month(X_DATE_AND_TIME_OF_OPERATION),
             RiskGroup) %>%
    count() %>%
    unite(Month_Year, Month, Year, sep = "/") %>%
    mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                                 "month") - days(1)) %>%
    ggplot(aes(x = Date, y = n, group = RiskGroup)) +
    facet_wrap(~RiskGroup) +
    geom_line() +
    geom_smooth(formula = 'y ~ x', method = "loess", linetype = "dashed") +
    geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
    geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
    xlab("Date (month/Year)") + ylab(paste("Monthly ", ylab_name, " Cases", sep="")) +
    theme_bw(base_size = 12) +
    scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
                 limits = as.Date(c('2017/01/01', '2020/12/31'))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(Fig)
}
Figure_IsolatedAVR_Activity_by_RiskGroup <- SAVR_RiskGroup.fnc("Isolated_AVR", "Isolated AVR")
Figure_AVRCABG_Activity_by_RiskGroup <- SAVR_RiskGroup.fnc("AVR_CABG", "AVR+CABG")
Figure_AVROther_Activity_by_RiskGroup <- SAVR_RiskGroup.fnc("AVR_Other", "AVR+Other")


###-----------------------------------------------------------------------------------------------------------
# Figures: mortality 
###-----------------------------------------------------------------------------------------------------------

SurvTable <- summary(survfit(Surv(Time_To_Death, Mortality_Flag) ~ Surgical_Group, 
                                           data = SAVR_Analysis_Cohort %>%
                                             select(Surgical_Group, Time_To_Death, Mortality_Flag) %>%
                                             bind_rows(TAVI_Analysis_Cohort %>%
                                                         mutate(Surgical_Group = "TAVR") %>%
                                                         select(Surgical_Group, Time_To_Death, Mortality_Flag)) %>%
                                             mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                                                            "AVR+CABG" = "AVR_CABG",
                                                                                            "AVR+Other" = "AVR_Other",
                                                                                            "Isolated AVR" = "Isolated_AVR"),
                                                                                 c("Isolated AVR", "AVR+CABG",
                                                                                   "AVR+Other", "TAVR")))),
                     time = c(365/12, 365/2, 365, 365*2)) 

Overall_KM <- survminer::ggsurvplot(survfit(Surv(Time_To_Death, Mortality_Flag) ~ Surgical_Group, 
                                            data = SAVR_Analysis_Cohort %>%
                                              select(Surgical_Group, Time_To_Death, Mortality_Flag) %>%
                                              bind_rows(TAVI_Analysis_Cohort %>%
                                                          mutate(Surgical_Group = "TAVR") %>%
                                                          select(Surgical_Group, Time_To_Death, Mortality_Flag)) %>%
                                              mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                                                             "AVR+CABG" = "AVR_CABG",
                                                                                             "AVR+Other" = "AVR_Other",
                                                                                             "Isolated AVR" = "Isolated_AVR"),
                                                                                  c("Isolated AVR", "AVR+CABG",
                                                                                    "AVR+Other", "TAVR")))),
                                    conf.int = TRUE,
                                    censor = FALSE,
                                    risk.table = TRUE, risk.table.y.text.col = TRUE,
                                    ggtheme = theme_bw(),
                                    xlab = "Time (days)",
                                    legend.title = "",
                                    legend.labs = c("Isolated AVR", "AVR+CABG",
                                                    "AVR+Other", "TAVR"))





###-----------------------------------------------------------------------------------------------------------
# Table: hazard ratio of mortality by C19 period (pre-/during-) for each TAVR outcome/complication
###-----------------------------------------------------------------------------------------------------------

Table_30D_Mortality_HR <- SAVR_Mortality_ModelOutput %>%
  filter(str_detect(term, pattern = "COVID19PeriodDuringC19")) %>%
  extract(term, into = c("Surgical_Group", "term"), "([A-Za-z\\_]+):([A-Za-z0-9]+)") %>%
  mutate(Surgical_Group = str_remove(Surgical_Group, pattern = "Surgical_Group"),
         
         estimate = paste(round(estimate, 2), " (", round(conf.low, 2),
                          ", ", round(conf.high, 2), ")", sep = ""),
         
         p.value = as.character(ifelse(p.value < 0.001, "p<0.001", round(p.value, 3)))) %>%
  rename("Hazard Ratio (95% CI)" = estimate,
         "p-value" = p.value) %>%
  select(Surgical_Group, "Hazard Ratio (95% CI)", "p-value") %>%
  bind_rows(TAVR_Mortality_ModelOutput %>%
              filter(term == "COVID19PeriodDuringC19") %>%
              mutate(estimate = paste(round(estimate, 2), " (", round(conf.low, 2),
                                      ", ", round(conf.high, 2), ")", sep = ""),
                     
                     p.value = as.character(ifelse(p.value < 0.001, "p<0.001", round(p.value, 3))),
                     
                     "Surgical_Group" = "TAVR") %>%
              rename("Hazard Ratio (95% CI)" = estimate,
                     "p-value" = p.value) %>%
              select(Surgical_Group, "Hazard Ratio (95% CI)", "p-value")) %>%
  mutate(Surgical_Group = ifelse(Surgical_Group == "Isolated_AVR", "Isolated AVR",
                                 str_replace(Surgical_Group, pattern = "_", replacement = "+"))) %>%
  rename("Surgical Group" = "Surgical_Group")



Table_LOS_HR <- SAVR_LOS_ModelOutput %>%
  filter(str_detect(term, pattern = "COVID19PeriodDuringC19")) %>%
  extract(term, into = c("Surgical_Group", "term"), "([A-Za-z\\_]+):([A-Za-z0-9]+)") %>%
  mutate(estimate = paste(round(estimate, 2), " (", round(conf.low, 2),
                          ", ", round(conf.high, 2), ")", sep = ""),
         
         p.value = as.character(ifelse(p.value < 0.001, "p<0.001", round(p.value, 3)))) %>%
  rename("Hazard Ratio (95% CI)" = estimate,
         "p-value" = p.value) %>%
  select(term, Surgical_Group, "Hazard Ratio (95% CI)", "p-value") %>%
  bind_rows(TAVR_LOS_ModelOutput %>%
              filter(str_detect(term, pattern = "COVID19PeriodDuringC19")) %>%
              mutate(estimate = paste(round(estimate, 2), " (", round(conf.low, 2),
                                      ", ", round(conf.high, 2), ")", sep = ""),
                     
                     p.value = as.character(ifelse(p.value < 0.001, "p<0.001", round(p.value, 3))),
                     
                     "Surgical_Group" = "TAVR") %>%
              rename("Hazard Ratio (95% CI)" = estimate,
                     "p-value" = p.value) %>%
              select(term, Surgical_Group, "Hazard Ratio (95% CI)", "p-value"))  %>%
  mutate(Surgical_Group = ifelse(Surgical_Group == "Isolated_AVR", "Isolated AVR",
                                 str_replace(Surgical_Group, pattern = "_", replacement = "+"))) %>%
  rename("Surgical Group" = "Surgical_Group")





###-----------------------------------------------------------------------------------------------------------
# Save all aggregate-level outputs required for manuscript
###-----------------------------------------------------------------------------------------------------------

write_rds(list(
  "TAVI_Raw_SS" = nrow(TAVI_data),
  "SAVR_Raw_SS" = nrow(ACS_data),
  "TAVI_Included_SS" = nrow(TAVI_Analysis_Cohort),
  "SAVR_Included_SS" = nrow(SAVR_Analysis_Cohort),
  "IsolatedAVR" = sum(SAVR_Analysis_Cohort$Surgical_Group == "Isolated_AVR"),
  "AVR_CABG" = sum(SAVR_Analysis_Cohort$Surgical_Group == "AVR_CABG"),
  "AVR_Other" = sum(SAVR_Analysis_Cohort$Surgical_Group == "AVR_Other"),
  
  "TAVI_First_Month" = month.name[min(month(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION[which(
    year(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION) == 
      min(year(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION)))]))],
  "SAVR_First_Month" = month.name[min(month(SAVR_Analysis_Cohort$X_DATE_AND_TIME_OF_OPERATION[which(
    year(SAVR_Analysis_Cohort$X_DATE_AND_TIME_OF_OPERATION) == 
      min(year(SAVR_Analysis_Cohort$X_DATE_AND_TIME_OF_OPERATION)))]))],
  "TAVI_First_Year" = min(year(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION)),
  "SAVR_First_Year" = min(year(SAVR_Analysis_Cohort$X_DATE_AND_TIME_OF_OPERATION)),
  "TAVI_Last_Month" = month.name[MaxMonth],
  "SAVR_Last_Month" = month.name[MaxMonth],
  "TAVI_Last_Year" = max(year(TAVI_Analysis_Cohort$X7_01_DATE_AND_TIME_OF_OPERATION)),
  "SAVR_Last_Year" = max(year(SAVR_Analysis_Cohort$X_DATE_AND_TIME_OF_OPERATION)),
  
  "TAVR_SAVR_TableOne" = TAVR_SAVR_TableOne,
  "Isolated_SAVR_TableOne_Temporal" = Isolated_SAVR_TableOne_Temporal,
  "CABG_SAVR_TableOne_Temporal" = CABG_SAVR_TableOne_Temporal,
  "Other_SAVR_TableOne_Temporal" = Other_SAVR_TableOne_Temporal,
  "TAVR_TableOne_Temporal" = TAVR_TableOne_Temporal,
  
  "Figure_trends" = Figure_trends,
  "Figure_Risk_Profiles" = Figure_Risk_Profiles,
  "Figure_TAVR_Activity_by_RiskGroup" = Figure_TAVR_Activity_by_RiskGroup,
  "Figure_IsolatedAVR_Activity_by_RiskGroup" = Figure_IsolatedAVR_Activity_by_RiskGroup,
  "Figure_AVRCABG_Activity_by_RiskGroup" = Figure_AVRCABG_Activity_by_RiskGroup,
  "Figure_AVROther_Activity_by_RiskGroup" = Figure_AVROther_Activity_by_RiskGroup,
  "Figure_trends_annual_comparison" = Figure_trends_annual_comparison,
  "Max_Percentage_Drop" = Max_Percentage_Drop,
  "Figure_trends_vs_Predicted_plot" = Figure_trends_vs_Predicted_plot,
  "PredictedDrop" = PredictedDrop,
  "Table_CumulativeDrop" = Table_CumulativeDrop,
  "Overall_KM" = Overall_KM,
  "SurvTable" = SurvTable,
  "Table_30D_Mortality_HR" = Table_30D_Mortality_HR,
  "Table_LOS_HR" = Table_LOS_HR
  ),
  path = "../Effect of C19 on TAVI/AggregateOutputs/AggregateOutputs.rds")


# #### Save high-quality versions of relevant figures:
ggsave(filename = here::here("AggregateOutputs", "Fig1_Trends.tiff"),
       plot = Figure_trends,
       dpi = 300,
       height = 6,
       width = 6,
       units = "in")

ggsave(filename = here::here("AggregateOutputs", "Fig2_AnnualComparison.tiff"),
       plot = Figure_trends_annual_comparison,
       dpi = 300,
       height = 7,
       width = 5,
       units = "in")

ggsave(filename = here::here("AggregateOutputs", "Fig3_ObsExp.tiff"),
       plot = Figure_trends_vs_Predicted_plot,
       dpi = 300,
       height = 7,
       width = 5,
       units = "in")


###-----------------------------------------------------------------------------------------------------------
# Sensitivity analysis: look at only the centres that reported data in last month of analysis - these
# aim to capture the "rapidly submitting centres" - i.e. those that are continuing to submit data
# rapidly
###-----------------------------------------------------------------------------------------------------------

TAVR_SubSetCentres <- TAVI_Analysis_Cohort %>%
  filter(year(X7_01_DATE_AND_TIME_OF_OPERATION) == 2020) %>%
  transmute(Month = factor(month(X7_01_DATE_AND_TIME_OF_OPERATION), levels = 1:MaxMonth),
            X1_01_HOSPITAL_IDENTIFIER = X1_01_HOSPITAL_IDENTIFIER) %>%
  table() %>% #cross-tabulate month by centre
  as_tibble() %>% 
  filter(Month == MaxMonth) %>% #for last month, which centres submitted data?
  filter(n > 0) %>%
  pull(X1_01_HOSPITAL_IDENTIFIER)

AVR_SubSetCentres <- SAVR_Analysis_Cohort %>%
  filter(year(X_DATE_AND_TIME_OF_OPERATION) == 2020) %>%
  transmute(Month = factor(month(X_DATE_AND_TIME_OF_OPERATION), levels = 1:MaxMonth),
            X_HOSPITAL_IDENTIFIER = X_HOSPITAL_IDENTIFIER) %>%
  table() %>% #cross-tabulate month by centre
  as_tibble() %>% 
  filter(Month == MaxMonth) %>% #for last month, which centres submitted data?
  filter(n > 0) %>%
  pull(X_HOSPITAL_IDENTIFIER)



Figure_trends_Sensitivity_Analysis <- SAVR_Analysis_Cohort %>% 
  filter(X_HOSPITAL_IDENTIFIER %in% AVR_SubSetCentres) %>%
  select(X_DATE_AND_TIME_OF_OPERATION, X_OPERATIVE_URGENCY, Surgical_Group) %>%
  transmute("Procedure_Urgency" = fct_collapse(X_OPERATIVE_URGENCY,
                                               "Elective" = c("1. Elective"),
                                               "Non-Elective" = c("2. Urgent",
                                                                  "3. Emergency",
                                                                  "4. Salvage")),
            Month = month(X_DATE_AND_TIME_OF_OPERATION),
            Year = year(X_DATE_AND_TIME_OF_OPERATION),
            Surgical_Group = Surgical_Group) %>%
  filter(!is.na(Procedure_Urgency)) %>%
  group_by(Month, Year, Procedure_Urgency, Surgical_Group) %>%
  count() %>%
  ungroup() %>%
  unite(Month_Year, Month, Year, sep = "/") %>%
  mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                               "month") - days(1)) %>%
  bind_rows(TAVI_Analysis_Cohort %>%
              filter(X1_01_HOSPITAL_IDENTIFIER %in% TAVR_SubSetCentres) %>%
              select(X7_01_DATE_AND_TIME_OF_OPERATION, X7_06_PROCEDURE_URGENCY) %>%
              transmute("Procedure_Urgency" = fct_collapse(X7_06_PROCEDURE_URGENCY,
                                                           "Elective" = c("1. Elective"),
                                                           "Non-Elective" = c("2. Urgent",
                                                                              "3. Emergency",
                                                                              "4. Salvage")),
                        Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                        Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
              filter(!is.na(Procedure_Urgency)) %>%
              group_by(Month, Year, Procedure_Urgency) %>%
              count() %>%
              ungroup() %>%
              unite(Month_Year, Month, Year, sep = "/") %>%
              mutate("Date" = ceiling_date(dmy(paste("01/", Month_Year, sep="")), 
                                           "month") - days(1),
                     "Surgical_Group" = "TAVR")) %>%
  mutate(Surgical_Group = fct_relevel(fct_recode(factor(Surgical_Group),
                                                 "AVR+CABG" = "AVR_CABG",
                                                 "AVR+Other" = "AVR_Other",
                                                 "Isolated AVR" = "Isolated_AVR"),
                                      c("Isolated AVR", "AVR+CABG",
                                        "AVR+Other", "TAVR"))) %>%
  ggplot(aes(x = Date, y = n, group = Procedure_Urgency, colour = Procedure_Urgency)) +
  geom_line() +
  facet_wrap(~Surgical_Group) +
  geom_vline(xintercept = dmy("01/03/2020"), linetype = "dashed") +
  geom_vline(xintercept = dmy("23/03/2020"), linetype = "dotted") +
  xlab("Date (month/Year)") + ylab("Number of Procedures per Month") +
  theme_bw(base_size = 12) +
  scale_color_discrete(name = "Procedure Urgency") +
  scale_x_date(date_breaks = "3 month", date_labels = "%m/%Y",
               limits = as.Date(c('2017/01/01', '2020/12/31'))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top")



###-----------------------------------------------------------------------------------------------------------
# Sensitivity analysis: baseline tables with pre-covid group only being from 2019
###-----------------------------------------------------------------------------------------------------------
#Baseline table by Pre-During-C19 group, stratified by surgical group
Isolated_SAVR_TableOne_Sensitivity <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                                   names(TableOneData) == "Proc_Date")],
                                                               data = TableOneData %>%
                                                                 filter(Surgical_Group == "Isolated AVR") %>%
                                                                 filter(year(Proc_Date) == 2019 | year(Proc_Date) == 2020) %>%
                                                                 filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                                 mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                                        "During-Covid-19",
                                                                                                        "Pre-Covid-19"),
                                                                                                 levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                               factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                                 sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                               ],
                                                               strata = "COVID19Period",
                                                               addOverall = FALSE,
                                                               test = TRUE)

CABG_SAVR_TableOne_Sensitivity <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                               names(TableOneData) == "Proc_Date")],
                                                           data = TableOneData %>%
                                                             filter(Surgical_Group == "AVR+CABG") %>%
                                                             filter(year(Proc_Date) == 2019 | year(Proc_Date) == 2020) %>%
                                                             filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                             mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                                    "During-Covid-19",
                                                                                                    "Pre-Covid-19"),
                                                                                             levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                           factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                             sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                           ],
                                                           strata = "COVID19Period",
                                                           addOverall = FALSE,
                                                           test = TRUE)

Other_SAVR_TableOne_Sensitivity <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                                names(TableOneData) == "Proc_Date")],
                                                            data = TableOneData %>%
                                                              filter(Surgical_Group == "AVR+Other") %>%
                                                              filter(year(Proc_Date) == 2019 | year(Proc_Date) == 2020) %>%
                                                              filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                              mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                                     "During-Covid-19",
                                                                                                     "Pre-Covid-19"),
                                                                                              levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                            factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                              sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                            ],
                                                            strata = "COVID19Period",
                                                            addOverall = FALSE,
                                                            test = TRUE)

TAVR_TableOne_Sensitivity <- tableone::CreateTableOne(vars = names(TableOneData)[-which(names(TableOneData) == "Surgical_Group" |
                                                                                          names(TableOneData) == "Proc_Date")],
                                                      data = TableOneData %>%
                                                        filter(Surgical_Group == "TAVR") %>%
                                                        filter(year(Proc_Date) == 2019 | year(Proc_Date) == 2020) %>%
                                                        filter(month(Proc_Date) %in% c(2:MaxMonth)) %>% #filter months Feb-MaxMonth each year
                                                        mutate("COVID19Period" = factor(ifelse(year(Proc_Date) == 2020,
                                                                                               "During-Covid-19",
                                                                                               "Pre-Covid-19"),
                                                                                        levels = c("Pre-Covid-19", "During-Covid-19"))),
                                                      factorVars = names(TableOneData %>% select(-Surgical_Group, -Proc_Date))[ 
                                                        sapply(TableOneData %>% select(-Surgical_Group, -Proc_Date), is.factor) 
                                                      ],
                                                      strata = "COVID19Period",
                                                      addOverall = FALSE,
                                                      test = TRUE)

###-----------------------------------------------------------------------------------------------------------
# Sensitivity analysis: outcome models with pre-covid group only being from 2019
###-----------------------------------------------------------------------------------------------------------
summary(coxph(Surv(Time_To_Death, Mortality_Flag) ~ strata(Surgical_Group):COVID19Period + 
                strata(Surgical_Group):LES_LP + strata(Surgical_Group),
              data = SAVR_Analysis_Cohort %>%
                filter(year(X_DATE_AND_TIME_OF_OPERATION) == 2019 | year(X_DATE_AND_TIME_OF_OPERATION) == 2020) %>%
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
                       Time_To_Death = ifelse(Time_To_Death > 30, 30, Time_To_Death))))


summary(coxph(Surv(Time_To_Death, Mortality_Flag) ~ COVID19Period + TAVR_CPM_LP,
              data = TAVI_Analysis_Cohort %>%
                filter(year(X7_01_DATE_AND_TIME_OF_OPERATION) == 2019 | year(X7_01_DATE_AND_TIME_OF_OPERATION) == 2020) %>%
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
                       Time_To_Death = ifelse(Time_To_Death > 30, 30, Time_To_Death))))





###-----------------------------------------------------------------------------------------------------------
# Save results from sensitivity analysis
###-----------------------------------------------------------------------------------------------------------
write_rds(list(
  "Figure_trends_Sensitivity_Analysis" = Figure_trends_Sensitivity_Analysis,
  "Isolated_SAVR_TableOne_Sensitivity" = Isolated_SAVR_TableOne_Sensitivity, 
  "CABG_SAVR_TableOne_Sensitivity" = CABG_SAVR_TableOne_Sensitivity, 
  "Other_SAVR_TableOne_Sensitivity" = Other_SAVR_TableOne_Sensitivity, 
  "TAVR_TableOne_Sensitivity" = TAVR_TableOne_Sensitivity
),
path = "../Effect of C19 on TAVI/AggregateOutputs/SensitivityAnalysis.rds")


ggsave(filename = here::here("AggregateOutputs", "Fig1B_Trends_Sensitivity.tiff"),
       plot = Figure_trends_Sensitivity_Analysis,
       dpi = 300,
       height = 6,
       width = 6,
       units = "in")



