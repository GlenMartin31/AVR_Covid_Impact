library(tidyverse)
library(lubridate)

###-----------------------------------------------------------------------------------------------------------
# Establish the connection to database
###-----------------------------------------------------------------------------------------------------------
con <- odbc::dbConnect(odbc::odbc(), dsn = "databricks")
con@quote = ''

#Load the different files for ACS data:
ACS_dataV4 <- DBI::dbGetQuery(con, "SELECT * FROM nicor_clear.acsv4")
ACS_dataV5 <- DBI::dbGetQuery(con, "SELECT * FROM nicor_clear.acsv5")
ACS_dataV51 <- DBI::dbGetQuery(con, "SELECT * FROM nicor_clear.acsv5_1")

#Stack all the ACS datasets (any duplicates will be removed at end of this script):
ACS_data <- bind_rows(ACS_dataV4,
                      ACS_dataV5,
                      ACS_dataV51)
#Clear each individual dataset to save R memory:
rm(ACS_dataV4, ACS_dataV5, ACS_dataV51)

#Load the ONS mortality data:
Mortality_data <- DBI::dbGetQuery(con, "SELECT * FROM nicor_clear.dars_bird_deaths")

###-----------------------------------------------------------------------------------------------------------
# Change structure of the dataset
###-----------------------------------------------------------------------------------------------------------
ACS_data <- ACS_data %>%
  rename_all(function(x) paste0("X_", x)) %>%
  #convert all numeric variables into integer:
  mutate_at(.vars = c("X_ACTUAL_CREATININE_AT_SURGERY",
                      "X_PA_SYSTOLIC",
                      "X_SEVERITY_OF_AVS_EOA_IN_CM2",
                      "X_SEVERITY_OF_AVS_GRADIENT_MMHG",
                      "X_LEFT_VENTRICULAR_FUNCTION",
                      "X_HEIGHT",
                      "X_WEIGHT",
                      "X_TOTAL_DISTAL_CORONARY_ANASTAM",
                      "X_VALVES_REPLACED_REPAIRED",
                      "X_AORTIC_VALVE_OR_RING_SIZE_MM",
                      "X_MITRAL_VALVE_SIZE",
                      "X_TRICUSPID_VALVE_SIZE",
                      "X_PULMONARY_VALVE_SIZE",
                      "X_NUMBER_AORTA_SEGMENTS_OPERATED",
                      "X_CUMULATIVE_BYPASS_TIME",
                      "X_CUMULATIVE_CROSS_CLAMP_TIME",
                      "X_TOTAL_CIRCULATORY_ARREST_TIME",
                      "X_AGE_AT_OP"),
            as.numeric) %>%
  #turn all other variables into factors
  mutate_if(is.character, as.factor) %>%
  as_tibble()


###-----------------------------------------------------------------------------------------------------------
# Correct continuous variables
###-----------------------------------------------------------------------------------------------------------

ACS_data$X_ACTUAL_CREATININE_AT_SURGERY[
  which(ACS_data$X_ACTUAL_CREATININE_AT_SURGERY > 5000 |
          ACS_data$X_ACTUAL_CREATININE_AT_SURGERY <= 0)] <- NA

ACS_data$X_PA_SYSTOLIC[which(ACS_data$X_PA_SYSTOLIC < 0)] <- NA

ACS_data$X_SEVERITY_OF_AVS_EOA_IN_CM2[which(ACS_data$X_SEVERITY_OF_AVS_EOA_IN_CM2 > 5)] <- NA

ACS_data$X_SEVERITY_OF_AVS_GRADIENT_MMHG[
  which(ACS_data$X_SEVERITY_OF_AVS_GRADIENT_MMHG > 250)] <- NA


ACS_data$X_LEFT_VENTRICULAR_FUNCTION[
  which(ACS_data$X_LEFT_VENTRICULAR_FUNCTION > 100 |
          ACS_data$X_LEFT_VENTRICULAR_FUNCTION < 0)] <- NA

###Height
#Convert units for height from cm into m:
ACS_data$X_HEIGHT[
  which(ACS_data$X_HEIGHT > 100)] <- ACS_data$X_HEIGHT[
    which(ACS_data$X_HEIGHT > 100)] / 100
#convert any from inches to m:
ACS_data$X_HEIGHT[
  which(ACS_data$X_HEIGHT > 10 &
          ACS_data$X_HEIGHT < 150)] <- ACS_data$X_HEIGHT[
            which(ACS_data$X_HEIGHT > 10 &
                    ACS_data$X_HEIGHT < 150)] / 39.37
#Define a reasonable range for height in m:
ACS_data$X_HEIGHT[which(ACS_data$X_HEIGHT > 5)] <- NA
ACS_data$X_HEIGHT[which(ACS_data$X_HEIGHT < 1)] <- NA


###Weight
ACS_data$X_WEIGHT[
  which(ACS_data$X_WEIGHT > 250 |
          ACS_data$X_WEIGHT < 10)] <- NA 


#Total number of distal coronary anastamoses - ensure a whole integer (round up if needed)
ACS_data$X_TOTAL_DISTAL_CORONARY_ANASTAM <- ceiling(ACS_data$X_TOTAL_DISTAL_CORONARY_ANASTAM)

#Total number of valved replaced - ensure a whole integer (round up if needed)
ACS_data$X_VALVES_REPLACED_REPAIRED <- ceiling(ACS_data$X_VALVES_REPLACED_REPAIRED)

#Aortic valve size - ensure positive:
ACS_data$X_AORTIC_VALVE_OR_RING_SIZE_MM[
  which(ACS_data$X_AORTIC_VALVE_OR_RING_SIZE_MM <= 0 |
          ACS_data$X_AORTIC_VALVE_OR_RING_SIZE_MM > 500)] <- NA

#Mitral valve size - ensure positive:
ACS_data$X_MITRAL_VALVE_SIZE[which(ACS_data$X_MITRAL_VALVE_SIZE <= 0)] <- NA
ACS_data$X_MITRAL_VALVE_SIZE[which(ACS_data$X_MITRAL_VALVE_SIZE>90)] <- ACS_data$X_MITRAL_VALVE_SIZE[which(ACS_data$X_MITRAL_VALVE_SIZE>90)] /100

#Tricuspid valve size - ensure positive:
ACS_data$X_TRICUSPID_VALVE_SIZE[which(ACS_data$X_TRICUSPID_VALVE_SIZE <= 0 |
                                        ACS_data$X_TRICUSPID_VALVE_SIZE > 80)] <- NA

#Pulmonary valve size - ensure positive:
ACS_data$X_PULMONARY_VALVE_SIZE[which(ACS_data$X_PULMONARY_VALVE_SIZE <= 0)] <- NA

#Number of aorta segments - ensure a whole integer (round up if needed) and positive:
ACS_data$X_NUMBER_AORTA_SEGMENTS_OPERATED <- ceiling(ACS_data$X_NUMBER_AORTA_SEGMENTS_OPERATED)
ACS_data$X_NUMBER_AORTA_SEGMENTS_OPERATED[which(ACS_data$X_NUMBER_AORTA_SEGMENTS_OPERATED <= 0)] <- NA


#Bypass/ clamp and arrest time - ensure positive:
ACS_data$X_CUMULATIVE_BYPASS_TIME[which(ACS_data$X_CUMULATIVE_BYPASS_TIME <= 0 |
                                          ACS_data$X_CUMULATIVE_BYPASS_TIME >6000)] <- NA
ACS_data$X_CUMULATIVE_CROSS_CLAMP_TIME[which(ACS_data$X_CUMULATIVE_CROSS_CLAMP_TIME <= 0)] <- NA
ACS_data$X_TOTAL_CIRCULATORY_ARREST_TIME[which(ACS_data$X_TOTAL_CIRCULATORY_ARREST_TIME <= 0)] <- NA


#Age - ensure positive
ACS_data$X_AGE_AT_OP[which(ACS_data$X_AGE_AT_OP <= 0)] <- NA


###-----------------------------------------------------------------------------------------------------------
# Change/ clean factor levels, where needed
###-----------------------------------------------------------------------------------------------------------

ACS_data <- ACS_data %>%
  mutate_if(is.factor,
            ~fct_recode(.,
                        NULL = "",
                        NULL = "0. Not known",
                        NULL = "8. Not measured",
                        NULL = "9. Unknown",
                        NULL = "9. Not known",
                        NULL = "9. Not investigated",
                        NULL = "10. Unknown")) %>%
  #Group some factor levels where low counts:
  mutate(X_PREVIOUS_CARDIAC_SURGERY = factor(ifelse(X_PREVIOUS_CARDIAC_SURGERY == "0. No previous cardiac surgery" |
                                                      X_PREVIOUS_CARDIAC_SURGERY == "0. No previous surgery",
                                                    "0. No",
                                                    "1. Yes"),
                                             levels = c("0. No", "1. Yes")),
         
         X_CIGARETTE_SMOKING_HISTORY = fct_collapse(X_CIGARETTE_SMOKING_HISTORY,
                                                    "1. Current/Ex Smoker" = c("1. Ex smoker",
                                                                               "2. Current smoker")),
         
         X_RENAL_FUNCTION_DIALYSIS = fct_recode(X_RENAL_FUNCTION_DIALYSIS,
                                                "3. No dialysis but Pre-operative acute renal failure (anuria or oliguria < 10ml/hour)" =
                                                  "3. No dialysis but Pre-operative acute remnal failure (anuria or oliguria < 10ml/hour)"),
         
         X_HISTORY_OF_PULMONARY_DISEASE = fct_collapse(X_HISTORY_OF_PULMONARY_DISEASE,
                                                       "0. No chronic pulmonary disease" = c("0. No chronic pulmonary disease ",
                                                                                             "0. No pulmonary disease "),
                                                       "1. COAD/emphysema or Asthma" = c("1. COAD/emphysema or Asthma",
                                                                                         "1. Chronic pulmonary disease requiring use of long-term medication ")),
         
         X_HISTORY_OF_NEUROLOGICAL_DIS = factor(ifelse(str_detect(as.character(X_HISTORY_OF_NEUROLOGICAL_DIS),
                                                                  pattern = "3. CVA with residual deficit") == TRUE,
                                                       "3. CVA with residual deficit",
                                                       ifelse(str_detect(as.character(X_HISTORY_OF_NEUROLOGICAL_DIS),
                                                                         pattern = "2. CVA with full recovery") == TRUE,
                                                              "2. CVA with full recovery",
                                                              ifelse(str_detect(as.character(X_HISTORY_OF_NEUROLOGICAL_DIS),
                                                                                pattern = "1. TIA") == TRUE,
                                                                     "1. TIA",
                                                                     ifelse(str_detect(as.character(X_HISTORY_OF_NEUROLOGICAL_DIS),
                                                                                       pattern = "0. No history of neurological disease") == TRUE,
                                                                            "0. No history of neurological disease",
                                                                            NA)))),
                                                levels = c("0. No history of neurological disease",
                                                           "1. TIA",
                                                           "2. CVA with full recovery",
                                                           "3. CVA with residual deficit")),
         
         X_LEFT_MAIN_STEM_DISEASE = fct_recode(X_LEFT_MAIN_STEM_DISEASE,
                                               "1. LMS >50% diameter stenosis" = "1. LMS >50% diameter stenosis."),
         
         X_EJECTION_FRACTION_CATEGORY = fct_recode(X_EJECTION_FRACTION_CATEGORY,
                                                   "2. Moderate (LVEF 31 - 50%)" = "2. Fair (LVEF 30 - 50%)",
                                                   "3. Poor (LVEF < 30%)" = "3. Poor (LVEF 21 - 30%)",
                                                   "3. Poor (LVEF < 30%)" = "4. Very Poor (LVEF <21%)")
  ) %>%
  #Separate the pre-op heart rhythm variable into each component indicator variable (i.e.
  #rather than Sinus & AF, we create two new columns: one for sinus (yes/no) and one for AF
  mutate(X_Atrial_Fib_Flutter = factor(ifelse(str_detect(as.character(X_PRE_OPERATIVE_HEART_RHYTHM),
                                                               pattern = "1. Atrial fibrillation/flutter") ==
                                                      TRUE,
                                                    "1. Yes", "0. No"),
                                             levels = c("0. No", "1. Yes")),
         
         X_Sinus = factor(ifelse(str_detect(as.character(X_PRE_OPERATIVE_HEART_RHYTHM),
                                                  pattern = "0. Sinus rhythm") == TRUE,
                                       "1. Yes", "0. No"),
                                levels = c("0. No", "1. Yes")),
         .after = "X_PRE_OPERATIVE_HEART_RHYTHM")


#Variable 2.28 should match the category given in variable 2.28
ACS_data$X_LEFT_VENTRICULAR_FUNCTION[
  which(ACS_data$X_EJECTION_FRACTION_CATEGORY == "1. Good (LVEF > 50%)" &
          ACS_data$X_LEFT_VENTRICULAR_FUNCTION <= 50)] <- NA
ACS_data$X_LEFT_VENTRICULAR_FUNCTION[
  which(ACS_data$X_EJECTION_FRACTION_CATEGORY == "2. Moderate (LVEF 31 - 50%)" &
          (ACS_data$X_LEFT_VENTRICULAR_FUNCTION >= 50 |
             ACS_data$X_LEFT_VENTRICULAR_FUNCTION <= 30))] <- NA
ACS_data$X_LEFT_VENTRICULAR_FUNCTION[
  which(ACS_data$X_EJECTION_FRACTION_CATEGORY == "3. Poor (LVEF < 30%)" &
          ACS_data$X_LEFT_VENTRICULAR_FUNCTION >= 30)] <- NA

# ACS_data %>%
#   group_by(X_EJECTION_FRACTION_CATEGORY) %>%
#   summarise(min(X_LEFT_VENTRICULAR_FUNCTION, na.rm = TRUE),
#             max(X_LEFT_VENTRICULAR_FUNCTION, na.rm = TRUE))


###-----------------------------------------------------------------------------------------------------------
# Format date/time variables
###-----------------------------------------------------------------------------------------------------------

ACS_data$X_DATE_OF_LAST_CARDIAC_OPERATION <- dmy(
  as.character(ACS_data$X_DATE_OF_LAST_CARDIAC_OPERATION)
)

ACS_data$X_DATE_OF_LAST_CATHETERISATION <- dmy(
  as.character(ACS_data$X_DATE_OF_LAST_CATHETERISATION)
)

ACS_data$X_ADMISSION_DATE <- dmy(
  as.character(ACS_data$X_ADMISSION_DATE)
)

ACS_data$X_DATE_AND_TIME_OF_OPERATION <- dmy_hm(
  as.character(ACS_data$X_DATE_AND_TIME_OF_OPERATION)
)

ACS_data$X_DATE_DISCHARGE_OR_HOSP_DEATH <- dmy(
  as.character(ACS_data$X_DATE_DISCHARGE_OR_HOSP_DEATH)
)

###-----------------------------------------------------------------------------------------------------------
# Select variables
###-----------------------------------------------------------------------------------------------------------

ACS_data <- ACS_data %>%
  select(X_HOSPITAL_IDENTIFIER,
         X_1_03_NHS_NUMBER,
         X_AGE_AT_OP,
         everything())

###-----------------------------------------------------------------------------------------------------------
# Remove duplicates based on NHS number, age, sec, admission date, and operation time
###-----------------------------------------------------------------------------------------------------------

ACS_data_clean <- ACS_data %>%
  distinct(X_1_03_NHS_NUMBER, X_AGE_AT_OP, X_SEX, X_ADMISSION_DATE, X_DATE_AND_TIME_OF_OPERATION,
           .keep_all = TRUE)





