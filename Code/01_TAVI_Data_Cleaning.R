library(tidyverse)
library(lubridate)

###-----------------------------------------------------------------------------------------------------------
# Establish the connection to database
###-----------------------------------------------------------------------------------------------------------
con <- odbc::dbConnect(odbc::odbc(), dsn = "databricks")
con@quote = ''

#Load the TAVR dataset from the database:
TAVI_data <- DBI::dbGetQuery(con, "SELECT * FROM nicor_clear.tavi")

#Load the ONS mortality data:
Mortality_data <- DBI::dbGetQuery(con, "SELECT * FROM nicor_clear.dars_bird_deaths")

###-----------------------------------------------------------------------------------------------------------
# Change structure of the dataset
###-----------------------------------------------------------------------------------------------------------
TAVI_data <- TAVI_data %>%
  rename_all(function(x) paste0("X", x)) %>%
  rename("X5_052_KATZ" = "X5_052_KATZ_INDEX_OF_INDEPENDENCE_IN_ACTIVITIES_OF_DAILY_LIVING") %>%
  #convert all numeric variables into integer:
  mutate_at(.vars = c("X3_03_CREATININE",
                      "X5_01_HEIGHT",
                      "X5_02_WEIGHT",
                      "X5_052_KATZ",
                      "X6_012_PA_SYSTOLIC_PRESSURE_MMHG",
                      "X6_014_AORTIC_VALVE_MEAN_GRADIENT_MMHG",
                      "X6_02_AORTIC_VALVE_PEAK_GRADIENT_MMHG",
                      "X6_03_AORTIC_VALVE_AREA",
                      "X6_04_AORTIC_ANNULAR_DIAMETER",
                      "X7_16_VALVE_SIZE",
                      "X7_181_PROCEDURE_TIME_MINS",
                      "X8_022_POST_DEPLOYMENT_AORTIC_VALVE_PEAK_GRADIENT_MMHG",
                      "X8_023_POST_DEPLOYMENT_AORTIC_VALVE_MEAN_GRADIENT_MMHG",
                      "X8_024_POST_DEPLOYMENT_AORTIC_VALVE_AREA",
                      "XAGE_AT_OP"),
            as.numeric) %>%
  #turn all other variables into factors
  mutate_if(is.character, as.factor) %>%
  as_tibble()

###-----------------------------------------------------------------------------------------------------------
# Correct continuous variables
###-----------------------------------------------------------------------------------------------------------

###Creatinine
TAVI_data$X3_03_CREATININE[
  which(TAVI_data$X3_03_CREATININE > 5000)] <- NA

###Height
#Convert units for height from cm into m:
TAVI_data$X5_01_HEIGHT[
  which(TAVI_data$X5_01_HEIGHT > 100)] <- TAVI_data$X5_01_HEIGHT[
    which(TAVI_data$X5_01_HEIGHT > 100)] / 100
#convert any from inches to m:
TAVI_data$X5_01_HEIGHT[
  which(TAVI_data$X5_01_HEIGHT > 10 &
          TAVI_data$X5_01_HEIGHT < 150)] <- TAVI_data$X5_01_HEIGHT[
            which(TAVI_data$X5_01_HEIGHT > 10 &
                    TAVI_data$X5_01_HEIGHT < 150)] / 39.37
#Define a reasonable range for height in m:
TAVI_data$X5_01_HEIGHT[which(TAVI_data$X5_01_HEIGHT > 5)] <- NA
TAVI_data$X5_01_HEIGHT[which(TAVI_data$X5_01_HEIGHT < 1)] <- NA

###Weight
TAVI_data$X5_02_WEIGHT[
  which(TAVI_data$X5_02_WEIGHT > 250 |
          TAVI_data$X5_02_WEIGHT < 10)] <- NA 

###KATZ - make sure between 0 and 6 and whole number
TAVI_data$X5_052_KATZ <- ceiling(TAVI_data$X5_052_KATZ)
TAVI_data$X5_052_KATZ[
  which(TAVI_data$X5_052_KATZ > 6 |
          TAVI_data$X5_052_KATZ < 0)] <- NA

###PA Systolic
TAVI_data$X6_012_PA_SYSTOLIC_PRESSURE_MMHG[
  which(TAVI_data$X6_012_PA_SYSTOLIC_PRESSURE_MMHG > 150 |
          TAVI_data$X6_012_PA_SYSTOLIC_PRESSURE_MMHG == 0)] <- NA

###Aortic mean gradient
TAVI_data$X6_014_AORTIC_VALVE_MEAN_GRADIENT_MMHG[
  which(TAVI_data$X6_014_AORTIC_VALVE_MEAN_GRADIENT_MMHG > 200 |
          TAVI_data$X6_014_AORTIC_VALVE_MEAN_GRADIENT_MMHG == 0)] <- NA

###Aortic peak gradient
TAVI_data$X6_02_AORTIC_VALVE_PEAK_GRADIENT_MMHG[
  which(TAVI_data$X6_02_AORTIC_VALVE_PEAK_GRADIENT_MMHG > 300 |
          TAVI_data$X6_02_AORTIC_VALVE_PEAK_GRADIENT_MMHG == 0)] <- NA

###Aortic valve area
TAVI_data$X6_03_AORTIC_VALVE_AREA[
  which(TAVI_data$X6_03_AORTIC_VALVE_AREA > 100)] <- TAVI_data$X6_03_AORTIC_VALVE_AREA[
    which(TAVI_data$X6_03_AORTIC_VALVE_AREA > 100)] / 100
TAVI_data$X6_03_AORTIC_VALVE_AREA[
  which(TAVI_data$X6_03_AORTIC_VALVE_AREA > 2 |
          TAVI_data$X6_03_AORTIC_VALVE_AREA == 0)] <- NA

###Annular diameter
TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER[
  which(TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER > 100)] <- TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER[
    which(TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER > 100)] / 100
TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER[
  which(TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER < 10 |
          TAVI_data$X6_04_AORTIC_ANNULAR_DIAMETER > 60)] <- NA

###Valve size
TAVI_data$X7_16_VALVE_SIZE[
  which(TAVI_data$X7_16_VALVE_SIZE > 50)] <- NA


###Procedure time
TAVI_data$X7_181_PROCEDURE_TIME_MINS[
  which(TAVI_data$X7_181_PROCEDURE_TIME_MINS > 10000)] <- TAVI_data$X7_181_PROCEDURE_TIME_MINS[
    which(TAVI_data$X7_181_PROCEDURE_TIME_MINS > 10000)] / 60
TAVI_data$X7_181_PROCEDURE_TIME_MINS[
  which(TAVI_data$X7_181_PROCEDURE_TIME_MINS < 0 |
          TAVI_data$X7_181_PROCEDURE_TIME_MINS > 20000)] <- NA

###Post-op aortic peak gradient
TAVI_data$X8_022_POST_DEPLOYMENT_AORTIC_VALVE_PEAK_GRADIENT_MMHG[
  which(TAVI_data$X8_022_POST_DEPLOYMENT_AORTIC_VALVE_PEAK_GRADIENT_MMHG > 300 |
          TAVI_data$X8_022_POST_DEPLOYMENT_AORTIC_VALVE_PEAK_GRADIENT_MMHG == 0)] <- NA

###Post-op aortic mean gradient
TAVI_data$X8_023_POST_DEPLOYMENT_AORTIC_VALVE_MEAN_GRADIENT_MMHG[
  which(TAVI_data$X8_023_POST_DEPLOYMENT_AORTIC_VALVE_MEAN_GRADIENT_MMHG > 200 |
          TAVI_data$X8_023_POST_DEPLOYMENT_AORTIC_VALVE_MEAN_GRADIENT_MMHG == 0)] <- NA

##Post-op aortic area
TAVI_data$X8_024_POST_DEPLOYMENT_AORTIC_VALVE_AREA[
  which(TAVI_data$X8_024_POST_DEPLOYMENT_AORTIC_VALVE_AREA > 10 |
          TAVI_data$X8_024_POST_DEPLOYMENT_AORTIC_VALVE_AREA == 0)] <- NA

###Age
TAVI_data$XAGE_AT_OP[which(TAVI_data$XAGE_AT_OP == 0)] <- NA



###-----------------------------------------------------------------------------------------------------------
# Change/ clean factor levels, where needed
###-----------------------------------------------------------------------------------------------------------
TAVI_data <- TAVI_data %>%
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
  mutate(X3_02_SMOKING_STATUS = fct_collapse(X3_02_SMOKING_STATUS,
                                             "1. Current/Ex Smoker" = c("1. Ex smoker",
                                                                        "2. Current smoker")),
         
         X7_074_AORTIC_BALLOON_VALVULOPLASTY_BEFORE_VALVE_DEPLOYMENT = fct_collapse(
           X7_074_AORTIC_BALLOON_VALVULOPLASTY_BEFORE_VALVE_DEPLOYMENT,
           "0. Not done/ Failed" = c("0. Not done",
                                     "2. Failed")),
         
         X7_13_VALVE_MANUFACTURER = fct_collapse(X7_13_VALVE_MANUFACTURER,
                                                 "Other" = c("DFM. Direct Flow Medical",
                                                             "JNV. Jenavalve",
                                                             "OTR. Other (not listed)"))) %>%
  #Separate the pre-op heart rhythm variable into each component indicator variable (i.e.
  #rather than Sinus & AF, we create two new columns: one for sinus (yes/no) and one for AF
  mutate(X3_11_1_Atrial_Fib_Flutter = factor(ifelse(str_detect(as.character(X3_11_PRE_OPERATIVE_HEART_RHYTHM),
                                                               pattern = "1. Atrial fibrillation/flutter") ==
                                                      TRUE,
                                                    "1. Yes", "0. No"),
                                             levels = c("0. No", "1. Yes")),
         
         X3_11_2_Sinus = factor(ifelse(str_detect(as.character(X3_11_PRE_OPERATIVE_HEART_RHYTHM),
                                                  pattern = "0. Sinus rhythm") == TRUE |
                                         str_detect(as.character(X3_11_PRE_OPERATIVE_HEART_RHYTHM),
                                                    pattern = "2. 1st degree heart block") == TRUE,
                                       "1. Yes", "0. No"),
                                levels = c("0. No", "1. Yes")),
         .after = "X3_11_PRE_OPERATIVE_HEART_RHYTHM") %>%
  #Create new indicator variables for different previous cardiac surgeries:
  mutate(X4_01_1_Previous_CABG = factor(ifelse(str_detect(as.character(X4_01_PREVIOUS_CARDIAC_SURGERY),
                                                          pattern = "1. Previous CABG") == TRUE,
                                               "1. Yes", "0. No"),
                                        levels = c("0. No", "1. Yes")),
         
         X4_01_2_Previous_Valve_Op = factor(ifelse(str_detect(as.character(X4_01_PREVIOUS_CARDIAC_SURGERY),
                                                              pattern = "2. Previous valve operation") == TRUE,
                                                   "1. Yes", "0. No"),
                                            levels = c("0. No", "1. Yes")),
         
         X4_01_3_Other_preop_open_pericardium = factor(ifelse(str_detect(as.character(X4_01_PREVIOUS_CARDIAC_SURGERY),
                                                                          pattern = "3. Other operation requiring opening of the pericardium") == TRUE,
                                                              "1. Yes", "0. No"),
                                                        levels = c("0. No", "1. Yes")),
         .after = "X4_01_PREVIOUS_CARDIAC_SURGERY") %>%
  #Create new indicator variables for different vascular closure:
  mutate(X7_18_1_Vascular_closure_manual = factor(ifelse(str_detect(as.character(X7_18_VASCULAR_CLOSURE_TECHNIQUE),
                                                                    pattern = "0. Manual pressure") == TRUE,
                                                         "1. Yes", "0. No"),
                                                  levels = c("0. No", "1. Yes")),
         
         X7_18_2_Vascular_closure_device = factor(ifelse(str_detect(as.character(X7_18_VASCULAR_CLOSURE_TECHNIQUE),
                                                                    pattern = "2. Device closure") == TRUE,
                                                         "1. Yes", "0. No"),
                                                  levels = c("0. No", "1. Yes")),
         
         X7_18_3_Vascular_closure_planned_surgery = factor(ifelse(str_detect(as.character(X7_18_VASCULAR_CLOSURE_TECHNIQUE),
                                                                    pattern = "3. Surgical closure as planned") == TRUE,
                                                         "1. Yes", "0. No"),
                                                  levels = c("0. No", "1. Yes")),
         
         X7_18_4_Vascular_closure_bailout_surgery = factor(ifelse(str_detect(as.character(X7_18_VASCULAR_CLOSURE_TECHNIQUE),
                                                                             pattern = "4. Surgical closure as bail out from failed percutaneous attempt") == TRUE,
                                                                  "1. Yes", "0. No"),
                                                           levels = c("0. No", "1. Yes")),
         .after = "X7_18_VASCULAR_CLOSURE_TECHNIQUE")

###-----------------------------------------------------------------------------------------------------------
# Format date/time variables
###-----------------------------------------------------------------------------------------------------------

TAVI_data$X4_022_DATE_OF_PREVIOUS_BALLOON_AORTIC_VALVULOPLASTY <- dmy(
  as.character(TAVI_data$X4_022_DATE_OF_PREVIOUS_BALLOON_AORTIC_VALVULOPLASTY)
)

TAVI_data$X4_024_DATE_OF_PREVIOUS_TAVI <- dmy(
  as.character(TAVI_data$X4_024_DATE_OF_PREVIOUS_TAVI)
)

TAVI_data$X5_06_ADMISSION_DATE_FOR_PROCEDURE_FIRST_HOSPITAL_IN_CHAIN_IF_THERE_IS_ONE <- dmy(
  as.character(TAVI_data$X5_06_ADMISSION_DATE_FOR_PROCEDURE_FIRST_HOSPITAL_IN_CHAIN_IF_THERE_IS_ONE)
)

TAVI_data$X7_01_DATE_AND_TIME_OF_OPERATION <- dmy_hm(
  as.character(TAVI_data$X7_01_DATE_AND_TIME_OF_OPERATION)
)

TAVI_data$X10_01_DATE_OF_DISCHARGE_OR_DEATH <- dmy(
  as.character(TAVI_data$X10_01_DATE_OF_DISCHARGE_OR_DEATH)
)



###-----------------------------------------------------------------------------------------------------------
# Select variables
###-----------------------------------------------------------------------------------------------------------

TAVI_data <- TAVI_data %>%
  select(X1_01_HOSPITAL_IDENTIFIER,
         X1_03_NHS_NUMBER,
         XAGE_AT_OP,
         everything())

###-----------------------------------------------------------------------------------------------------------
# Remove duplicates based on NHS number, age, sec, admission date, and operation time
###-----------------------------------------------------------------------------------------------------------

TAVI_data_clean <- TAVI_data %>%
  distinct(X1_03_NHS_NUMBER, XAGE_AT_OP, X1_07_SEX, 
           X5_06_ADMISSION_DATE_FOR_PROCEDURE_FIRST_HOSPITAL_IN_CHAIN_IF_THERE_IS_ONE, 
           X7_01_DATE_AND_TIME_OF_OPERATION,
           .keep_all = TRUE)


