#Call the analysis scripts (which in turn call data loading and cleaning scripts):
source(here::here("Scripts", "03_TAVI_Analysis.R"))
source(here::here("Scripts", "04_SAVR_Analysis.R"))

###-----------------------------------------------------------------------------------------------------------
# Fit a negative binomial model to 2017-2019 monthly TAVI/SAVR numbers, to forecast what we would expect to see
#through 2020, and therefore estimate the "expected loss in procedures#
###-----------------------------------------------------------------------------------------------------------
NB_Count_mod <- with(SAVR_Analysis_Cohort %>%
                       select(X_DATE_AND_TIME_OF_OPERATION, Surgical_Group) %>%
                       transmute(Month = month(X_DATE_AND_TIME_OF_OPERATION),
                                 Year = year(X_DATE_AND_TIME_OF_OPERATION),
                                 Surgical_Group = Surgical_Group) %>%
                       filter(Year != 2020) %>% #fit the model only to 2017-2019 data
                       group_by(Month, Year, Surgical_Group) %>%
                       count() %>%
                       ungroup() %>%
                       arrange(Year, Month) %>%
                       group_by(Surgical_Group) %>%
                       mutate("Time" = 1:n()) %>%
                       ungroup() %>%
                       mutate(Month = factor(Month)) %>%
                       bind_rows(TAVI_Analysis_Cohort %>%
                                   select(X7_01_DATE_AND_TIME_OF_OPERATION) %>%
                                   transmute(Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                                             Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
                                   filter(Year != 2020) %>% #fit the model only to 2017-2019 data
                                   group_by(Month, Year) %>%
                                   count() %>%
                                   ungroup() %>%
                                   arrange(Year, Month) %>%
                                   rowid_to_column("Time") %>%
                                   mutate(Month = factor(Month),
                                          "Surgical_Group" = "TAVR")),
                     MASS::glm.nb(n ~ -1 + Surgical_Group + Surgical_Group:Time + Surgical_Group:Month, 
                                  link = log))

predictions <- predict(NB_Count_mod,
                       newdata = SAVR_Analysis_Cohort %>%
                         select(X_DATE_AND_TIME_OF_OPERATION, Surgical_Group) %>%
                         transmute(Month = month(X_DATE_AND_TIME_OF_OPERATION),
                                   Year = year(X_DATE_AND_TIME_OF_OPERATION),
                                   Surgical_Group = Surgical_Group) %>%
                         group_by(Month, Year, Surgical_Group) %>%
                         count() %>%
                         ungroup() %>%
                         arrange(Year, Month) %>%
                         group_by(Surgical_Group) %>%
                         mutate("Time" = 1:n()) %>%
                         ungroup() %>%
                         mutate(Month = factor(Month)) %>%
                         bind_rows(TAVI_Analysis_Cohort %>%
                                     select(X7_01_DATE_AND_TIME_OF_OPERATION) %>%
                                     transmute(Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                                               Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
                                     group_by(Month, Year) %>%
                                     count() %>%
                                     ungroup() %>%
                                     arrange(Year, Month) %>%
                                     rowid_to_column("Time") %>%
                                     mutate(Month = factor(Month),
                                            "Surgical_Group" = "TAVR")),
                       type = "response",
                       se.fit = TRUE)

#Create a dataset that can be used to plot this data: 
NB_Count_Predictions <- SAVR_Analysis_Cohort %>%
  select(X_DATE_AND_TIME_OF_OPERATION, Surgical_Group) %>%
  transmute(Month = month(X_DATE_AND_TIME_OF_OPERATION),
            Year = year(X_DATE_AND_TIME_OF_OPERATION),
            Surgical_Group = Surgical_Group) %>%
  group_by(Month, Year, Surgical_Group) %>%
  count() %>%
  ungroup() %>%
  arrange(Year, Month) %>%
  group_by(Surgical_Group) %>%
  mutate("Time" = 1:n()) %>%
  ungroup() %>%
  mutate(Month = factor(Month)) %>%
  bind_rows(TAVI_Analysis_Cohort %>%
              select(X7_01_DATE_AND_TIME_OF_OPERATION) %>%
              transmute(Month = month(X7_01_DATE_AND_TIME_OF_OPERATION),
                        Year = year(X7_01_DATE_AND_TIME_OF_OPERATION)) %>%
              group_by(Month, Year) %>%
              count() %>%
              ungroup() %>%
              arrange(Year, Month) %>%
              rowid_to_column("Time") %>%
              mutate(Month = factor(Month),
                     "Surgical_Group" = "TAVR")) %>%
  mutate("Predicted_Counts" = as.numeric(predictions$fit),
         "SE" = as.numeric(predictions$se.fit),
         "Lower" = Predicted_Counts - (qnorm(0.975) * SE),
         "Upper" = Predicted_Counts + (qnorm(0.975) * SE))


