
require(dplyr)
require(tidyr)
library(readr)
library(dplyr) 
library(survival)
library(splines)
library(ggplot2)
library(lubridate)
require(fst)
require(broom)
require(timeDate)
require(gridExtra)


# Get a vector of holiday dates
holiday_dates <- holidayNYSE(2019:2022,
                             type = "standard") 
holiday_dates <- as.Date(holiday_dates)


setwd("~/outagesxasthma/data")
dta <- read.fst("sparcs_outages_buildings_full.fst") %>%
  filter(!(year_month >= "2020-03" & year_month < "2021-01")) %>%
  arrange(id) %>%
  mutate(Case = if_else(DayName == "CaseDay_0", 1,0))%>%
  filter(AGE >= 5)%>%
  mutate(is_holiday = DayDate %in% holiday_dates)



mod.lin <- clogit(Case ~ exposed +
                    ns(mean_temp, df = 2)+
                    sum_precip+
                    ns(max_ws, df = 2)+
                    mean_rh+
                    is_holiday+
                    strata(id), # each case day is a strata
                  method = "efron",   # the method tells the model how to deal with ties
                  dta)

summary(mod.lin)

# 3B create predictions for each model

N_case = sum(dta$Case)

exp <- dta %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)

mod.lin_results <- tidy(mod.lin) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Overall") %>%
  filter(term == "exposed") %>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)





############################################################
### Hottest months
############################################################


dta_hot <- dta %>% filter(month(date.x)%in% c(6,7,8))


mod.lin_hot <- clogit(Case ~ exposed +
                        ns(mean_temp, df = 2)+
                        sum_precip+
                        ns(max_ws, df = 2)+
                        avg_daily_pm+
                        mean_rh+
                        is_holiday+
                        strata(id), # each case day is a strata
                      method = "efron",   # the method tells the model how to deal with ties
                      dta_hot)

summary(mod.lin_hot)

N_case = sum(dta_hot$Case)

exp <- dta_hot %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)


mod.lin_hot_result <- tidy(mod.lin_hot) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Hot months (Jun-Aug)") %>%
  filter(term == "exposed")%>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)




############################################################
### Hottest days
############################################################


dta_hot <- dta %>% filter(max_temp >= 23.88889)


mod.lin_hot <- clogit(Case ~ exposed +
                        ns(mean_temp, df = 2)+
                        sum_precip+
                        ns(max_ws, df = 2)+
                        avg_daily_pm+
                        mean_rh+
                        is_holiday+
                        strata(id), # each case day is a strata
                      method = "efron",   # the method tells the model how to deal with ties
                      dta_hot)

summary(mod.lin_hot)

N_case = sum(dta_hot$Case)

exp <- dta_hot %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)


mod.lin_hot_day_result <- tidy(mod.lin_hot) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Warm days (Max >75F)") %>%
  filter(term == "exposed")%>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)

############################################################
### Coldest months
############################################################


dta_cold <- dta %>% filter(month(date.x)%in% c(12,1,2))


mod.lin_cold <- clogit(Case ~ exposed +
                         ns(mean_temp, df = 2)+
                         sum_precip+
                         ns(max_ws, df = 2)+
                         avg_daily_pm+
                         mean_rh+
                         is_holiday+
                         strata(id), # each case day is a strata
                       method = "efron",   # the method tells the model how to deal with ties
                       dta_cold)

summary(mod.lin_cold)

N_case = sum(dta_cold$Case)

exp <- dta_cold %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)


mod.lin_cold_result <- tidy(mod.lin_cold) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Cold months (Dec-Feb)") %>%
  filter(term == "exposed")%>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)




############################################################
### Coldest days
############################################################


dta_cold <- dta %>% filter(max_temp <= 12.7778)


mod.lin_cold <- clogit(Case ~ exposed +
                         ns(mean_temp, df = 2)+
                         sum_precip+
                         ns(max_ws, df = 2)+
                         avg_daily_pm+
                         mean_rh+
                         is_holiday+
                         strata(id), # each case day is a strata
                       method = "efron",   # the method tells the model how to deal with ties
                       dta_cold)

summary(mod.lin_cold)

N_case = sum(dta_cold$Case)

exp <- dta_cold %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)


mod.lin_cold_day_result <- tidy(mod.lin_cold) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Cold days (Max <55F)") %>%
  filter(term == "exposed")%>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)






#############################################################
### just children
#############################################################

dta_children <- dta %>% filter(AGE < 18)


mod.lin_children <- clogit(Case ~ exposed +
                             ns(mean_temp, df = 2)+
                             sum_precip+
                             ns(max_ws, df = 2)+
                             avg_daily_pm+
                             mean_rh+
                             
                             
                             is_holiday+
                             
                             strata(id), # each case day is a strata
                           method = "efron",   # the method tells the model how to deal with ties
                           dta_children)

N_case = sum(dta_children$Case)

exp <- dta_children %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)

mod.lin_children_result <- tidy(mod.lin_children) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Children only") %>%
  filter(term == "exposed")%>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)


#############################################################
### just adults
#############################################################

dta_adults <- dta %>% filter(AGE >= 18)


mod.lin_adults <- clogit(Case ~ exposed +
                           ns(mean_temp, df = 2)+
                           sum_precip+
                           ns(max_ws, df = 2)+
                           avg_daily_pm+
                           mean_rh+
                           
                           is_holiday+
                           
                           strata(id), # each case day is a strata
                         method = "efron",   # the method tells the model how to deal with ties
                         dta_adults)

N_case = sum(dta_adults$Case)

exp <- dta_adults %>% filter(exposed == 1)
N_exposed_case = sum(exp$Case)

mod.lin_adults_result <- tidy(mod.lin_adults) %>%
  mutate(fit.or = exp(estimate),
         lci.or = exp(estimate - 1.96 * std.error), 
         uci.or = exp(estimate + 1.96 * std.error),
         group = "Adults only") %>%
  filter(term == "exposed")%>%
  mutate(n_case = N_case,
         n_exposed_case = N_exposed_case)




#############################################################
### Plot all
#############################################################


results_data <- rbind(mod.lin_results,
                      mod.lin_hot_result,
                      mod.lin_cold_result,
                      mod.lin_hot_day_result,
                      mod.lin_cold_day_result,
                      mod.lin_children_result,
                      mod.lin_adults_result
)


# Change the factor levels of the 'group' variable
results_data$group <- factor(results_data$group, levels = c("Adults only",
                                                            "Children only",
                                                            "Cold days (Max <55F)",
                                                            "Warm days (Max >75F)",
                                                            "Cold months (Dec-Feb)",
                                                            "Hot months (Jun-Aug)",
                                                            "Overall"))


# Create a ggplot
p2 <- ggplot(results_data, aes(y = group, x = fit.or)) +
  geom_point(size = 3) +            # Add points for odds ratios
  geom_errorbar(
    aes(xmin = lci.or, xmax = uci.or), 
    width = 0.1#,                    # Adjust the width of error bars
   # position = position_dodge(0.2)  # Dodge the error bars for better visibility
  ) +  
  geom_text(
    aes(label = sprintf("%.2f (%.2f, %.2f)", fit.or, lci.or, uci.or)),
    vjust = -1, hjust = .5,  # Adjust the vertical position of text labels
    size = 4,    # Adjust the text size
    position = position_dodge(0.2)
  ) +  
  geom_text(
    aes(label = paste0("N exposed case days = ", n_exposed_case)),
    vjust = 2, hjust = .5,  # Adjust the vertical position of text labels
    size = 4,    # Adjust the text size
    position = position_dodge(0.2))+
  geom_vline(xintercept = 1, linetype = "dotted", color = "purple") +  # Dotted line at y = 1
  labs(
    x = "Odds Ratio",
    y = "",
    title = "b. Excluding March 2020 - Dec 2020",
  ) +
  theme_minimal() +
  theme(text = element_text(size = 15), axis.text.y = element_blank())+
  scale_x_log10() 
p2

grid.arrange(p, p2, ncol = 2, widths = c(3, 2))
