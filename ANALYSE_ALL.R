###########################################################
############# IMPORT DATA AND PACKAGE USED #################
###########################################################
rm(list=ls())
setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/2nd_paper/3. Analysis")
getwd()

## read data from stata
library(haven)
tvsep <- read_dta("ALL_CLEANED_DATA.dta")

## prepare and clean all data again
### drop year 2007 since it has not enough all variables
library(dplyr)
tvsep <- filter(tvsep, year != 2007)
## recode irrigation, crop_category, and education to become factor
#tvsep_clean <- tvsep_clean %>% mutate(irrigation_factor = case_when(
 # irrigation == 1 ~ "No irrigation (rainfed, pond, lake, river)",
  #irrigation == 2 ~ "Irrigated from well, tap, gravity",
  #irrigation == 3 ~ "Use public irrigation canal")) %>% 
  #mutate(crop_category_factor = case_when(
   # crop_category == 1 ~ "Field crops (rice, corn and others)",
    #crop_category == 2 ~ "Tree crops",
    #crop_category == 3 ~ "Horticultural crops")) %>% 
 # mutate(education_factor = case_when(
  #  education == 1 ~ "Primary & secondary level",
   # education == 2 ~ "High school",
    #education == 3 ~ "Bachelor or more"))
tvsep <- tvsep %>% mutate(Province = case_when(
  prov == 405 ~ "Ha Tinh",
  prov == 411 ~ "Thua Thien Hue",
  prov == 605 ~ "Dak Lak")) %>% 
  mutate(risk_freq_factor = case_when(
    risk_freq_next5 == 1 ~"1 time",
    risk_freq_next5 == 2 ~"2 times",
    risk_freq_next5 == 3 ~"3 times",
    risk_freq_next5 == 4 ~"4 times",
    risk_freq_next5 == 5 ~"5 times",
    risk_freq_next5 == 6 ~"6 times")) %>% 
  mutate(year = case_when(
    year == 2008 ~ "2008",
    year == 2010 ~ "2010",
    year == 2013 ~ "2013",
    year == 2016 ~ "2016",
    year == 2017 ~ "2017")) %>% 
  mutate(drought_impact_inc_fct = case_when(
    drought_impact_inc == 1 ~ "No impact",
    drought_impact_inc == 2 ~ "Low impact",
    drought_impact_inc == 3 ~ "Moderate impact",
    drought_impact_inc == 4 ~ "High impact")) %>% 
  mutate(drought_impact_ass_fct = case_when(
    drought_impact_ass == 1 ~ "No impact",
    drought_impact_ass == 2 ~ "Low impact",
    drought_impact_ass == 3 ~ "Moderate impact",
    drought_impact_ass == 4 ~ "High impact")) %>% 
  mutate(education_factor = case_when(
    education == 1 ~ "Primary & secondary level",
    education == 2 ~ "High school",
    education == 3 ~ "Bachelor or more")) %>% 
  mutate(mitigation = case_when(
    mitigation == 0 ~ "No coping",
    mitigation == 1 ~ "Crop diversification",
    mitigation == 2 ~ "Action for irrigation, infrastructure",
    mitigation == 3 ~ "Capital investment",
    mitigation == 4 ~ "Income and property management",
    mitigation == 5 ~ "Others"))
tvsep$education_factor <- relevel(as.factor(tvsep$education_factor), ref = "Primary & secondary level")

##############################################################################
#################### PART 2.1: PERCEPTION  ###################################
##############################################################################

## PLOT PERCEPTION OF DROUGHT IN THE FUTURE BAR CHART
### BY PROVINCE
## perception of drought frequency
tvsep$risk_freq_next5 <- as.factor(tvsep$risk_freq_next5)
a <- tvsep %>%dplyr::select(risk_freq_next5,Province)
aa <- na.omit(a)
data_frequency_prov <- aa %>%
  group_by(Province, risk_freq_next5) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(Province) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(Province, percent, fill = risk_freq_next5))+ 
  scale_fill_brewer(palette = "YlOrBr")+
  labs(fill="Frequency")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
data_frequency_prov

## perception of drought impact to income
tvsep$drought_impact_inc <- as.factor(tvsep$drought_impact_inc)
tvsep$drought_impact_ass <- as.factor(tvsep$drought_impact_ass)

b <- tvsep %>%dplyr::select(drought_impact_inc,Province)
bb <- na.omit(b)
drought_impact_inc_prov <- bb %>%
  group_by(Province, drought_impact_inc) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(Province) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(Province, percent, fill = drought_impact_inc))+ 
  scale_fill_brewer(palette = "YlOrBr",labels=c("No impact",
                                               "Low impact",
                                               "Moderate impact",
                                               "High impact"))+
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")

drought_impact_inc_prov
## perception of drought impact to asset
c <- tvsep %>%dplyr::select(drought_impact_ass, Province)
cc <- na.omit(c)
drought_impact_ass_prov <- cc %>%
  group_by(Province, drought_impact_ass) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(Province) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(Province, percent, fill = drought_impact_ass))+ 
  scale_fill_brewer(palette = "YlOrBr",labels=c("No impact",
                                                "Low impact",
                                                "Moderate impact",
                                                "High impact"))+
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
drought_impact_ass_prov

### BY YEARS
## perception of drought frequency
tvsep$risk_freq_next5 <- as.factor(tvsep$risk_freq_next5)
a <- tvsep %>%dplyr::select(risk_freq_next5,year)
aa <- na.omit(a)
data_frequency_year <- aa %>%
  group_by(year, risk_freq_next5) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(year, percent, fill = risk_freq_next5))+ 
  scale_fill_brewer(palette = "YlOrBr")+
  labs(fill="Frequency")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
data_frequency_year
## perception of drought impact to income
tvsep$drought_impact_inc <- as.factor(tvsep$drought_impact_inc)
tvsep$drought_impact_ass <- as.factor(tvsep$drought_impact_ass)

b <- tvsep %>%dplyr::select(drought_impact_inc,year)
bb <- na.omit(b)
drought_impact_inc_year <- bb %>%
  group_by(year, drought_impact_inc) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(year, percent, fill = drought_impact_inc))+ 
  scale_fill_brewer(palette = "YlOrBr",labels=c("No impact",
                                                "Low impact",
                                                "Moderate impact",
                                                "High impact"))+
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")

drought_impact_inc_year
## perception of drought impact to asset
c <- tvsep %>%dplyr::select(drought_impact_ass, year)
cc <- na.omit(c)
drought_impact_ass_year <- cc %>%
  group_by(year, drought_impact_ass) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(year, percent, fill = drought_impact_ass))+ 
  scale_fill_brewer(palette = "YlOrBr",labels=c("No impact",
                                                "Low impact",
                                                "Moderate impact",
                                                "High impact"))+
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
drought_impact_ass_year

## COMBINE THE FIGURES TOGETHER
library(cowplot) # arrange the three plots in a single row
## Drought perception
prow <- plot_grid( data_frequency_prov + theme(legend.position="none"),
                   data_frequency_year+ theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1)
# extract the legend from one of the plots
legend_b <- get_legend(data_frequency_year + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1,0.1))
p
## Drought impact to income
prow <- plot_grid( drought_impact_inc_prov + theme(legend.position="none"),
                   drought_impact_inc_year+ theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1)
# extract the legend from one of the plots
legend_b <- get_legend(drought_impact_inc_prov + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1,0.1))
p
## drought impact to assets
prow <- plot_grid( drought_impact_ass_prov + theme(legend.position="none"),
                   drought_impact_ass_year+ theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1)
# extract the legend from one of the plots
legend_b <- get_legend(drought_impact_ass_prov + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1,0.1))
p
###### CHOOSING VARIABLES ########
tvsep_clean <- tvsep %>% 
  dplyr::select(risk_freq_next5, drought_impact_inc, drought_impact_ass, prevention,
                total_10y, total_7y, total_5y, total_3y, drought_exp,
                irri_public, irri_well, irri_nature, crop_horticultural, crop_tree, crop_rice, gender, 
                employ, age_year, ethnic, education, education_factor, income, hh_size, year, distr, prov)
tvsep_clean <- na.omit(tvsep_clean)

#tvsep_clean$irrigation_factor <- relevel(factor(tvsep_clean$irrigation),ref=1)
#tvsep_clean$crop_category_factor <- relevel(factor(tvsep_clean$crop_category),ref=1)
#tvsep_clean$education_factor <- relevel(factor(tvsep_clean$education),ref=1)

#+ irri_public+ irri_well + irri_nature 
#risk_freq_next5
library(MASS)
olr_perception_freq_10 <- polr(factor(risk_freq_next5) ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                              age_year + ethnic + education_factor + log(income) + hh_size,
                             data = tvsep_clean, Hess = TRUE)
olr_perception_freq_7 <- polr(factor(risk_freq_next5) ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                 age_year + ethnic + education_factor + log(income) + hh_size,
                               data = tvsep_clean, Hess = TRUE)
olr_perception_freq_5 <- polr(factor(risk_freq_next5) ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                 age_year + ethnic + education_factor + log(income) + hh_size,
                               data = tvsep_clean, Hess = TRUE)
olr_perception_freq_3 <- polr(factor(risk_freq_next5) ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                 age_year + ethnic + education_factor + log(income) + hh_size,
                               data = tvsep_clean, Hess = TRUE)
olr_perception_freq_exp <- polr(factor(risk_freq_next5) ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                 age_year + ethnic + education_factor + log(income) + hh_size,
                               data = tvsep_clean, Hess = TRUE)

## with fixed effect district
olr_perception_freq_10_fix <- polr(factor(risk_freq_next5) ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                 age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                               data = tvsep_clean, Hess = TRUE)
olr_perception_freq_7_fix <- polr(factor(risk_freq_next5) ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                              data = tvsep_clean, Hess = TRUE)
olr_perception_freq_5_fix <- polr(factor(risk_freq_next5) ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                              data = tvsep_clean, Hess = TRUE)
olr_perception_freq_3_fix <- polr(factor(risk_freq_next5) ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                              data = tvsep_clean, Hess = TRUE)
olr_perception_freq_exp_fix <- polr(factor(risk_freq_next5) ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                  age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                                data = tvsep_clean, Hess = TRUE)


library(stargazer)
models <- list(olr_perception_freq_10, olr_perception_freq_7, olr_perception_freq_5, olr_perception_freq_3,olr_perception_freq_exp,
               olr_perception_freq_10_fix, olr_perception_freq_7_fix, olr_perception_freq_5_fix, olr_perception_freq_3_fix,olr_perception_freq_exp_fix)
stargazer(models, apply.coef=exp, t.auto=F, p.auto=F, type = "html",
          out="OLR Relative risk ratios of Drought Perception.htm",align=TRUE)

library(brant)
a <- brant(olr_perception_freq_10, by.var=F)
b <- brant(olr_perception_freq_7, by.var=F)
c <- brant(olr_perception_freq_5, by.var=F)
d <- brant(olr_perception_freq_3, by.var=F)
e <- brant(olr_perception_freq_exp, by.var=F)

####################
# drought_impact_inc
olr_impact_inc_10 <- polr(factor(drought_impact_inc) ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                            age_year + ethnic + education_factor + log(income) + hh_size,
                          data = tvsep_clean, Hess = TRUE)
olr_impact_inc_7 <- polr(factor(drought_impact_inc) ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size,
                         data = tvsep_clean, Hess = TRUE)
olr_impact_inc_5 <- polr(factor(drought_impact_inc) ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size,
                         data = tvsep_clean, Hess = TRUE)
olr_impact_inc_3 <- polr(factor(drought_impact_inc) ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size,
                         data = tvsep_clean, Hess = TRUE)
olr_impact_inc_exp <- polr(factor(drought_impact_inc) ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                             age_year + ethnic + education_factor + log(income) + hh_size,
                           data = tvsep_clean, Hess = TRUE)

## with fixed effect district
olr_impact_inc_10_fix <- polr(factor(drought_impact_inc) ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                            age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                          data = tvsep_clean, Hess = TRUE)
olr_impact_inc_7_fix <- polr(factor(drought_impact_inc) ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                         data = tvsep_clean, Hess = TRUE)
olr_impact_inc_5_fix <- polr(factor(drought_impact_inc) ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                         data = tvsep_clean, Hess = TRUE)
olr_impact_inc_3_fix <- polr(factor(drought_impact_inc) ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                         data = tvsep_clean, Hess = TRUE)
olr_impact_inc_exp_fix <- polr(factor(drought_impact_inc) ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                             age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                           data = tvsep_clean, Hess = TRUE)

library(stargazer)
models_2 <- list(olr_impact_inc_10, olr_impact_inc_7, olr_impact_inc_5, olr_impact_inc_3,olr_impact_inc_exp,
               olr_impact_inc_10_fix, olr_impact_inc_7_fix, olr_impact_inc_5_fix, olr_impact_inc_3_fix,olr_impact_inc_exp_fix)
stargazer(models_2, apply.coef=exp, t.auto=F, p.auto=F, type = "html",
          out="OLR Relative risk ratios of Impact-to-income Perception.htm",align=TRUE)

####################
# drought_impact_ass
olr_impact_ass_10 <- polr(factor(drought_impact_ass) ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                            age_year + ethnic + education_factor + log(income) + hh_size,
                          data = tvsep_clean, Hess = TRUE)
olr_impact_ass_7 <- polr(factor(drought_impact_ass) ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size,
                         data = tvsep_clean, Hess = TRUE)
olr_impact_ass_5 <- polr(factor(drought_impact_ass) ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size,
                         data = tvsep_clean, Hess = TRUE)
olr_impact_ass_3 <- polr(factor(drought_impact_ass) ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size,
                         data = tvsep_clean, Hess = TRUE)
olr_impact_ass_exp <- polr(factor(drought_impact_ass) ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                             age_year + ethnic + education_factor + log(income) + hh_size,
                           data = tvsep_clean, Hess = TRUE)

## with fixed effect district
olr_impact_ass_10_fix <- polr(factor(drought_impact_ass) ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),
                              data = tvsep_clean, Hess = TRUE)
olr_impact_ass_7_fix <- polr(factor(drought_impact_ass) ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                               age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                             data = tvsep_clean, Hess = TRUE)
olr_impact_ass_5_fix <- polr(factor(drought_impact_ass) ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                               age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                             data = tvsep_clean, Hess = TRUE)
olr_impact_ass_3_fix <- polr(factor(drought_impact_ass) ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                               age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                             data = tvsep_clean, Hess = TRUE)
olr_impact_ass_exp_fix <- polr(factor(drought_impact_ass) ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                 age_year + ethnic + education_factor + log(income) + hh_size+ factor(distr),
                               data = tvsep_clean, Hess = TRUE)

library(stargazer)
models_3 <- list(olr_impact_ass_10, olr_impact_ass_7, olr_impact_ass_5, olr_impact_ass_3,olr_impact_ass_exp,
                 olr_impact_ass_10_fix, olr_impact_ass_7_fix, olr_impact_ass_5_fix, olr_impact_ass_3_fix,olr_impact_ass_exp_fix)
stargazer(models_3, apply.coef=exp, t.auto=F, p.auto=F, type = "text",
          out="OLR Relative risk ratios of Impact-to-assets Perception.htm",align=TRUE)

#############################################
#########      PREVENTION    ################
## draw figures
tvsep$prevention <- as.factor(tvsep$prevention)
d <- tvsep %>%dplyr::select(prevention, year, Province)
dd <- na.omit(d)
prevention_year <- dd %>%
  group_by(year, prevention) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(year, percent, fill = prevention))+ 
  scale_fill_brewer(palette = "YlOrBr", labels = c("No","Yes"))+
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.7,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
prevention_year

prevention_prov <- dd %>%
  group_by(Province, prevention) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(Province) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(Province, percent, fill = prevention))+ 
  scale_fill_brewer(palette = "YlOrBr", labels = c("No","Yes"))+
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.7,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
prevention_prov

library(cowplot) # arrange the three plots in a single row
prow <- plot_grid( prevention_prov + theme(legend.position="none"),
                   prevention_year + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1)
# extract the legend from one of the plots
legend_b <- get_legend(prevention_prov + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1,0.1))
p

###### CHOOSING VARIABLES ########
tvsep_2 <- tvsep %>% 
  dplyr::select(prevention,
                total_10y, total_7y, total_5y, total_3y, drought_exp,
                irri_public, irri_well, irri_nature, crop_horticultural, crop_tree, crop_rice, gender, 
                employ, age_year, ethnic, education, education_factor, income, hh_size, year, distr, prov)
tvsep_2 <- na.omit(tvsep_2)

## REGRESIONS 
### LOGIT REGRESIONS
# prevention
#library(lme4)
#prevention_10 <- glmer(prevention ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
#                         age_year + ethnic + education_factor + log(income) + hh_size+ (1|distr), 
#                       data = tvsep_2, family=binomial("logit"))
library(survival) 

prevention_10 <- glm(prevention ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                       age_year + ethnic + education_factor + log(income) + hh_size,family=binomial(link='logit'),data=tvsep_2)
prevention_7 <- glm(prevention ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                       age_year + ethnic + education_factor + log(income) + hh_size,family=binomial(link='logit'),data=tvsep_2)
prevention_5 <- glm(prevention ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                       age_year + ethnic + education_factor + log(income) + hh_size,family=binomial(link='logit'),data=tvsep_2)
prevention_3 <- glm(prevention ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                       age_year + ethnic + education_factor + log(income) + hh_size,family=binomial(link='logit'),data=tvsep_2)
prevention_exp <- glm(prevention ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                      age_year + ethnic + education_factor + log(income) + hh_size,family=binomial(link='logit'),data=tvsep_2)

prevention_10_test <- glm(prevention ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                       age_year + ethnic + education_factor + log(income) + hh_size + factor(distr),family=binomial(link='logit'),data=tvsep_2)

prevention_10_fix <- clogit(prevention ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                          age_year + ethnic + education_factor + log(income) + hh_size+ strata(distr), data=tvsep_2)
prevention_7_fix <- clogit(prevention ~ total_7y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                              age_year + ethnic + education_factor + log(income) + hh_size+ strata(distr), data=tvsep_2)
prevention_5_fix <- clogit(prevention ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                              age_year + ethnic + education_factor + log(income) + hh_size+ strata(distr), data=tvsep_2)
prevention_3_fix <- clogit(prevention ~ total_3y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                              age_year + ethnic + education_factor + log(income) + hh_size+ strata(distr), data=tvsep_2)
prevention_exp_fix <- clogit(prevention ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                             age_year + ethnic + education_factor + log(income) + hh_size+ strata(distr), data=tvsep_2)

library(stargazer)
models_prevention <- list(prevention_10, prevention_7, prevention_5, prevention_3, prevention_exp, 
                          prevention_10_fix, prevention_7_fix, prevention_5_fix, prevention_3_fix, prevention_exp_fix)
stargazer(models_prevention, apply.coef=exp, t.auto=F, p.auto=F, type = "html",
          out="OLR prevention in the future_updated.htm",align=TRUE)                                        

models_prevention <- list(prevention_10, prevention_7, prevention_5, prevention_3, prevention_exp, 
                          prevention_10_test, prevention_10_fix)
stargazer(models_prevention, apply.coef=exp, t.auto=F, p.auto=F, type = "html",
          out="OLR prevention in the future_test.htm",align=TRUE)                                        

## Mitigation strategy
tvsep_mi <- tvsep %>% 
  dplyr::select(mitigation,
                total_10y, total_7y, total_5y, total_3y, drought_exp, Province, 
                irri_public, irri_well, irri_nature, crop_horticultural, crop_tree, crop_rice, gender, 
                employ, age_year, ethnic, education, education_factor, income, hh_size, year, distr, prov)
tvsep_mi <- na.omit(tvsep_mi)

library(ggplot2)
c <- tvsep_mi %>%dplyr::select(mitigation, Province, year)
cc <- na.omit(c)
mitigation_prov <- cc %>%
  group_by(Province, mitigation) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(Province) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(Province, percent, fill = mitigation))+ 
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
mitigation_prov

mitigation_year <- cc %>%
  group_by(year, mitigation) %>%
  summarise(total=n(),.groups = 'drop') %>% 
  group_by(year) %>% 
  mutate(percent = total/sum(total)) %>% 
  ggplot(aes(year, percent, fill = mitigation))+ 
  labs(fill="")+
  scale_y_continuous(limits = c(0, 1.01),labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE))+
  geom_col(width = 0.75,position = position_stack(reverse = TRUE)) +
  #geom_text(aes(label = percent), size = 2.5)+
  theme_bw() +
  ylab("") + xlab("")
mitigation_year

library(cowplot) # arrange the three plots in a single row
prow <- plot_grid(mitigation_prov + theme(legend.position="none"),
                  mitigation_year + theme(legend.position="none"),
                  align = 'vh',
                  hjust = -1,
                  nrow = 1)
# extract the legend from one of the plots
legend_b <- get_legend(mitigation_prov + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1,0.1))
p

## regression
library(nnet)
tvsep_mi$mitigation <- relevel(as.factor(tvsep_mi$mitigation), ref = "No coping")
mitigation_10 <- multinom(mitigation ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size, data=tvsep_mi, model= TRUE)
mitigation_5 <- multinom(mitigation ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                          age_year + ethnic + education_factor + log(income) + hh_size, data=tvsep_mi, model= TRUE)
mitigation_exp <- multinom(mitigation ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                           age_year + ethnic + education_factor + log(income) + hh_size, data=tvsep_mi, model= TRUE)

mitigation_10_fix <- multinom(mitigation ~ total_10y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                            age_year + ethnic + education_factor + log(income) + hh_size + factor(distr), data=tvsep_mi, model= TRUE)
mitigation_5_fix <- multinom(mitigation ~ total_5y + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                                age_year + ethnic + education_factor + log(income) + hh_size + factor(distr), data=tvsep_mi, model= TRUE)
mitigation_exp_fix <- multinom(mitigation ~ drought_exp + irri_public+ irri_well + crop_tree + crop_rice + gender + employ +
                               age_year + ethnic + education_factor + log(income) + hh_size + factor(distr), data=tvsep_mi, model= TRUE)

summary(mitigation_exp)
exp(coef(mitigation_10_fix))
library(stargazer)
stargazer(mitigation_10, mitigation_5, apply.coef=exp, t.auto=F, p.auto=F, type = "html",
          out="OLR mitigation strategies in the future.htm",align=TRUE) 
stargazer(mitigation_10_fix, mitigation_5_fix, apply.coef=exp, t.auto=F, p.auto=F, type = "html",
          out="OLR prevention with district fixed effect in the future_updated.htm",align=TRUE) 

#### Add grid map to GPS location

