## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(dplyr)
library(knitr)
library(reshape2)
library(kableExtra)


## ----echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------
gss <- read.csv('gss.csv')

gss_clean <- gss %>% 
  transmute(age, 
            sex,

            place_birth_canada, #place of birth of respondent - Canada
            place_birth_macro_region, #place of birth of respondent - Geographical macro-region
            
            marital_status, 
            total_children, #Total number of children reported by respondent
            number_total_children_intention,
            has_grandchildren,
            
            feelings_life, #feelings of life as a whole
            self_rated_health,
            self_rated_mental_health,
            
            education, 
            occupation,
            
            religion_has_affiliation, #Religious affiliation flag
            'religion_importance' = regilion_importance, #Importance of religious or spiritual beliefs
            religion_participation,
            
            income_family, #Family income - Total (before tax)
            income_respondent #Income of respondent - Total (before tax)
            )

write_csv(gss_clean, 'gss_clean.csv')

gss_clean$income_respondent <- factor(gss_clean$income_respondent, levels = c("Less than $25,000", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $ 124,999", "$125,000 and more"))
gss_clean$income_family <- factor(gss_clean$income_family, levels = c("Less than $25,000", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $ 124,999", "$125,000 and more"))


## ----include = FALSE---------------------------------------------------------------------------------------------
a <- gss_clean %>% 
  filter(income_respondent == "Less than $25,000") %>% 
  count()

b <- gss_clean %>% 
  filter(income_respondent == "$25,000 to $49,999") %>% 
  count()


## ----include = FALSE---------------------------------------------------------------------------------------------
c <- gss_clean %>% 
  filter(income_respondent == "$125,000 and more") %>% 
  count()

d <- gss_clean %>% 
  filter(income_respondent == "$125,000 and more",
         marital_status == "Married") %>% 
  count()


## ----fig.cap = "\\label{fig:indincome}Individual Total Income (Before Tax)", echo=FALSE, warning=FALSE, message=FALSE----
gss_clean %>% 
  ggplot(aes(x = income_respondent)) +
  geom_bar(fill = "dark green", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Survery Respondents' Individual Income Level",
       y = "Count") +
  theme(axis.text.x = element_text(size = 6)) 


## ----fig.cap = "\\label{fig:famincome}Family Total Income (Before Tax)", echo=FALSE, warning=FALSE, message=FALSE----
gss_clean %>% 
  ggplot(aes(x = income_family)) +
  geom_bar(fill = "dark blue", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Survery Respondents' Family Income Level",
       y = "Count") +
  theme(axis.text.x = element_text(size = 6)) 


## ----fig.cap = "\\label{fig:fammarital}Family Total Income Across Marital Status", echo=FALSE, warning=FALSE, message=FALSE----
gss_clean %>%
  ggplot(aes(x = income_family, fill = marital_status)) +
  geom_histogram(colour = "#e9ecef", alpha = 0.7, stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Family Income of Respondent",
       y = "Count",
       fill = "Marital Status")


## ----fig.cap = "\\label{fig:indmarital}Individual Total Income Across Marital Status", echo=FALSE, warning=FALSE, message=FALSE----
gss_clean %>%
  ggplot(aes(x = income_respondent, fill = marital_status)) +
  geom_histogram(colour = "#e9ecef", alpha = 0.7, stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Personal Income of Respondent",
       y = "Count",
       fill = "Marital Status")


## ----fig.cap = "\\label{fig:child}Individual Total Income and Children", fig.height = 3, echo=FALSE, warning=FALSE, message=FALSE----
gss_child <- gss_clean %>% 
  mutate('grandchildren' = case_when(
    has_grandchildren == 'Yes' ~ 1,
    has_grandchildren == 'No' ~ 0)) %>% 
  transmute(total_children, number_total_children_intention, grandchildren, income_family, income_respondent) %>% 
  drop_na() %>% 
  pivot_longer(!c(income_family, income_respondent), names_to = "name", values_to = "value")

labeller <- c(grandchildren = "Whether Has Grandchildren", total_children = "Number of Children in Total", number_total_children_intention = "Number of Children Intended to Have")      

gss_child %>% 
  ggplot(aes(x = factor(value), fill = income_respondent)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  facet_grid(~ name, labeller = as_labeller(labeller)) +  
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  labs(x = "Number of (Intended) Children or Grandchilren",
       y = "Count",
       fill = "Income Level")


## ----fig.cap = "\\label{fig:agegender}Individual Income, Gender, and Age", echo=FALSE, warning=FALSE, message=FALSE----
gss_clean %>% 
  ggplot(aes(x = age, y = income_respondent, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Age",
       y = "Income Level",
       fill = "Gender")


## ----fig.cap = "\\label{fig:incomeandbirth}Individual Income and Place of Birth", echo=FALSE, warning=FALSE, message=FALSE----
gss_clean %>%
  ggplot(aes(x = income_respondent, fill = place_birth_canada)) +
  geom_histogram(alpha = 0.8, stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Income Level",
       y = "Count",
       fill = "Place of Birth")


## ----fig.cap = "\\label{fig:outsidecanada}Individual Income of Respondents Born Outside Canada", echo=FALSE, warning=FALSE, message=FALSE----
bornoutsidecanada <- gss_clean %>%
  filter(gss_clean$place_birth_canada == "Born outside Canada")

bornoutsidecanada %>% 
  ggplot(aes(x = income_respondent, fill = place_birth_macro_region)) +
  geom_histogram(alpha = 0.8, stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Income Level",
       y = "Count",
       fill = "Geographical Macro Region Respondent was Born")


## ----fig.cap = "\\label{fig:health}Individual Income and Self-rated Health Level", fig.height = 3.5, echo=FALSE, warning=FALSE, message=FALSE----
gss_life <- gss_clean %>% 
  transmute(feelings_life, 'Physical Health' = self_rated_health, 'Mental Health' = self_rated_mental_health, income_respondent) %>% 
  drop_na() %>% 
  pivot_longer(!c(feelings_life, income_respondent), names_to = "health_type", values_to = "level")

gss_life$level <- factor(gss_life$level, levels = c("Excellent", "Very good", "Good", "Fair", "Poor", "Don't know"))

gss_life %>% 
  ggplot(aes(x = level, fill = health_type)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  facet_wrap(~ income_respondent) +  
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Self-rated Health Level",
       y = "Count",
       fill = "Mental/Physical Health")


## ----fig.cap = "\\label{fig:life}Individual Income and Life Satisfaction Level", fig.height = 3.5, echo=FALSE, warning=FALSE, message=FALSE----
gss_life %>% 
  ggplot(aes(x = feelings_life)) +
  geom_histogram(stat = "count", alpha = 0.8, fill = "dark blue") +
  facet_wrap(~ income_respondent) + 
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Level of Life Satisfaction",
       y = "Count")


## ----tabedu, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------
gss_edu_ocu <- gss_clean %>% 
  transmute(education, occupation, income_respondent) %>% 
  drop_na()

gss_edu_ocu$income_respondent <- factor(gss_edu_ocu$income_respondent, levels = c("Less than $25,000", "$25,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $124,999", "$125,000 and more"))

gss_edu <- gss_edu_ocu %>% 
  count(education, income_respondent) %>% 
  dcast(education ~ income_respondent)

knitr::kable(gss_edu, caption = "Individual Income and Education Level", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"))



## ----fig.cap = "\\label{fig:education}Individual Income and Education Level", fig.height = 3.5, echo=FALSE, warning=FALSE, message=FALSE----
gss_edu_ocu %>% 
  ggplot(aes(x = income_respondent, fill = education)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, size = 4)) +
  labs(x = "Respondents' Income Level",
       y = "Count",
       fill = "Education Level")


## ----tabocu, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------
gss_ocu <- gss_edu_ocu %>% 
  count(occupation, income_respondent) %>% 
  dcast(occupation ~ income_respondent) 

knitr::kable(gss_ocu, caption = "Individual Income and Occupation", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"))


## ----fig.cap = "\\label{fig:occupation}Individual Income and Occupation", fig.height = 3.5, echo=FALSE, warning=FALSE, message=FALSE----
gss_edu_ocu %>% 
  ggplot(aes(x = income_respondent, fill = occupation)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, size = 4)) +
  labs(x = "Respondents' Income Level",
       y = "Count",
       fill = "Occupation")


## ----fig.cap = "\\label{fig:affiliation}Individual Income and Religion Affiliation", echo=FALSE, warning=FALSE, message=FALSE----
gss_rel <- gss_clean %>% 
  transmute(religion_has_affiliation, religion_importance, religion_participation, income_respondent) %>% 
  drop_na()

gss_rel %>% 
  ggplot(aes(x = income_respondent, fill = religion_has_affiliation)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Respondents' Income Level",
       y = "Count",
       fill = "Religion Affiliation")


## ----fig.cap = "\\label{fig:importance}Individual Income and Religion Importance", echo=FALSE, warning=FALSE, message=FALSE----
gss_rel %>% 
  ggplot(aes(x = income_respondent, fill = religion_importance)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Respondents' Income Level",
       y = "Count",
       fill = "Importance of Religion")


## ----fig.cap = "\\label{fig:participation}Individual Income and Religion Participation", echo=FALSE, warning=FALSE, message=FALSE----
gss_rel %>% 
  ggplot(aes(x = income_respondent, fill = religion_participation)) +
  geom_histogram(stat = "count", alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 45, size = 5)) +
  labs(x = "Respondents' Income Level",
       y = "Count",
       fill = "Frequency of Religion Participation")

