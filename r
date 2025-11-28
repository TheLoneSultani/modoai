# Libraries
library(tidyverse)
library(stargazer)
library(broom)
library(broom.mixed)
library(modelsummary)
library(summarytools)
library(kableExtra)
library(gt)
library(vtable)
library(aod)
library(pscl)
library(mice)

rm(list = ls())

# Set the wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Upload data
Leb_tidied <- read.csv("../data/Lebanon_tidied.csv")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------PART 0: DESCRIPTIVE STATISTICS ---------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# descr statistic with vTable
st(Leb_tidied, out = "viewer")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------PART 2: BIVARIATE ANALYSIS ---------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# distribution Sect x Dual Engagement
ggplot(data = Leb_tidied, aes(x=Dual_Eng, fill=Sect)) +
  geom_bar() +
  scale_y_continuous(name="Members\n") +
  scale_x_continuous(name="", breaks=c(0, 1), 
                     labels=c("Others", "Dual Eng.")) +
  theme_classic()


######## Analisi distribuzione dei Doppi partecipanti per tipo di partito ######## 
table(Leb_tidied$Party_type)
table(Leb_tidied$Dual_Eng)

table(Leb_tidied$Dual_Eng, Leb_tidied$Party_type)

# Frequencies #
BiDuobleParty <- table(Leb_tidied$Dual_Eng, Leb_tidied$Party_type)
BiDuobleParty %>% prop.table(margin = 1)
BiDuobleParty %>% prop.table(margin = 2) 
BiDuobleParty %>% prop.table()

chisq.test(BiDuobleParty)

# Converting in factors
Leb_tidied$Dual_Eng <- factor(Leb_tidied$Dual_Eng)
Leb_tidied$Party_type <- factor(Leb_tidied$Party_type)

# Visualization 1
Leb_tidied %>% 
  filter(!is.na(Party_type), !is.na(Dual_Eng)) %>%
  mutate(
    Party_type = factor(Party_type,
                        levels = c("Hezbollah", "Amal", "FPM", "PSP",
                                   "Others", "Kataeb", "FM", "LF")),
    Dual_Eng   = factor(Dual_Eng,
                        levels = c(0, 1),
                        labels = c("Others", "Dual Engagement"))) %>%
  ggplot(aes(x = Party_type, fill = Dual_Eng)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
    legend.position = "none",
    plot.caption  = element_text(hjust = 0.5, margin = margin(t = 10), size = 12)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_grey(start = 0.7, end = 0.3, name = "") +
  labs(caption = "")

# Save plot 1
#ggsave("../output/Barplot_dodge_DUAL_grey2.jpeg", width = 8, height = 6, dpi = 300)


# Visualization 2
Leb_tidied %>%
  mutate(Party_type = factor(Party_type, levels = c("Hezbollah", "Amal", "FPM", "PSP", "FM", "Others", "Kataeb", "LF"))) %>%
  ggplot() +
  geom_bar(data = . %>% filter(!is.na(Party_type) & !is.na(Dual_Eng)),
           aes(x = Party_type, fill = Dual_Eng),
           position = "fill") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), 
        legend.text = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 10))) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "") +
  scale_fill_grey(start = 0.7, end = 0.3,
                  labels = c("1" = "Dual Engagement", "0" = "Others")) +
  coord_flip() +
  labs(caption = "", caption.text = element_text(size = 10)) +
  guides(fill = guide_legend(title = "")) +
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10), size = 12))

# Save plot 2
# ggsave("../output/Barplot_fill_DUAL_grey2.jpeg", width = 8, height = 6, dpi = 300)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------PART 3: MULTIVARIATE ANALYSIS ---------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############## Theoretical Clusters #################

# 1 - Cluster CLASS
#Eco_diff + Employment

# 2 - Cluster POLITICAL COUSCIOUSNESS
#Pol_Interest + Reform

# 3 - Cluster SOCIAL CAPITAL
#Trust_Institutions + Voters + Gov_satisf

# 4 - Cluster POLITICAL EFFICACY
#Gov_respons + infl_gov

# 5 - Cluster GRD
# Redistribution_past

# 6 - Cluster SPECIFIC
#Religious + Corr_national + Sect

# CONTROLS
#Male + Edu + Age + Urban + Internet_user


########### Converting categorical variables in factors and setting a reference value ##############
Leb_tidied$Edu <- factor(Leb_tidied$Edu)
Leb_tidied$Edu <- relevel(Leb_tidied$Edu, ref = "Med_Edu")

Leb_tidied$Employment <- factor(Leb_tidied$Employment)
Leb_tidied$Employment <- relevel(Leb_tidied$Employment, ref = "Employed")

Leb_tidied$Sect <- factor(Leb_tidied$Sect)
Leb_tidied$Sect <- relevel(Leb_tidied$Sect, ref = "Christian")


################################ Logit for dual engagement ##############################################

###### Cluster CLASS #######
Dual_model1 <- glm(Dual_Eng ~ Eco_diff + Employment + Religious + Corr_national +  Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model1)

###### Cluster POLITICAL COUSCIOUSNESS #######
Dual_model2 <- glm(Dual_Eng ~ Pol_Interest + Reform + Religious + Corr_national +  Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model2)

###### Cluster SOCIAL CAPITAL #######
Dual_model3 <- glm(Dual_Eng ~ Trust_Instit + Voters + Gov_satisf + Religious + Corr_national +  Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model3)

###### Cluster POLITICAL EFFICACY #######
Dual_model4 <- glm(Dual_Eng ~ Gov_respons + infl_gov + Religious + Corr_national +  Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model4)

###### Cluster RELATIVE DEPRIVATION #######
Dual_model5 <- glm(Dual_Eng ~ Redistribution_past + Religious + Corr_national +  Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model5)

###### Sectarian Identity #######
Dual_model6 <- glm(Dual_Eng ~ Sect + Male + Edu + Age + Urban + Religious + Corr_national + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model6)

###### FULL MODEL #######
Dual_model7 <- glm(Dual_Eng ~ Eco_diff + Employment + Pol_Interest + Reform + Trust_Instit + Voters + Gov_satisf +
                        Gov_respons + infl_gov + Redistribution_past + Sect +
                        Male + Edu + Age + Urban + Religious + Corr_national + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Dual_model7) 


### Regression table in Markdown ###
models <- list(
  "Social Class and Material Drivers" = Dual_model1,
  "Political and Cultural Awareness" = Dual_model2,
  "Social Capital" = Dual_model3,
  "Political Efficacy" = Dual_model4,
  "GRD" = Dual_model5,
  "MENA Predictors" = Dual_model6,
  "Full Model" = Dual_model7
)

coef_order_names <- c("Eco_diff" = "Economic difficulties",
                      "EmploymentStudent" = "Student",
                      "EmploymentUnemployed" = "Unemployed",
                      "Pol_Interest" = "Political Interest",
                      "Reform" = "Introducing Reforms",
                      "Trust_Instit" = "Trust in Institutions",
                      "Voters" = "Voting",
                      "Gov_satisf" = "Satisfaction with Gov.",
                      "Gov_respons" = "Gov. Responsiveness",
                      "infl_gov" = "Influence the Gov.",
                      "Redistribution_past" = "Redistribution 2021vs2020",
                      "Religious" = "Religiousness",
                      "Corr_national" = "Perception of National Corruption",
                      "SectMus-Other" = "Muslim-Other",
                      "SectMus-Shia" = "Muslim-Shia",
                      "SectMus-Sunni" = "Muslim-Sunni",
                      "(Intercept)" = "Constant")

caption = "Table 1 : Predictors of Dual Engagement"
reference = "Source : Arab Barometer, Wave 7 2021-2022"

controls_row <- data.frame(
  term = "Controls",
  "Social Class and Material Drivers" = "YES",
  "Political and Cultural Awareness" = "YES",
  "Social Capital" = "YES",
  "Political Efficacy" = "YES",
  "GRD" = "YES",
  "MENA Predictors" = "YES",
  "Full Model" = "YES"
)

attr(controls_row, 'position') <- 33

modelsummary(models, coef_map = coef_order_names,
             gof_omit = c("Deviance|DF|RMSE|AIC|BIC"),
             exponentiate = TRUE, # ODDS RATIO
             stars = TRUE,
             conf_level = .95,
             caption = caption,
             notes = c(reference, "McFadden Pseudo R2 = 0.18"),
             add_rows = controls_row,
             output = "markdown")


########### Logit for Party Type ###########

table(Leb_tidied$Party_type)
Leb_tidied$Party_type <- factor(Leb_tidied$Party_type)
Leb_tidied$Party_type <- relevel(Leb_tidied$Party_type, ref = "Hezbollah")

# Reg without controls
Party_type_mod1 <- glm(Dual_Eng ~ Party_type, na.action = na.omit, data = Leb_tidied, family = binomial(link = 'logit'))
summary(Party_type_mod1)

# Reg with controls
Party_type_mod2 <- glm(Dual_Eng ~ Party_type + Male + Age + Edu + Urban + Religious + Corr_national + Internet_user, na.action = na.omit, data = Leb_tidied, family = binomial(link = 'logit'))
summary(Party_type_mod2)

### Regression table in Markdown ###
models3 <- list(
  "Model 1" = Party_type_mod1,
  "Model 2" = Party_type_mod2
)

coef_order_names3 <- c("Party_typeAmal" = "Amal",
                       "Party_typeFM" = "FM",
                       "Party_typeFPM" = "FPM",
                       "Party_typeKataeb" = "Kataeb",
                       "Party_typeLF" = "LF",
                       "Party_typeOthers" = "Others",
                       "Party_typePSP" = "PSP",
                       "(Intercept)" = "Constant")

reference3 = "Source : Arab Barometer, Wave 7 2021-2022"

controls_row3 <- data.frame(
  term = "Controls",
  "Model 1" = "NO",
  "Model 2" = "YES"
)


attr(controls_row3, 'position') <- 15

modelsummary(models3, coef_map = coef_order_names3,
             gof_omit = c("Deviance|DF|RMSE"),
             exponentiate = TRUE, # ODDS RATIO
             stars = TRUE,
             caption = caption3,
             notes = c(reference3, "McFadden Pseudo R2 = 0.12"),
             add_rows = controls_row3,
             output = "markdown")



####################  Coefficient Plot  ##########################

Party_model7 <- glm(Party_Identification ~ Eco_diff + Employment + Pol_Interest + Reform + Trust_Instit + Voters + Gov_satisf +
                      Gov_respons + infl_gov + Redistribution_past + Sect +
                      Male + Edu + Age + Urban + Religious + Corr_national + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model7) 

Protest_model7 <- glm(Protest_Participation ~ Eco_diff + Employment + Pol_Interest + Reform + Trust_Instit + Voters + Gov_satisf +
                        Gov_respons + infl_gov + Redistribution_past + Sect +
                        Male + Edu + Age + Urban + + Religious + Corr_national + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model7) 


# Selecting only the main dual participation predictors
coefficients <- c("EmploymentStudent" = "Student", "Pol_Interest" = "Political Interest",
                  "Trust_Instit" = "Trust in Institutions", "Redistribution_past" = "Redistribution 2021vs2020",
                  "SectMus-Shia" = "Shia", "SectMus-Sunni" = "Sunni")

# Estract coef
coefs_Dual_model7 <- tidy(Dual_model7)
coefs_Party_model7 <- tidy(Party_model7)
coefs_Protest_model7 <- tidy(Protest_model7)

# Select coef
coefs_Dual_model7 <- coefs_Dual_model7[coefs_Dual_model7$term %in% names(coefficients), ]
coefs_Party_model7 <- coefs_Party_model7[coefs_Party_model7$term %in% names(coefficients), ]
coefs_Protest_model7 <- coefs_Protest_model7[coefs_Protest_model7$term %in% names(coefficients), ]

coefs_Dual_model7$model <- "Dual_model7"
coefs_Party_model7$model <- "Party_model7"
coefs_Protest_model7$model <- "Protest_model7"

all_coefs <- rbind(coefs_Dual_model7, coefs_Party_model7, coefs_Protest_model7)

all_coefs <- all_coefs %>%
  mutate(term = case_when(
    term == "EmploymentStudent" ~ "Student",
    term == "Pol_Interest" ~ "Political Interest",
    term == "Trust_Instit" ~ "Trust in Institutions",
    term == "Redistribution_past" ~ "Redistribution 2021vs2020",
    term == "SectMus-Shia" ~ "Shia",
    term == "SectMus-Sunni" ~ "Sunni",
    TRUE ~ as.character(term)
  ))

# Visualize
ggplot(all_coefs, aes(x = reorder(term, estimate), y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.2), size = 2.5) + # via di mezzo
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = position_dodge(width = 0.2), width = 0.06, size = 0.8) +
  labs(x = "", y = "", color = "Models", shape = "Models") +
  theme_minimal(base_size = 11) +
  coord_flip() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0.5, margin = margin(t = 10), size = 11),
    plot.margin = margin(10, 15, 10, 15)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_grey(start = 0.3, end = 0.7, 
                   labels = c("Dual Engagement", "Party Identification", "Protest Participation")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("Dual Engagement", "Party Identification", "Protest Participation")) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15)))) +
  labs(title = "", subtitle = "", caption = "")

# Save Coef Plot
#ggsave("../output/Comparative_Coefficient_plot_grey.jpeg", width = 8.5, height = 6.5, dpi = 300)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------- PART 4: PREDICTED PROBABILITIES ---------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Pred.Prob. for Sect ###

predict_reg3 <- glm(Dual_Eng ~ Sect + Male + Age + Edu + Urban + Religious + Corr_national + Internet_user, data = Leb_tidied, family = binomial(link = 'logit'))
summary(predict_reg3)

new_data3 <- expand.grid(
  Sect = factor(c("Christian", "Mus-Other", "Mus-Shia", "Mus-Sunni")),
  Male = c(0, 1),
  Age = c(0.0, 0.5, 1.0),
  Edu = factor(c("High_Edu", "Low_Edu", "Med_Edu")),
  Urban = c(0, 1),
  Religious = c(0, 0.5, 1),
  Corr_national = c(0, 0.33, 0.66, 1),
  Internet_user = c(0, 1)
)

predicted_probs3 <- predict(predict_reg3, newdata = new_data3, type = "response")

# Add the predicted probabilities to the data frame
new_data3$Predicted_Prob3 <- predicted_probs3

# Use 'aggregate' to calculate the mean predicted probability for each 'Sect' level
mean_predicted_probs3 <- aggregate(predicted_probs3 ~ Sect, data = new_data3, FUN = mean)

# Print the mean predicted probabilities for each 'Sect' level
print(mean_predicted_probs3)

# Convert Sect to a factor with the desired order for the x-axis
mean_predicted_probs3$Sect <- factor(mean_predicted_probs3$Sect, levels = c("Mus-Other", "Mus-Shia", "Mus-Sunni", "Christian"))

# Calculate standard error for each Sect
mean_predicted_probs3$se <- sqrt(mean_predicted_probs3$predicted_probs3 * (1 - mean_predicted_probs3$predicted_probs3) / nrow(new_data3))

ggplot(mean_predicted_probs3, aes(x = Sect, y = predicted_probs3)) +
  geom_point(size = 1.5, color = "black", fill = "black") +
  geom_errorbar(aes(ymin = predicted_probs3 - 1.96 * se, ymax = predicted_probs3 + 1.96 * se), 
                width = 0.1, color = "black") +
  geom_ribbon(aes(ymin = predicted_probs3 - 1.96 * se, ymax = predicted_probs3 + 1.96 * se),
              alpha = 0.3, fill = "black") +
  labs(title = "",
       x = "",
       y = "",
       caption = "") +
  ylim(0, 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        panel.grid.major = element_line(color = "gray", linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))) +
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10), size = 12))

# Save predict plot 1
# ggsave("../output/Predict_prob_plot_gray.jpeg", width = 8, height = 6, dpi = 300)   


### Pred.Prob. for Party Type ###

predict_reg4 <- glm(Dual_Eng ~ Party_type + Male + Age + Edu + Urban + Religious + Corr_national + Internet_user, na.action = na.omit, data = Leb_tidied, family = binomial(link = 'logit'))
summary(predict_reg4)


new_data4 <- expand.grid(
  Party_type = factor(c("Hezbollah", "Amal", "FPM", "PSP", "FM", "Kataeb", "LF", "Others")),
  Male = c(0, 1),
  Age = c(0.0, 0.5, 1.0),
  Edu = factor(c("High_Edu", "Low_Edu", "Med_Edu")),
  Urban = c(0, 1),
  Religious = c(0, 0.5, 1),
  Corr_national = c(0, 0.33, 0.66, 1),
  Internet_user = c(0, 1))

predicted_probs4 <- predict(predict_reg4, newdata = new_data4, type = "response")

# Add the predicted probabilities to the data frame
new_data4$Predicted_Prob4 <- predicted_probs4

# Use 'aggregate' to calculate the mean predicted probability for each 'Sect' level
mean_predicted_probs4 <- aggregate(Predicted_Prob4 ~ Party_type, data = new_data4, FUN = mean)

# Print the mean predicted probabilities for each 'Sect' level
print(mean_predicted_probs4)

# Convert Party_type to a factor with the desired order for the x-axis
mean_predicted_probs4$Party_type <- factor(mean_predicted_probs4$Party_type, levels = c("Hezbollah", "Amal", "FPM", "PSP", "FM", "Kataeb", "LF", "Others"))

# Calculate standard error for each Party_type
mean_predicted_probs4$se <- sqrt(mean_predicted_probs4$Predicted_Prob4 * (1 - mean_predicted_probs4$Predicted_Prob4) / nrow(new_data4))

# Create the plot
ggplot(mean_predicted_probs4, aes(x = Party_type, y = Predicted_Prob4)) +
  geom_point(size = 1.5, color = "black", fill = "black") +
  geom_errorbar(aes(ymin = Predicted_Prob4 - 1.96 * se, ymax = Predicted_Prob4 + 1.96 * se), 
                width = 0.1, color = "black") +
  geom_ribbon(aes(ymin = Predicted_Prob4 - 1.96 * se, ymax = Predicted_Prob4 + 1.96 * se),
              alpha = 0.3, fill = "black") +
  labs(title = "",
       x = "",
       y = "",
       caption = "") +
  ylim(0, 0.6) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        panel.grid.major = element_line(color = "gray", linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))) +
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10), size = 12))


# Save predict plot 2
# ggsave("../output/Predict_prob_plot2_gray.jpeg", width = 8, height = 6, dpi = 300)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------- PART 5: Robustness Check ----------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++

################## Wald Test and LLR Test ##################

######### Cluster 1 ######### 
which(names(coef(Dual_model7)) %in% c("Eco_diff", "Employment"))
wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = c(2,3,4,5,6,7)) # da vedere un attimo come gestire il fattore

######### Cluster 2 #########
which(names(coef(Dual_model7)) %in% c("Pol_Interest", "Reform"))
wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = c(8,9))

######### Cluster 3 #########
which(names(coef(Dual_model7)) %in% c("Trust_Instit", "Voters", "Gov_satisf"))
wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = c(10,11,12))

######### Cluster 4 #########
which(names(coef(Dual_model7)) %in% c("Gov_respons", "infl_gov"))
wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = c(13,14))

######### Cluster 5 #########
which(names(coef(Dual_model7)) %in% c("Redistribution_past"))
wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = c(15))

######### Cluster 6 #########
which(names(coef(Dual_model7)) %in% c("SectMus-Other"))
wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = c(16,17,18))


# Defining Clusters
clusters <- list(
  "Social Class and Material Drivers" = c("Eco_diff", "EmploymentHousewife", "EmploymentRetired", "EmploymentSelf-Employed", "EmploymentStudent", "EmploymentUnemployed"),
  "Post-Materialist Drivers" = c("Pol_Interest", "Reform"),
  "Social Capital" = c("Trust_Instit", "Voters", "Gov_satisf"),
  "Political Efficacy" = c("Gov_respons", "infl_gov"),
  "GDR" = "Redistribution_past",
  "Sectarian Identity" = c("SectMus-Other", "SectMus-Shia", "SectMus-Sunn"))

# Visulize all the tests
for(cluster_name in names(clusters)) {
  variables <- clusters[[cluster_name]]
  terms <- which(names(coef(Dual_model7)) %in% variables)
  wald_result <- wald.test(b = coef(Dual_model7), Sigma = vcov(Dual_model7), Terms = terms)
  cat("\nCluster:", cluster_name, "\n")
  print(wald_result)}


###### Pseudo R-squared for all models  ######
pR2(Dual_model1)
pR2(Dual_model2)
pR2(Dual_model3)
pR2(Dual_model4)
pR2(Dual_model5)
pR2(Dual_model6)
pR2(Dual_model7)

# AIC
AIC(Dual_model1, Dual_model2, Dual_model3, Dual_model4, Dual_model5, Dual_model6, Dual_model7)

# BIC
BIC(Dual_model1, Dual_model2, Dual_model3, Dual_model4, Dual_model5, Dual_model6, Dual_model7)


################## Multiple Imputation ##################

# Visualize missing data pattern
md.pattern(Leb_tidied)

# Multiple Imputations (x5)
imputed_data <- mice(Leb_tidied, m = 5, method = 'pmm', seed = 123)

# Analize imputed models
imputed_models <- with(imputed_data, glm(Dual_Eng ~ Eco_diff + Employment + Pol_Interest + Reform + 
                                           Trust_Instit + Voters + Gov_satisf + Gov_respons + infl_gov + 
                                           Redistribution_past + Sect + Male + Edu + Age + Urban + 
                                           Religious + Corr_national + Internet_user, family = binomial(link='logit')))

pooled_results <- pool(imputed_models)
summary(pooled_results)


####### Compare original with imputed full models #######

models <- list("Original Model" = Dual_model7, "Imputed Model" = pooled_results)

# Visualize
modelsummary(models, statistic = "({std.error}){stars}", stars = c('*' = .05, '**' = .01, '***' = .001))

# Visulize in Markdown
variable_labels <- c(
  "Eco_diff" = "Economic Hardship",
  "EmploymentStudent" = "Student",
  "EmploymentUnemployed" = "Unemployed",
  "Pol_Interest" = "Political Interest",
  "Reform" = "Introducing Reforms",
  "Trust_Instit" = "Trust in Institutions",
  "Voters" = "Voting",
  "Gov_satisf" = "Satisfaction with Gov.",
  "Gov_respons" = "Gov. Responsiveness (external pol.eff.)",
  "infl_gov" = "Influence the Gov. (internal pol.eff.)",
  "Redistribution_past" = "Redistribution 2021vs2020",
  "SectMus-Other" = "Muslim-Other",
  "SectMus-Shia" = "Muslim-Shia",
  "SectMus-Sunni" = "Muslim-Sunni",
  "(Intercept)" = "Constant"
)

controls_row <- data.frame(
  term = "Controls",
  "Original" = "YES",
  "Imputed" = "YES"
)


attr(controls_row, 'position') <- 29

modelsummary(models,
             output = "markdown",
             gof_omit = c("Log.Lik.|RMSE|AIC|BIC"),
             title = "Regression Table: Original Model vs. Imputed model",
             coef_map = variable_labels,
             statistic = "({std.error})",
             stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             add_rows = controls_row)



################## Separate Regressions for Party id. and Protest part. ##################

########### Party Identification ###########

###### Cluster CLASS #######
Party_model1 <- glm(Party_Identification ~ Eco_diff + Employment + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model1)

###### Cluster POLITICAL COUSCIOUSNESS #######
Party_model2 <- glm(Party_Identification ~ Pol_Interest + Reform + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model2)

###### Cluster SOCIAL CAPITAL #######
Party_model3 <- glm(Party_Identification ~ Trust_Instit + Voters + Gov_satisf + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model3)

###### Cluster POLITICAL EFFICACY #######
Party_model4 <- glm(Party_Identification ~ Gov_respons + infl_gov + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model4)

###### Cluster RELATIVE DEPRIVATION #######
Party_model5 <- glm(Party_Identification ~ Redistribution_past + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model5)

###### Cluster MENA #######
Party_model6 <- glm(Party_Identification ~ Religious + Corr_national + Sect + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model6)

###### Cluster TUTTE VI #######
Party_model7 <- glm(Party_Identification ~ Eco_diff + Employment + Pol_Interest + Reform + Trust_Instit + Voters + Gov_satisf +
                     Gov_respons + infl_gov + Redistribution_past + Religious + Corr_national + Sect +
                     Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Party_model7) 

# Visualize
models1 <- list(
  "Social Class and Material Drivers" = Party_model1,
  "Political and Cultural Awareness" = Party_model2,
  "Social Capital" = Party_model3,
  "Political Efficacy" = Party_model4,
  "GRD" = Party_model5,
  "MENA Predictors" = Party_model6,
  "Full Model" = Party_model7
)

coef_order_names1 <- c("Eco_diff" = "Economic difficulties",
                      "EmploymentStudent" = "Student",
                      "EmploymentUnemployed" = "Unemployed",
                      "Pol_Interest" = "Political Interest",
                      "Reform" = "Introducing Reforms",
                      "Trust_Instit" = "Trust in Institutions",
                      "Voters" = "Voting",
                      "Gov_satisf" = "Satisfaction with Gov.",
                      "Gov_respons" = "Gov. Responsiveness",
                      "infl_gov" = "Influence the Gov.",
                      "Redistribution_past" = "Redistribution 2021vs2020",
                      "Religious" = "Religiousness",
                      "Corr_national" = "Perception of National Corruption",
                      "SectMus-Other" = "Muslim-Other",
                      "SectMus-Shia" = "Muslim-Shia",
                      "SectMus-Sunni" = "Muslim-Sunni",
                      "(Intercept)" = "Constant")

caption1 = ""
reference1 = "Source : Arab Barometer, Wave 7 2021-2022"

controls_row1 <- data.frame(
  term = "Controls",
  "Social Class and Material Drivers" = "YES",
  "Political and Cultural Awareness" = "YES",
  "Social Capital" = "YES",
  "Political Efficacy" = "YES",
  "GRD" = "YES",
  "MENA Predictors" = "YES",
  "Full Model" = "YES"
)


attr(controls_row1, 'position') <- 33

modelsummary(models1, coef_map = coef_order_names1,
             gof_omit = c("Deviance|DF|Log.Lik.|RMSE"),
             stars = TRUE,
             caption = caption1,
             notes = c(reference1, "McFadden Pseudo R2 = 0.27"),
             add_rows = controls_row1,
             output = "markdown")



########### Protest Participation ###########

###### Cluster CLASSE #######
Protest_model1 <- glm(Protest_Participation ~ Eco_diff + Employment + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model1)

###### Cluster COSCENZA #######
Protest_model2 <- glm(Protest_Participation ~ Pol_Interest + Reform + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model2)

###### Cluster SOCIAL CAPITAL #######
Protest_model3 <- glm(Protest_Participation ~ Trust_Instit + Voters + Gov_satisf + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model3)

###### Cluster POLITICAL EFFICACY #######
Protest_model4 <- glm(Protest_Participation ~ Gov_respons + infl_gov + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model4)

###### Cluster RELATIVE DEPRIVATION #######
Protest_model5 <- glm(Protest_Participation ~ Redistribution_past + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model5)

###### Cluster MENA #######
Protest_model6 <- glm(Protest_Participation ~ Religious + Corr_national + Sect + Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model6)

###### FULL MODEL #######
Protest_model7 <- glm(Protest_Participation ~ Eco_diff + Employment + Pol_Interest + Reform + Trust_Instit + Voters + Gov_satisf +
                     Gov_respons + infl_gov + Redistribution_past + Religious + Corr_national + Sect +
                     Male + Edu + Age + Urban + Internet_user, data = Leb_tidied, family = binomial(link='logit'))
summary(Protest_model7) 

# Visualize
models2 <- list(
  "Social Class and Material Drivers" = Protest_model1,
  "Political and Cultural Awareness" = Protest_model2,
  "Social Capital" = Protest_model3,
  "Political Efficacy" = Protest_model4,
  "GRD" = Protest_model5,
  "MENA Predictors" = Protest_model6,
  "Full Model" = Protest_model7
)

coef_order_names2 <- c("Eco_diff" = "Economic difficulties",
                       "EmploymentStudent" = "Student",
                       "EmploymentUnemployed" = "Unemployed",
                       "Pol_Interest" = "Political Interest",
                       "Reform" = "Introducing Reforms",
                       "Trust_Instit" = "Trust in Institutions",
                       "Voters" = "Voting",
                       "Gov_satisf" = "Satisfaction with Gov.",
                       "Gov_respons" = "Gov. Responsiveness",
                       "infl_gov" = "Influence the Gov.",
                       "Redistribution_past" = "Redistribution 2021vs2020",
                       "Religious" = "Religiousness",
                       "Corr_national" = "Perception of National Corruption",
                       "SectMus-Other" = "Muslim-Other",
                       "SectMus-Shia" = "Muslim-Shia",
                       "SectMus-Sunni" = "Muslim-Sunni",
                       "(Intercept)" = "Constant")

caption2 = ""
reference2 = "Source : Arab Barometer, Wave 7 2021-2022"

controls_row2 <- data.frame(
  term = "Controls",
  "Social Class and Material Drivers" = "YES",
  "Political and Cultural Awareness" = "YES",
  "Social Capital" = "YES",
  "Political Efficacy" = "YES",
  "GRD" = "YES",
  "MENA Predictors" = "YES",
  "Full Model" = "YES"
)


attr(controls_row2, 'position') <- 33

modelsummary(models2, coef_map = coef_order_names2,
             gof_omit = c("Deviance|DF|Log.Lik.|RMSE"),
             stars = TRUE,
             caption = caption2,
             notes = c(reference2, "McFadden Pseudo R2 = 0.12"),
             add_rows = controls_row2,
             output = "markdown")

