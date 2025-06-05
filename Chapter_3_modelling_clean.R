#### Load Packages ####

library(dplyr)
library(tidyverse)
library(ggplot2)
library(stats)
library(jtools)
library(bbmle)
library(car)
library(interactions)
library(lsmeans)
library(ggpubr)
library(dplyr)
library(caret)
library(GGally)
library(DHARMa)
library(pwr)
library(WebPower)
library(simr)
library(splines)
library(brglm2)
library(brms)
library(mclust)
library(MASS)     
library(ggeffects)
library(ggplot2)
library(devEMF)
library(MuMIn)
library(jtools)
library(DescTools)

install.packages("ROCR")

library(ROCR)

#### Load in environmental data ####

setwd("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Daly_Env_Vars")

df <- read.csv("seasonal_predictors_silo.csv", stringsAsFactors = T)

df$total_discharge_DI <- df$total_discharge_DI / 1000
  
colnames(df)

#### Presence Models ####

m1 <- glm(pristis_presence ~ ami_standardised_brien, # Good fit, good resids 
          data = df, 
          family = "binomial")

m3 <- glm(pristis_presence ~ minor_flood, 
          data = df, 
          family = "binomial")

m4 <- glm(pristis_presence ~ mod_flood, 
          data = df, 
          family = "binomial")

m5 <- glm(pristis_presence ~ avg_SOI, 
           data = df, 
           family = "binomial")

m6 <- glm(pristis_presence ~ TEK_inundation, 
           data = df, 
           family = "binomial")

m7 <- glm(pristis_presence ~ total_discharge_DI,
           data = df, 
           family = "binomial")

m8 <- glm(pristis_presence ~ average_height_DI,
                  data = df, 
                  family = "binomial")

m10 <- glm(pristis_presence ~ average_MJO_amp, 
          data = df,
          family = "binomial")

m11 <- glm(pristis_presence ~ mode_MJO_phase, 
           data = df, 
           family = "binomial")

AICctab(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, quasi, mod_firth, sort=T, base=T, weights=TRUE) # AMI is the best


##### Look at top model results ####

summary(m8)
summary(m7)
summary(m6)
summary(m1)
summary(m3)

#### Check model residuals ####

## Model 8 Analysis

m8_resids <- residuals(m8, type = c("deviance"))
acf(m8_resids)

m8_resids <- simulateResiduals(m8)
plot(m8_resids) #good

summary(m8)

confint(m8)
confint.default(m8)

testResiduals(m8_resids)
testDispersion(m8_resids) ## data is underdispersed
testZeroInflation(m8_resids)

library(car)
influencePlot(m8)
cooksD <- cooks.distance(m8)
which(cooksD > 0.7)

library(sandwich)
library(lmtest)

coeftest(m8, vcov = vcovHC(m8, type = "HC0"))

# Create a prediction object
m8_prob <- predict(m8, type = "response")
m8_pred <- prediction(m8_prob, df$pristis_presence)
m8_perf <- performance(m8_pred, measure = "tpr", x.measure = "fpr") # Calculate AUC
plot(m8_perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

auc = performance(m8_pred, "auc"); auc@y.values # Print the AUC

summary(m8_prob)  # m8_prob <- predict(m8, type = "response")
table(Predicted = predict(m8, type = "response") > 0.5, Actual = df$pristis_presence)

#### Model 7####

m7_resids <- residuals(m7, type = c("deviance"))
acf(m7_resids) #good

m7_resids <- simulateResiduals(m7)
plot(m7_resids) #good 

summary(m7)
confint(m7)
confint.default(m7)

m7_prob <- predict(m7, type = "response")
m7_pred <- prediction(m7_prob, df$pristis_presence)
m7_perf <- performance(m7_pred, measure = "tpr", x.measure = "fpr") # Calculate AUC
plot(m7_perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

auc = performance(m7_pred, "auc"); auc@y.values # Print the AUC

summary(m7_prob)  # m8_prob <- predict(m8, type = "response")
table(Predicted = predict(m7, type = "response") > 0.5, Actual = df$pristis_presence)

## Model 6

m6_resids <- simulateResiduals(m6)
plot(m6_resids) # not good

summary(m6)
confint(m6)
confint.default(m6)

m6_prob <- predict(m6, type = "response")
m6_pred <- prediction(m6_prob, df$pristis_presence)
m6_perf <- performance(m6_pred, measure = "tpr", x.measure = "fpr") # Calculate AUC
plot(m6_perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

auc = performance(m6_pred, "auc"); auc@y.values # Print the AUC

summary(m6_prob)  # m8_prob <- predict(m8, type = "response")
table(Predicted = predict(m6, type = "response") > 0.5, Actual = df$pristis_presence)

## Model 1 

m1_resids <- simulateResiduals(m1)
plot(m1_resids) #good

summary(m1)
confint(m1)
confint.default(m1)

m1_prob <- predict(m1, type = "response")
m1_pred <- prediction(m1_prob, df$pristis_presence)
m1_perf <- performance(m1_pred, measure = "tpr", x.measure = "fpr") # Calculate AUC
plot(m1_perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

auc = performance(m1_pred, "auc"); auc@y.values # Print the AUC

summary(m1_prob)  # m8_prob <- predict(m8, type = "response")
table(Predicted = predict(m1, type = "response") > 0.5, Actual = df$pristis_presence)
## Model 3 

m3_resids <- simulateResiduals(m3)
plot(m3_resids) #good

summary(m3)
confint(m3)
confint.default(m3)

m3_prob <- predict(m3, type = "response")
m3_pred <- prediction(m3_prob, df$pristis_presence)
m3_perf <- performance(m3_pred, measure = "tpr", x.measure = "fpr") # Calculate AUC
plot(m3_perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

auc = performance(m3_pred, "auc"); auc@y.values # Print the AUC

summary(m1_prob)  # m8_prob <- predict(m8, type = "response")
table(Predicted = predict(m1, type = "response") > 0.5, Actual = df$pristis_presence)

# Pdeudo R2

PseudoR2(m8, which = "all")
PseudoR2(m7, which = "all")
PseudoR2(m6, which = "all")
PseudoR2(m1, which = "all")
PseudoR2(m3, which = "all")

##### Model prediction Model 8 ####

## Plot best fitting model 

Av_height_raw <- ggplot(data = df, aes(x = as.factor(pristis_presence), y = average_height_DI, fill = as.factor(pristis_presence))) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 5, alpha = 0.5) +
  scale_fill_manual(values = c("grey", "darkolivegreen3"))  +
  scale_y_continuous(limits = c(2,8), breaks = seq(0,8, by = 2)) +
  theme_bw() +
  xlab("Sawfish Presence") +
  ylab("Average Wet Season Stage Height (m)") +
  theme(legend.position = "none")

Av_height_raw

emf("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Riv_height_raw.emf", width = 10, height = 8)
print(Av_height_raw)
dev.off()

##### Generate smooth predictions ####
pred_df <- data.frame(
  average_height_DI = seq(min(df$average_height_DI), max(df$average_height_DI), length.out = 100)
)

# Predict on link scale
predictions <- predict(m8, newdata = pred_df, type = "link", se.fit = TRUE)

# Convert to probabilities and CIs
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)

pred_df$predicted_probabilities <- plogis(predictions$fit)
pred_df$lower_bound <- plogis(predictions$fit - z_value * predictions$se.fit)
pred_df$upper_bound <- plogis(predictions$fit + z_value * predictions$se.fit)


plot_m8 <- ggplot(df, aes(x = average_height_DI, 
                          y = pristis_presence)) +
  geom_ribbon(data = pred_df, 
              aes(x = average_height_DI, 
                  ymin = lower_bound, 
                  ymax = upper_bound), 
              inherit.aes = FALSE,
              fill = "grey", alpha = 0.3) +
  
  geom_line(data = pred_df, 
            aes(x = average_height_DI, 
                y = predicted_probabilities), 
            inherit.aes = FALSE,
            color = "black", size = 1) +
  geom_point(colour = "black", aes(fill = factor(pristis_presence)), 
             size = 5, shape = 21, alpha = 0.7) + 
  annotate("text", 
           x = 3, 
           y = 0.9, 
           label = annotation_text, 
           color = "black", 
           size = 3.5, 
           hjust = 0) +
  labs(
    x = "Average River Height (m)",
    y = "Probability of Sawfish Presence"
  ) +
  theme_bw() +
  scale_fill_manual(values = c("grey34", "darkolivegreen3"))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 15)),  
        axis.title.y = element_text(margin = margin(r = 15)))

print(plot_m8)

emf("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/logistic_regression.emf", width = 12, height = 10) 
print(plot_m8)
dev.off()

#### Firths Exact Logistic Regressions ####

library(logistf)

mod_exact <- logistf(pristis_presence ~ average_height_DI, data = df)

mod_exact2 <- logistf(pristis_presence ~ total_discharge_DI, data = df)

mod_exact3 <- logistf(pristis_presence ~ TEK_inundation, data = df)

mod_exact4 <- logistf(pristis_presence ~ ami_standardised_brien, data = df)

mod_exact5 <- logistf(pristis_presence ~ avg_SOI, data = df)

mod_exact6 <- logistf(pristis_presence ~ minor_flood, data = df)


AICc_logistf <- function(model) {
  logLik_val <- as.numeric(model$loglik["full"])
  k <- length(model$coefficients)
  n <- model$n
  aic <- -2 * logLik_val + 2 * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  return(aicc)
}

# Then compare:
A1 <- AICc_logistf(mod_exact)
A2 <- AICc_logistf(mod_exact2)
A3 <- AICc_logistf(mod_exact3)
A4 <- AICc_logistf(mod_exact4)
A5 <- AICc_logistf(mod_exact5)
A6 <- AICc_logistf(mod_exact6)

# Step 1: Calculate AICc values safely
aiccs <- c(
  mod1 = AICc_logistf(mod_exact),
  mod2 = AICc_logistf(mod_exact2),
  mod3 = AICc_logistf(mod_exact3), 
  mod4 = AICc_logistf(mod_exact4), 
  mod5 = AICc_logistf(mod_exact5), 
  mod6 = AICc_logistf(mod_exact6)
)

# Step 2: Remove models with NA AICc values (if any)
aiccs_clean <- aiccs[!is.na(aiccs)]

# Step 3: Calculate delta AICc
delta_aicc <- aiccs_clean - min(aiccs_clean)

# Step 4: Create a clean comparison table
aicc_table <- data.frame(
  Model = names(aiccs_clean),
  AICc = round(aiccs_clean, 2),
  Delta_AICc = round(delta_aicc, 2)
)

# Step 5: Sort table by Delta_AICc
aicc_table <- aicc_table[order(aicc_table$Delta_AICc), ]

# View results
print(aicc_table)

### Seperator 

## Summary model 2 

summary(mod_exact2)

pred_probs <- predict(mod_exact2, type = "response")

raw_resids <- df$pristis_presence - pred_probs
plot(pred_probs, raw_resids,
     xlab = "Fitted Probabilities",
     ylab = "Raw Residuals",
     main = "Residuals vs Fitted for logistf model")
abline(h = 0, col = "red", lty = 2)

## Summary model 1 

summary(mod_exact2)

mod_exact2$loglik

coef <- exp(mod_exact2$coefficients) # get the odds ratio - 1
LCI <- exp(mod_exact2$ci.lower) # lower CImo of odds ratio - 1
UCI <- exp(mod_exact2$ci.upper) # upper bound of odds ratio - 1

res <- data.frame(coef, LCI, UCI); res

## Mod 3 
summary(mod_exact3)

exp(0.08990273)
exp(0.01678929)
exp(0.2521655)

mod_exact3$loglik

## Mod 1 

summary(mod_exact)
mod_exact$loglik

exp(1.883915)
exp(0.3737111)
exp(12.026257)

citation("logistf")
citation("DHARMa")
citation("glmmTMB")
citation("stats")

# Note that there is no AIC for mod_exact as it does not use maximum likelihood

# WE calculate our own AIC then using teh log likelihood

#### Presence model - Exact model visualization ####

## CHECK THE FUCKING RESIDUALS ##

# Predicted probabilities

pred_probs <- predict(mod_exact2, type = "response")

# Raw residuals (observed - predicted)
resid_manual <- df$pristis_presence - pred_probs

# Plot basics - look good
plot(resid_manual, main = "Manual residuals: Observed - Predicted",
     ylab = "Residual", xlab = "Observation")
abline(h = 0, lty = 2)

# Residuals vs predicted probabilities

plot(pred_probs, resid_manual,
     xlab = "Predicted Probability",
     ylab = "Residual",
     main = "Residuals vs Predicted")
abline(h = 0, lty = 2)

## Check for temporal autocorrelation 

acf(resid_manual) # looks okay

### MAKE PREDICTIONS + VISUALISE ###

newdata <- data.frame(
  total_discharge_DI = seq(min(df$total_discharge_DI, na.rm = TRUE),
                          max(df$total_discharge_DI, na.rm = TRUE),
                          length.out = 1000)
)

# Linear predictor (logit scale) with standard errors
pred_link <- predict(mod_exact2, newdata = newdata, type = "link", se.fit = TRUE)

# Convert to probabilities
newdata$pred <- plogis(pred_link$fit)
newdata$lower <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
newdata$upper <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)

## Plot it ## 

plot_exact <- ggplot(newdata, aes(x = total_discharge_DI, 
                          y = pred)) +
  geom_ribbon(aes(x = total_discharge_DI,
                    ymin = lower, 
                  ymax = upper), 
              inherit.aes = FALSE,
              fill = "grey", 
              alpha = 0.3) +
  geom_line(color = "black", size = 1) +
  geom_point(data = df, aes(x = total_discharge_DI, y = pristis_presence, fill = factor(pristis_presence)), color = "black", size = 5, shape = 21, alpha = 0.7) +
  labs(
    x = "Total Discharge (ML)",
    y = "Predicted Probability of Pristis Presence",
  ) +
  theme_bw() +
  scale_fill_manual(values = c("grey34", "darkolivegreen3"))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 15)),  
        axis.title.y = element_text(margin = margin(r = 15)))

print(plot_exact)

emf("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/discharge_logistic_regression.emf", width = 12, height = 10) 
print(plot_exact)
dev.off()

##### Abundance Models #### 

require(pscl)
require(MASS)
require(boot)
library(glmmTMB)
library(DHARMa)


m1_negbin <- glm.nb(pristis_abund ~ ami_standardised_brien,
                    data = df)

m2_negbin <- glm.nb(pristis_abund ~ ami_standardised_sam,
                    data = df)

m3_negbin <- glm.nb(pristis_abund ~ total_discharge_DI,
                    data = df)

Z_inf <- glmmTMB(pristis_abund ~ ami_standardised_brien,
                 ziformula = ~1,
                 family = nbinom2,
                 data = df)

Z_inf2 <- glmmTMB(pristis_abund ~ ami_standardised_sam,
                  ziformula = ~1,
                  family = nbinom2,
                  data = df)

Z_inf3 <- glmmTMB(pristis_abund ~ total_discharge_DI,
                  ziformula = ~1,
                  family = nbinom2,
                  data = df)

Z_inf4 <- glmmTMB(pristis_abund ~ average_height_DI,
                   ziformula = ~1,
                   family = nbinom2,
                   data = df)

Z_inf5 <- glmmTMB(pristis_abund ~ minor_flood,
                   ziformula = ~1,
                   family = nbinom2,
                   data = df)

Z_inf6 <- glmmTMB(pristis_abund ~ mod_flood,
                   ziformula = ~1,
                   family = nbinom2,
                   data = df)

Z_inf7 <- glmmTMB(pristis_abund ~ major_flood,
                  ziformula = ~1,
                  family = nbinom2,
                  data = df)

Z_inf8 <- glmmTMB(pristis_abund ~ avg_SOI,
                  ziformula = ~1,
                  family = nbinom2,
                  data = df)

Z_inf9 <- glmmTMB(pristis_abund ~ TEK_inundation,
                  ziformula = ~1,
                  family = nbinom2,
                  data = df)

Z_inf9.1 <- zeroinfl(pristis_abund ~ TEK_inundation,
                   data = df, 
                   dist = "negbin")

Z_inf4.1 <- zeroinfl(pristis_abund ~ average_height_DI,
                     data = df, 
                     dist = "negbin")


AICctab(Z_inf, Z_inf2, Z_inf3, Z_inf4, Z_inf5, Z_inf6, Z_inf7, Z_inf8, Z_inf4.1, Z_inf9, Z_inf9.1, sort=T, base=T, weights=TRUE)

#### Check the model summaries ####

summary(Z_inf9)
summary(Z_inf9.1)# good 
summary(Z_inf4)
summary(Z_inf4.1)# good 
summary(Z_inf8) # good 
summary(Z_inf)


#### Check model residudals ####

Z9_resids <- simulateResiduals(Z_inf9)
plot(Z9_resids)

Z4_resids <- simulateResiduals(Z_inf4)
plot(Z4_resids)

Z8_resids <- simulateResiduals(Z_inf8)
plot(Z8_resids)



#### Visualising and predicting ####

#####
#### Zero Inflated ####

new_data <- data.frame(TEK_inundation = seq(min(df$TEK_inundation), 
                                            max(df$TEK_inundation), length.out = 100))

# Get predicted counts from the count model (non-zero)
new_data$predicted_count <- predict(Z_inf9.1, new_data, type = "response")


## 

# Get model matrix for the count model
X_count <- model.matrix(~ TEK_inundation, data = new_data)

# Extract coefficients for the count model
coef_count <- coef(Z_inf9.1)[1:length(coef(Z_inf9.1, model = "count"))]  # Ensure we only grab the count model coefficients

# Extract variance-covariance matrix for the count model
vcov_Z_inf <- vcov(Z_inf9.1)  # Full variance-covariance matrix
vcov_count <- vcov_Z_inf[names(coef_count), names(coef_count), drop = FALSE]  # Select only count model elements

# Predict on the link scale (log-scale since log link is used)
log_fit <- X_count %*% coef_count
log_se <- sqrt(diag(X_count %*% vcov_count %*% t(X_count)))

# Convert to response scale
new_data$predicted_count <- exp(log_fit)
new_data$lower_CI <- exp(log_fit - 1.96 * log_se)
new_data$upper_CI <- exp(log_fit + 1.96 * log_se)

new_data

Z_inf9.1

# Extract key model results
intercept <- round(coef(Z_inf9.1)["(Intercept)"], 2); intercept
slope <- round(coef(Z_inf9.1)["TEK_inundation"], 10); slope
theta <- round(Z_inf9.1$theta, 2); theta
p_value_slope <- summary(Z_inf9.1)$coefficients$count["TEK_inundation", "Pr(>|z|)"]
p_value_zero <- round(summary(Z_inf9.1)$coefficients$zero["TEK_inundation", "Pr(>|z|)"], 3)

p_value_slope
p_value_zero

coef(Z_inf)

summary(Z_inf9.1)

# Plot
z_inflate_plot <- ggplot() +
  geom_point(data = df, aes(x = TEK_inundation, y = pristis_abund, fill = pristis_abund), 
             shape = 21, colour = "black", size = 5) +
  geom_line(data = new_data, aes(x = TEK_inundation, y = predicted_count), 
            color = "black", size = 1) +
  geom_ribbon(data = new_data, aes(x = TEK_inundation, ymin = lower_CI, ymax = upper_CI), 
              alpha = 0.2, fill = "grey") +
  scale_fill_gradient(low = "darkgrey", high = "darkolivegreen2") +
  labs(x = "No. Days Stage Height > 6m", y = "Pristis Abundance", fill = "Pristis Presence") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 15), size = 15),
        axis.title.y = element_text(margin = margin(r = 15), size = 15)) +
  #annotate("text", x = 20, y = 69, label = paste0("Intercept: -0.82953"), hjust = 1, size = 4) +
  #annotate("text", x = 30, y = 65, label = paste0("Slope: 0.051171 (p = 2.54x10^-7*)"), hjust = 1, size = 4) +
 # annotate("text", x = 22, y = 61, label = paste0("Zero-inflation p: ", p_value_zero), hjust = 1, size = 4) +
 # annotate("text", x = 22, y = 58, label = paste0("Theta: ", theta), hjust = 1, size = 4)

print(z_inflate_plot)

emf("C:/Users/samue/Desktop/Honours/Daly_ENV/presence_boxplot.emf", width = 10, height = 8)  # Set the width and height in inches
print(box_plot)
dev.off()


