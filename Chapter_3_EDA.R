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
library(GGally)
library(ROCR)

#### Load in environmental data ####

setwd("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/Daly_Env_Vars")

df <- read.csv("seasonal_predictors_silo.csv", stringsAsFactors = T)

df$total_discharge_DI <- df$total_discharge_DI / 1000

colnames(df)

df %>% arrange(df$average_height_DI)

summary(df$total_discharge_DI)
sd(df$total_discharge_DI)

eda_df <- df %>%
  dplyr::select(c(ami_standardised_brien, total_discharge_DI, average_height_DI, avg_SOI, TEK_inundation, minor_flood, mod_flood, major_flood))

correlation_matrix <- cor(eda_df)
#View(correlation_matrix)

GGally::ggpairs(data = eda_df, aes(alpha = 0.5, size = 0.5), 
                diag = list(continuous = wrap("barDiag", bins = 15, fill = "grey"))) +
  theme_bw()

# function for scatter plots with smoothed trend line
lower_plots <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = "black", shape = 1, size = 1, alpha = 1) +
    geom_smooth(method = "glm",...) 
} 

# working with a trick of global assignment
diag_plots <- function(data, mapping, ...) {
  # increase counter each run globally so outside the function as well and this does the trick!
  x <<- x + 1
  ggplot(data = data, mapping = mapping) +
    # choose color by counter and send bin width argument in
    geom_histogram(fill = "grey", colour = "black", ...)
} 

x <- 0

# pairs plot
cor_plot <- ggpairs(eda_df, 
        diag = list(continuous = wrap(diag_plots, bins = 10)),
        lower = list(continuous = wrap(lower_plots, color="red", se=FALSE))) +
  theme_bw(base_size = 12) + 
  theme_bw(base_size = 12) + 
  theme(plot.margin = unit(c(2.5,2.5,2.5,3.5), "lines")) 

emf("C:/Users/samue/Desktop/Honours/Chapter_3_Sawfish_rescue_modelling/correlation_plot.emf", width = 12, height = 12) 
print(cor_plot)
dev.off()


### What predictors do we want to visualise over the study period? 