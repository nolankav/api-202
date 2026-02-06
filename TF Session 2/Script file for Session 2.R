# API 202: TF Session 2
# N.M. Kavanagh
# February 6, 2026

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(dplyr)        # Analysis tools
library(tidyverse)    # Analysis tools
library(ggplot2)      # Graphing tools
library(scales)       # Graphing tools
library(modelsummary) # Table tools

##############################################################################
# Dataset preparation
##############################################################################

# Read in sample dataset
df <- read.csv("Sample dataset.csv")

# # Scale median income by $1000
# df$med_inc_000s <- df$median_income/1000
# 
# # Re-write sample dataset
# write.csv(df %>% select(-X), "Sample dataset.csv")

##############################################################################
# Graphical exploration
##############################################################################

# Graph median income and Trump support
plot_1 <- ggplot(df, aes(x=med_inc_000s, y=pc_trump)) +
  
  # Add scatterplot points
  geom_point(alpha=0.25) +
  
  # Labels of axes
  xlab("County median income ($1000s)\n") +
  ylab("\nCounty support for Trump") +
  
  # Add best fit line
  geom_smooth(method="lm", se=F, formula = y~x) +
  
  # Cosmetic changes
  theme_light() + theme(text = element_text(face="bold")) +
  scale_y_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(0,150),
                     labels = scales::dollar_format())

# Export figure
ggsave(plot=plot_1, file="Example graph 1.pdf",
       width=4, height=4, units='in', dpi=600)

##############################################################################
# Regression exploration
##############################################################################

# Estimate regression
# LHS = support for Trump
# RHS = median income
reg_1 <- lm(pc_trump ~ med_inc_000s, data=df)
summary(reg_1)

# Estimate long regression
# LHS = support for Trump
# RHS = median income AND graduation rate
reg_2 <- lm(pc_trump ~ med_inc_000s + pc_hs_grad, data=df)
summary(reg_2)

# Compile results into tables
modelsummary(list("Model 1" = reg_1,
                  "Model 2" = reg_2),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_rename = c("(Intercept)"  = "Intercept",
                             "med_inc_000s" = "County median income ($1000s)",
                             "pc_hs_grad"   = "County graduation rate"),
             statistic   = c("({std.error})", "P={p.value}"),
             fmt         = fmt_statistic("estimate" = 2, "std.error" = 2, "p.value" = 3))

##############################################################################
# Advanced: Residualized graphs
##############################################################################

# Residualize variables by graduation rate
# Note: This is advanced. For demonstration only!
reg_x <- lm(pc_trump     ~ pc_hs_grad, data=df)
reg_y <- lm(med_inc_000s ~ pc_hs_grad, data=df)
df$trump_resid   <- residuals(reg_x)
df$med_inc_resid <- residuals(reg_y)

# Graph median income and Trump support
# Residualized to control for graduation rates
plot_2 <- ggplot(df, aes(x=med_inc_resid, y=trump_resid)) +
  
  # Add scatterplot points
  geom_point(alpha=0.25) +
  
  # Labels of axes
  xlab("County median income ($1000s),\ncontrolling for graduation rates") +
  ylab("County support for Trump,\ncontrolling for graduation rates") +
  
  # Cosmetic changes
  theme_light() + theme(text = element_text(face="bold")) +
  scale_y_continuous(limits = c(-60,60),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits = c(-70,70),
                     labels = scales::dollar_format()) +
  
  # Extend line of best fit to 0
  geom_smooth(method="lm", se=F, formula = y~x, fullrange=T)

# Export figure
ggsave(plot=plot_2, file="Example graph 2.pdf",
       width=4, height=4, units='in', dpi=600)
