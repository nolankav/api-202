# API 202: TF Session 3
# N.M. Kavanagh
# February 14, 2024

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

##############################################################################
# Explore pooled relationship
##############################################################################

# Graph graduation rate and Trump support
plot_1 <- ggplot(df, aes(x=pc_hs_grad, y=pc_trump)) +
  
  # Add scatterplot points
  geom_point(alpha=0.25) +
  
  # Labels of axes
  xlab("County high school graduation rate") +
  ylab("County support for Trump") +
  
  # Add best fit line
  geom_smooth(method="lm", se=F, formula = y~x) +
  
  # Cosmetic changes
  theme_light() + theme(text = element_text(face="bold")) +
  scale_y_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(40,100),
                     labels = function(x) paste0(x,"%"))

# Export figure
ggsave(plot=plot_1, file="Example graph 1.pdf",
       width=4, height=4, units='in', dpi=600)

# Estimate regression
reg_1 <- lm(pc_trump ~ pc_hs_grad, data=df)
summary(reg_1)

##############################################################################
# Explore interacted relationship
##############################################################################

# Generate dummy variable
# Majority-Black or Latin counties
df <- df %>% mutate(
  majority = case_when(
    pc_black >= 50 | pc_latin >= 50 ~ 1,
    TRUE ~ 0
  ))

# Generate labelled dummy variable
df <- df %>% mutate(
  majority_labeled = case_when(
    pc_black >= 50 | pc_latin >= 50 ~ "Majority-Black or Latin",
    TRUE ~ "Other counties"
  ))
df$majority_labeled <- factor(df$majority_labeled, levels=c("Other counties", "Majority-Black or Latin"))

# Graph graduation rate and Trump support
plot_2 <- ggplot(df, aes(x=pc_hs_grad, y=pc_trump,
                         group=majority_labeled, color=majority_labeled)) +
  
  # Add scatterplot points
  geom_point(alpha=0.25) +
  
  # Labels of axes
  xlab("County high school graduation rate") +
  ylab("County support for Trump") +
  
  # Add best fit line
  geom_smooth(method="lm", se=F, formula = y~x, color="black") +
  
  # Cosmetic changes
  theme_light() + theme(text = element_text(face="bold")) +
  scale_y_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(40,100),
                     labels = function(x) paste0(x,"%")) +
  
  # Split into facets
  facet_grid(.~majority_labeled) +
  theme(legend.position="none",
        strip.background=element_rect(fill = "black"))

# Export figure
ggsave(plot=plot_2, file="Example graph 2.pdf",
       width=7, height=4, units='in', dpi=600)

# Estimate interacted regression
reg_2 <- lm(pc_trump ~ pc_hs_grad + majority + pc_hs_grad*majority, data=df)
summary(reg_2)

# Compile results into tables
modelsummary(list("Model 1" = reg_1,
                  "Model 2" = reg_2),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_rename = c("(Intercept)"  = "Intercept",
                             "pc_hs_grad" = "County graduation rate",
                             "majority"   = "Majority-Black or Latin county",
                             "pc_hs_grad:majority" =
                               "Grad. rate * Majority-Black or Latin"),
             statistic   = c("({std.error})"),
             stars       = TRUE,
             fmt         = fmt_statistic("estimate" = 2, "std.error" = 2))
