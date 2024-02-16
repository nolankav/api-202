# API 202: TF Session 4
# N.M. Kavanagh
# February 16, 2024

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(tidyverse)    # Analysis tools
library(lfe)          # Analysis tools
library(survey)       # Analysis tools
library(ggplot2)      # Graphing tools
library(scales)       # Graphing tools
library(arsenal)      # Table tools
library(usmap)        # Mapping tools
library(cdlTools)     # FIPS tools

##############################################################################
# Dataset preparation
##############################################################################

# Lists of expansion states
EXPAND_2014 <- c(4:6,8,9,15,17,19,21,24,26,27,32:35,38,39,41,44,53,54)
NON_EXPAND  <- c(1,12,13,20,28,29,37,40,45:48,55,56,16,31,49)

# Note: All data is taken from the BRFSS 2011-2019
# It has been loaded in a separate script file
# The code below cleans that data for use in class

# # Rename state variable
# df$state <- df$X_STATE
# 
# # Age (percent under 50)
# df <- df %>% mutate(
#   age = case_when(
#     X_AGEG5YR %in% c(1:6)  ~ 1,
#     X_AGEG5YR %in% c(7:13) ~ 0,
#   ))
# 
# # Gender (percent men)
# df <- df %>% mutate(
#   gender = case_when(
#     SEX == 1 | SEX1 == 1 ~ 1,
#     SEX == 2 | SEX1 == 2 ~ 0,
#   ))
# 
# # Race/ethnicity (percent white)
# df <- df %>% mutate(
#   race_eth = case_when(
#     X_RACEGR2 == 1        | X_RACEGR3 == 1        ~ 1,
#     X_RACEGR2 %in% c(2:5) | X_RACEGR3 %in% c(2:5) ~ 0,
#   ))
# 
# # Marriage (percent married)
# df <- df %>% mutate(
#   married = case_when(
#     MARITAL == 1        ~ 1,
#     MARITAL %in% c(2:6) ~ 0,
#   ))
# 
# # Education (percent college educated)
# df <- df %>% mutate(
#   education = case_when(
#     EDUCA %in% c(6)   ~ 1,
#     EDUCA %in% c(1:5) ~ 0,
#   ))
# 
# # Income (percent under $35,000)
# df <- df %>% mutate(
#   income = case_when(
#     INCOME2 %in% c(1:5) ~ 1,
#     INCOME2 %in% c(6:8) ~ 0,
#   ))
# 
# # Insurance status
# df <- df %>% mutate(
#   insurance = case_when(
#     HLTHPLN1 == 1 ~ 1,
#     HLTHPLN1 == 2 ~ 0,
#   ))
# 
# # Expansion status
# df <- df %>% mutate(
#   expansion = case_when(
#     state %in% EXPAND_2014 ~ 1,
#     state %in% NON_EXPAND  ~ 0,
#   ))
# 
# # Expansion periods
# df <- df %>% mutate(
#   post_2014 = case_when(
#     date <  "2014-01-01" ~ 0,
#     date >= "2014-06-01" ~ 1,
#   ))
# 
# # Complete cases for variables
# df <- df %>%
#   filter_at(vars(age, gender, race_eth, married, education,
#                  income, insurance, expansion, post_2014), all_vars(!is.na(.)))
# 
# # Export cleaned sample dataset
# df %>% select(state, year, age, gender, race_eth, married, education,
#                 income, insurance, expansion, post_2014) %>%
#   write.csv("Sample dataset.csv")

# Load in dataset
df <- read.csv("Sample dataset.csv")

##############################################################################
# Simulate a randomized trial
##############################################################################

# Set seed
set.seed(1234)

# Randomly select 1,000 uninsured per group
sample_1000 <- subset(df, insurance==0) %>% sample_n(10000)
sample_1000$group <- c(rep(1,5000), rep(2,5000))

# Characteristics: All insured vs. uninsured
summary(tableby(insurance ~ age + gender + race_eth + married + education + income,
                df, digits.pct=0), text=T)

# Characteristics: Randomized sample
summary(tableby(group ~ age + gender + race_eth + married + education + income,
                sample_1000, digits.pct=0), text=T)

##############################################################################
# Estimate difference-in-differences
##############################################################################

# Difference-in-differences regression
# This function works like lm()
# But it also lets us cluster our standard errors by state
did <- felm(insurance ~ expansion + post_2014 + expansion*post_2014
            | 0 | 0 | state, df)
summary(did); length(did$residuals)

##############################################################################
# Unadjusted plots
##############################################################################

# Define study designs
design <- svydesign(id=~state, weights=~1, data=df)

# Tables for insurance status
table <- svyby(~insurance, ~year + expansion,
               design, svymean, na.rm=TRUE, vartype="ci")

# Treat years as numeric
table$year <- as.numeric(table$year)

# Label expansion status
table <- table %>% mutate(
  expansion_lab = case_when(
    expansion == 1 ~ "Expanded in 2014",
    expansion == 0 ~ "Didn't expand by 2019",
  ))
table$expansion_lab <- factor(table$expansion_lab, levels=c("Expanded in 2014", "Didn't expand by 2019"))

# Unadjusted plot of insurance
plot <- ggplot(table, aes(x=year, y=insurance,
                          group = expansion_lab,
                          fill  = expansion_lab,
                          color = expansion_lab)) +
  geom_vline(xintercept=2013.5, color="black", linetype="dashed") +
  geom_point() + geom_line() +
  theme_test() +
  theme(legend.position    = "right",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25),
        panel.grid.minor.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year") + ylab("Percent with health insurance") +
  annotate("text", x=2014.15, y=0.775, label="Medicaid expansion",
           fontface=2, size=3, hjust=0, vjust=0.5) +
  scale_x_continuous(breaks = seq(2006, 2022, 2)) +
  scale_y_continuous(limits = c(0.7,1),
                     labels = scales::percent_format(accuracy=1),
                     breaks = seq(0,1,0.1),
                     minor_breaks=seq(0,1,0.05)) +
  scale_color_manual(values = c("grey10","grey60"), name="") +
  scale_fill_manual(values = c("grey10","grey60"), name="")

# Export figure
ggsave(plot=plot, file="Example graph.pdf",
       width=5, height=3, units='in', dpi=600)

##############################################################################
# Map of included states
##############################################################################

# Get list of all states
states_df <- rbind(data.frame(datasets::state.abb), "DC")
colnames(states_df) <- "name"

# Get state FIPS
states_df$fips <- fips(states_df$name, to="FIPS")

# Assign expansion states of states
states_df <- states_df %>% mutate(
  `Expansion status` = case_when(
    fips %in% EXPAND_2014 ~ "Expanded in 2014",
    fips %in% NON_EXPAND  ~ "Didn't expand by 2019",
    TRUE ~ "Not included"
  ))
states_df$`Expansion status` <- factor(states_df$`Expansion status`, levels=c("Expanded in 2014", "Didn't expand by 2019", "Not included"))

# Map for included states
map_medicaid <- plot_usmap(regions="states", data=states_df,
                           values="Expansion status", size=0.4) +
  theme(legend.position = "bottom",
        text = element_text(size=10, face="bold")) +
  scale_fill_manual(name = "", values = c("grey40","grey70","grey99"))

# Export figure
ggsave(plot=map_medicaid, file="Map of states.pdf",
       width=8, height=4, units='in', dpi=600)
