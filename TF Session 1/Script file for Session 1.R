# API 202: TF Session 1
# N.M. Kavanagh
# January 26, 2024

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(dplyr)        # Analysis tools
library(tidyverse)    # Analysis tools
library(ggplot2)      # Graphing tools
library(usmap)        # Graphing tools
library(viridis)      # Graphing tools
library(tidycensus)   # Census tools

##############################################################################
# Dataset preparation
##############################################################################

# Read in election dataset
# Source: MIT Election Lab
elections <- read.csv("countypres_2000-2020.csv")

# Subset to 2020 results
elections <- subset(elections, year==2020)

# Subset to Trump
elections <- subset(elections, candidate=="DONALD J TRUMP")

# Sum all forms of vote
# Take one value for total votes
trump <- elections %>%
  group_by(county_fips) %>%
  summarise(trump_votes = sum(candidatevotes),
            all_votes   = first(totalvotes),
            state       = first(state_po))

# Compute percent support
trump$pc_trump <- trump$trump_votes/trump$all_votes*100

# Get ACS data on county-level characteristics
acs <- get_acs(geography = "county",
                 variables = c("DP05_0019P", "DP05_0024P", "DP05_0002P",
                               "DP05_0065P", "DP05_0071P", "DP02_0062P",
                               "DP03_0009P", "DP03_0062E", "DP03_0099P"),
                 year      = 2020,
                 survey    = "acs5")

# Treat FIPS as numeric
acs$county_fips <- as.numeric(acs$GEOID)

# Rename variables
acs <- acs %>% mutate(
  variable = case_when(
    
    # Demographics
    variable == "DP05_0019P" ~ "pc_under_18",
    variable == "DP05_0024P" ~ "pc_over_65",
    variable == "DP05_0002P" ~ "pc_male",
    variable == "DP05_0065P" ~ "pc_black",
    variable == "DP05_0071P" ~ "pc_latin",
    
    # Socioeconomics
    variable == "DP02_0062P" ~ "pc_hs_grad",
    variable == "DP03_0009P" ~ "unemploy_rate",
    variable == "DP03_0062"  ~ "median_income",
    variable == "DP03_0099P" ~ "pc_uninsured"
  ))

# Convert to wide dataset
acs_wide <- acs %>% select(county_fips, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

# Merge datasets
df <- left_join(trump, acs_wide, by=c("county_fips"))

# Eliminate unmatched counties
df <- df %>% filter_at(vars(pc_trump, pc_under_18), all_vars(!is.na(.)))

# Export dataframe
write.csv(df, "Sample dataset.csv")

##############################################################################
# Graphical exploration
##############################################################################

# Graph high school graduation and Trump support
plot_1 <- ggplot(df, aes(x=pc_hs_grad, y=pc_trump)) +
  
  # Add scatterplot points
  geom_point(alpha=0.25) +
  
  # Labels of axes
  xlab("County high school graduation rate") +
  ylab("County support for Trump") +
  
  # Cosmetic changes
  theme_light() + theme(text = element_text(face="bold")) +
  scale_y_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(0,60),
                     labels = function(x) paste0(x, "%"))

# Export figure
ggsave(plot=plot_1, file="Example graph 1.pdf",
       width=4, height=4, units='in', dpi=600)

# Graph high school graduation and Trump support
plot_2 <- ggplot(df, aes(x=pc_hs_grad, y=pc_trump)) +
  
  # Add scatterplot points
  geom_point(alpha=0.25) +
  
  # Labels of axes
  xlab("County high school graduation rate") +
  ylab("County support for Trump") +
  
  # Cosmetic changes
  theme_light() + theme(text = element_text(face="bold")) +
  scale_y_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(0,60),
                     labels = function(x) paste0(x, "%")) +
  
  # Add line of best fit
  geom_smooth(method="lm", se=F, formula = y~x)

# Export figure
ggsave(plot=plot_2, file="Example graph 2.pdf",
       width=4, height=4, units='in', dpi=600)

##############################################################################
# Regression exploration
##############################################################################

# Estimate regression
# LHS = support for Trump
# RHS = unemployment rate
reg_1 <- lm(pc_trump ~ pc_hs_grad, data=df)
summary(reg_1)
