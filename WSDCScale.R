###
#  Goal: Quick script to analyse translation from WSDC to WUDC scale
#  Author: Floris Holstege
###


# Packes required for subsequent analysis. P_load ensures these will be installed and loaded. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl,
               ggplot2,
               dplyr,
               reshape2,
               data.table)

# read in the data
WUDC_2018 <- read_xlsx("DebateData.xlsx", sheet = 1)
WSDC_2020 <- read_xlsx("DebateData.xlsx", sheet = 2)


# clean the wudc data - remove missing speaks, irrelevant columns, and melt dataframe for histogram/subsequent analysis
WUDC_2018_cleaned <- WUDC_2018 %>% 
  select(-ENL, -ESL, -EFL, -Team, -`Team Points`, -`Speaker Points`, -AVG) %>%
  melt(id.vars = "Speaker") %>%
  filter(value != 0 & value != "-") %>%
  na.omit()
# change to numeric to prevent error in ggplot
WUDC_2018_cleaned$value <- as.numeric(WUDC_2018_cleaned$value)


# creates histogram - slight skew to the left in the normal distribution
ggplot(data = WUDC_2018_cleaned, aes(x = value)) +
  geom_histogram(binwidth = 1)

# clean the wsdc data - remove missing speaks, irrelevant columns, and melt dataframe for histogram/subsequent analysis
WSDC_2020_cleaned <- WSDC_2020 %>%
  select(-Rank, -Country, -`Language Status`, -Avg, -Stdev, -Num) %>%
  melt(id.vars = "Debater Name") %>%
  filter(value != 0 & value != "—") %>%
  na.omit() 
# change to numeric to prevent error in ggplot
WSDC_2020_cleaned$value <- as.numeric(WSDC_2020_cleaned$value)

# check distribution - this is for the top 200 speakers, looks  normal
ggplot(WSDC_2020_cleaned, aes(x=value)) + 
  geom_histogram(binwidth = 0.5)

# we need to adjust mean and standard deviation of the wsdc data, since its only the top 200 debaters 
# to do this, I assume the speaks in the remainder of the tournament are normally distributed

# determine how many speakers we approximately are missing 
nTeams_WSDC_2018 <- 68
avg_nDebaters <- 5
nDebaters <- avg_nDebaters * nTeams_WSDC_2018
nDebaters_tab <- length(unique(WSDC_2020_cleaned$`Debater Name`))

# fraction of the total tab we have
frac_tab <- nDebaters_tab / nDebaters
round(frac_tab,1)

# By how much to adjust the standard deviation?
set.seed(1)
vStandardNormal <- rnorm(10000,0,1)
vTopStandardNormal <- tail(sort(vStandardNormal), 10000 * round(frac_tab,1))

# we need to multiply sd of the top 60% speakers by approximately 1.5 
adjustmenFactor_wsdc_2020 <- round(sd(vStandardNormal)/sd(vTopStandardNormal),1)

# get adjusted sd for WSDC 2020
sd_wsdc_2020_top100 <- sd(WSDC_2020_cleaned$value)
sd_wsdc_2020_adj <- sd_wsdc_2020_top100 * adjustmenFactor_wsdc_2020

# get adjusted mean for wsdc 2020, based on adjusted sd 
wsdc_2020_meanDeduction <- ((mean(vStandardNormal) - mean(vTopStandardNormal))/sd_wsdc_2020_adj)*sd_wsdc_2020_adj
mean_wsdc_2020_adj <- mean(WSDC_2020_cleaned$value) + wsdc_2020_meanDeduction

# create speaker scales of equal length
scale_wsdc <- seq(60,80, by = 0.5)
scale_wudc <- seq(50, 90, by = 1)

# calculate for each speak the zscore
Zscore_WUDC <- (scale_wudc - mean(WUDC_2018_cleaned$value))/sd(WUDC_2018_cleaned$value)
Zscore_WSDC <- (scale_wsdc - mean_wsdc_2020_adj)/sd_wsdc_2020_adj

# create dataframe in which the two scales are compared, write to csv
df_compare <- data.frame(speaks_WSDC = scale_wsdc, speaks_WUDC = scale_wudc, Zscore_WSDC = round(Zscore_WSDC,1), Zscore_WUDC = round(Zscore_WUDC,1))
write.csv(df_compare, "Compare_WSDC_WUDC_Scales.csv")




