## 

# to do: 
# - normality checks before stats
# - time-series plots and repeated measures anovas
# - update user-inputs to include faceting and grouping variables 

## Load required packages -----------------------------------------------------

library(googlesheets4)
library(tidyverse)
library(lubridate)
library(broom)

## Configure Google Sheets authentication -------------------------------------

gs4_auth(email = "mirzadehlab@gmail.com")

ss       <- "https://docs.google.com/spreadsheets/d/1EedHKx1zdw842Wk4JlJgbBCVntv6cZmWN7HlxTVbY1A/edit#gid=0"
day0     <- "2023-01-26"
plotvars <- c("Body_mass_g","Body_mass_pct","FI_g","FI_kcal","FI_g_cum","FI_kcal_cum", "Blood_glucose_mgdl")
facetvar <- "sex"
groupvar <- "treatment"

## Load spreadsheet -----------------------------------------------------------

ws       <- sheet_names(ss)
key      <- read_sheet(ss, sheet = "key")
df       <- read_sheet(ss)

## Clean ----------------------------------------------------------------------

# Index only columns that are not fully NAs
df       <- df[, colSums(is.na(df)) != nrow(df)]
key      <- key[, colSums(is.na(key)) != nrow(key)]

# Unblind by merging key sheet
df       <- left_join(df, key, by = "sbj")

# Clean up date-time, compute food intake
df %<>% 
  mutate(Date = ymd(df$Date)) %>%
  mutate(Time = format(as.POSIXct(df$Time), format = "%H:%M")) %>% 
  mutate(across(c(sbj,sex,treatment,Diet), as.factor)) %>%
  group_by(sbj) %>%
  mutate(FI_g = c(0,-diff(Food_weight_g))) %>%
  mutate(FI_kcal = case_when(
    Diet == "NCD" ~ FI_g * 3.35,
    Diet == "HFD" ~ FI_g * 5.47)) 

# Compute experimental day & cumulative food intake

# If "day0" does not exist, count up from first Date
if (!exists("day0")) {
  df %<>%
    mutate(Day = as.integer(ceiling(difftime(Date, first(Date), units = "days"))), .before = sbj) %>%
    mutate(FI_g_cum = cumsum(FI_g),
           FI_kcal_cum = cumsum(FI_kcal)) %>%
    ungroup()

# If "day0" exists, count up from user-defined "day0", fill pre-day0 values with NA.     
} else if (exists("day0")) {
  df %<>%
    mutate(Day = as.integer(ceiling(difftime(Date, as.POSIXct(day0), units = "days"))))
  
  # Extract pre-day0 into separate df, fill cumulative FI with NA
  pre      <- df %>% 
    filter(Day < 0) %>% 
    mutate(FI_g_cum = NA,
           FI_kcal_cum = NA) 
  
  # Extract post-day0 into separate df, cumulative FI calculations
  post     <- df %>% 
    filter(Day >= 0) %>%
    group_by(sbj) %>%
    mutate(FI_g_cum = cumsum(FI_g),
           FI_kcal_cum = cumsum(FI_kcal)) %>%
    ungroup()
  
  # Combine pre- and post-dfs
  df <- bind_rows(pre,post, .id = "period")
  rm(list = c("pre","post"))

}

# Compute % initial body weight
df %<>% 
  group_by(sbj) %>%
  mutate(Body_mass_pct = Body_mass_g / Body_mass_g[Day == 0] * 100) %>%
  ungroup()

# Check that all of plotvars is in column space of df, modify if not
if (!all(plotvars %in% colnames(df))) {
  idx = plotvars %in% colnames(df)
  plotvars <- plotvars[idx]
  rm(idx)
}

## Quality control --------------------------------------------------------
# Confirm initial body weights/FI are normally distributed and not significantly different between groups
# https://www.datanovia.com/en/lessons/t-test-in-r/

# Extract initial body masses
BW_init <- df %>%
  filter(Day == 0) %>%
  select(c(sbj,Body_mass_g, sex, treatment))

# Identify outliers & check for normality
BW_init %>% group_by(sex,treatment) %>% get_summary_stats(Body_mass_g)

# T-test
BW_init_ttest <- BW_init %>%
  group_by(sex) %>% # group by variable that will be used to facet
  t_test(Body_mass_g ~ treatment) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

BW_init_ttest <- BW_ttest %>% add_xy_position(x = "treatment")

BW_init_boxplot <- ggplot(BW_init, aes(x = treatment,y = Body_mass_g)) + 
  facet_grid(~sex) +
  geom_boxplot(aes(fill = treatment), alpha = 0.25) + 
  geom_point(aes(color = treatment, fill = treatment), color = "black", shape = 21, position = "jitter", show.legend = FALSE) + 
  stat_pvalue_manual(BW_init_ttest,
                     bracket.nudge.y = -0.5,
                     label = "{p.adj.signif}") +
  labs(x = "Treatment", y = "Initial Body Mass (g)", fill = "Treatment") + 
  theme_classic()
BW_init_boxplot

## Time-Series Plots ---------------------------------------------------------

# saving for reference: https://community.rstudio.com/t/using-stat-instead-of-dplyr-to-summarize-groups-in-a-ggplot/13916/2
ggplot(data = df, mapping = aes(x = Day, 
                                y = Body_mass_g, 
                                color = treatment, 
                                group = treatment, 
                                fill = treatment)) + 
  #geom_point(alpha=0.5, size = 1) + 
  geom_line(aes(group = sbj), alpha = 0.25, linewidth = 1) +
  facet_grid(~ sex) + 
  stat_summary(fun = "mean",geom = "line", size = 1.5) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.5, linetype = 0) +
  theme_classic() + 
  labs(y = "Body Mass (g)")

