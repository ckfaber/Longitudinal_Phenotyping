## 

# to do:
# - add BW as % initial (user-input which day)

## Load required packages -----------------------------------------------------

library(googlesheets4)
library(tidyverse)
library(lubridate)
library(broom)

## Configure Google Sheets authentication -------------------------------------

gs4_auth(email = "mirzadehlab@gmail.com")

ss       <- "https://docs.google.com/spreadsheets/d/1EedHKx1zdw842Wk4JlJgbBCVntv6cZmWN7HlxTVbY1A/edit#gid=0"
plotvars <- c("Body_mass_g","FI_g","FI_kcal","FI_g_cum","FI_kcal_cum", "Blood_glucose_mgdl")

## Load spreadsheet -----------------------------------------------------------
ws       <- sheet_names(ss)
df       <- read_sheet(ss)
key      <- read_sheet(ss, sheet = "key")

## Clean ----------------------------------------------------------------------

# Index only columns that are not fully NAs
df       <- df[, colSums(is.na(df)) != nrow(df)]
key      <- key[, colSums(is.na(key)) != nrow(key)]

# Unblind by merging groups from key sheet
df       <- left_join(df, key, by = "sbj")

# Clean up date-time, calculate experimental day
df %<>% 
  mutate(Date = ymd(df$Date)) %>%
  mutate(Time = format(as.POSIXct(df$Time), format = "%H:%M")) %>% 
  mutate(across(c(sbj,sex,treatment,Diet), as.factor)) %>%
  group_by(sbj) %>%
  mutate(Day = as.integer(difftime(Date, first(Date), units = "days")), .before = sbj) %>%
  mutate(FI_g = c(0,-diff(Food_weight_g))) %>%
  mutate(FI_kcal = case_when(
    Diet == "NCD" ~ FI_g * 3.35,
    Diet == "HFD" ~ FI_g * 5.47)) %>%
  mutate(FI_g_cum = cumsum(FI_g),
         FI_kcal_cum = cumsum(FI_kcal)) %>%
  ungroup() %>%
  select(Day : last_col())

# Check that all of plotvars is in column space of df, modify if not
if (!all(plotvars %in% colnames(df))) {
  idx <- plotvars %in% colnames(df)
  plotvars <- plotvars[idx]
}

## Summarize and Plot --------------------------------------------------------

# WIP

# test <- df %>%
#   group_by(sex,treatment) %>% 
#   summarize(
#     n = n(),
#     across(all_of(plotvars), list(mean = ~ mean(.x, na.rm = TRUE),
#                                   sd = ~ sd(.x, na.rm = TRUE),
#                                   sem = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
#            .names = "{.col}_{.fn}"))
# 
# 
# df_plot <- left_join(df,
#                      df %>%
#                        group_by(sex,treatment,Day) %>% 
#                        summarize(
#                          n = n(),
#                          across(all_of(plotvars), list(mean = mean,
#                                                        sd = sd,
#                                                        sem = ~ sd(.x) / sqrt(n())),
#                                 .names = "{.col}_{.fn}")))
## Plot
# saving for reference: https://community.rstudio.com/t/using-stat-instead-of-dplyr-to-summarize-groups-in-a-ggplot/13916/2
ggplot(data = df, mapping = aes(x = Day, y = Body_mass_g, color = treatment, group = treatment, fill = treatment)) + 
  #geom_point(alpha=0.5, size = 1) + 
  geom_line(aes(group = sbj), alpha = 0.25, size = 1) +
  facet_grid(~ sex) + 
  stat_summary(fun = "mean",geom = "line", size = 1.5) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.5, linetype = 0)

