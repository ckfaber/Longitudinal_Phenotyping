## LP_Plot: A script for plotting and analyzing longitudinal phenotyping data in mice

# Written by Chelsea Faber
# Mirzadeh Lab, Barrow Neurological Institute

# kasper.chelsea@gmail.com

# ASSUMPTIONS:
# - "Key" sheet is filled out to unblind subjects
# - Food_refill_weight_g column is all NAs except for days when food was refilled for ALL animals
# - 

# to do: 
# - normality checks before stats
# - repeated measures anovas/stats annotations on ts plots
# - update user-inputs to include faceting and grouping variables 

## Load required packages -----------------------------------------------------

library(googlesheets4)
library(tidyverse)
library(lubridate)
library(broom)
library(ggpubr)
library(rstatix)

## Configure Google Sheets authentication -------------------------------------

gs4_auth(email = "mirzadehlab@gmail.com")

ss       <- ""
day0     <- "2023-01-26"
groupvar <- 'Treatment' # primary grouping variable for statistical tests
facetvar <- 'Sex' # primary faceting variable for visualization, but no statistical comparisons (as yet)
plt      <- 'Dark2' # default; palette for scale_brewer functions

plotvars <- c("Body_mass_g","Body_mass_pct","FI_g","FI_kcal","FI_g_cum","FI_kcal_cum", "Blood_glucose_mgdl")
NCDgcal  <- 3.35 #default conversion 
HFDgcal  <- 5.47 # default conversion
export   <- F
ftype    <- "pdf" # default file type for exporting plots
fpath    <- "C:/Users/cfaber/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/CLF/Projects/dtx/Tt01/Longitudinal_Phenotyping" # path to folder where plots should be saved

## Load spreadsheet -----------------------------------------------------------

ws       <- sheet_names(ss)
key      <- read_sheet(ss, sheet = "key")
df       <- read_sheet(ss)

## Clean ----------------------------------------------------------------------

# Index only columns that are not fully NAs
df       <- df[, colSums(is.na(df)) != nrow(df)]
key      <- key[, colSums(is.na(key)) != nrow(key)]

# Unblind by merging key sheet
df       <- left_join(df, key, by = "Subject")

# Clean up date-time, factorize variables
df <- df %>%
  mutate(Date = ymd(df$Date)) %>%
  mutate(across(c(Subject,Sex,Treatment,Diet), as.factor))

# Check whether food refill column meets requirements for automatic FI calculation
n <- n_distinct(df$Subject)
if ("Food_refill_weight_g" %in% colnames(df)) {
  if ( (nrow(df) - sum(is.na(df$Food_refill_weight_g))) %% n == 0 ) {
    
    go <- TRUE
    print("Everything seems to be in order, proceed!")
    
  } else {
    go <- FALSE
    print("Warning! It looks like the full cohort was not always refed on the same day - is this in error?")
  }
}

## Compute daily food intake, accounting for food refills ----------------------
if (!exists("day0")) {
  df <- df %>%
    group_by(Subject) %>%
    mutate(Day = as.integer(ceiling(difftime(Date, first(Date), units = "days"))), .before = Subject,
           Int = Day - lag(Day)) %>% 
    mutate(Food_refill_weight_g = lag(Food_refill_weight_g),
           FI_g = -(Food_weight_g - lag(Food_weight_g)) / Int,
           FI_g = ifelse(!is.na(Food_refill_weight_g), Food_refill_weight_g - Food_weight_g, FI_g)) %>%
    ungroup()
  
} else if (exists("day0") & go) {
  
  # Compute experimental day and interval between days, assign baseline vs post-baseline period
  df <- df %>%
    mutate(Day = as.integer(ceiling(difftime(Date, as.POSIXct(day0), units = "days"))), .before = Subject) %>% 
    group_by(Subject) %>% 
    mutate(Int = Day - lag(Day)) %>% # interval between days
    mutate(Food_refill_weight_g = lag(Food_refill_weight_g), # lag the refill column so it it's in same row as value to be subtracted
           FI_g = -(Food_weight_g - lag(Food_weight_g)) / Int,
           FI_g = ifelse(!is.na(Food_refill_weight_g), Food_refill_weight_g - Food_weight_g, FI_g)) %>%
    ungroup()
  
  # Extract pre-day0 into separate df, fill cumulative FI with NA
  baseline <- df %>% 
    filter(Day < -1) %>% 
    mutate(FI_g_cum = NA) %>%
    ungroup()
  
  # Extract post-day0 into separate df, cumulative FI calculations
  df <- df %>% 
    filter(Day >= -1) %>%
    group_by(Subject) %>%
    mutate(FI_g_cum = cumsum(FI_g)) %>%
    ungroup()

}

# Compute FI in kcal, % initial body weight
df <- df %>%
  group_by(Subject) %>% 
  mutate(FI_kcal = case_when(Diet == "NCD" ~ FI_g * NCDgcal,Diet == "HFD" ~ FI_g * HFDgcal),
         FI_kcal_cum = case_when(Diet == "NCD" ~ FI_g_cum * NCDgcal,Diet == "HFD" ~ FI_g_cum * HFDgcal)) %>%
  mutate(Body_mass_pct = Body_mass_g / Body_mass_g[Day == 0] * 100) %>%
  ungroup()

## Time-Series Plots ---------------------------------------------------------

# Create df with y-axis labels for plotvars
plotlabs <- tibble(plotvar = plotvars, 
                   label = c("Body Mass (g)", 
                             "Body Mass (% Initial)", 
                             "Food Intake (g)",
                             "Food Intake (kcal)",
                             "Cumulative Food Intake (g)",
                             "Cumulative Food Intake (kcal)",
                             "Blood Glucose (mg/dL)"))


# Check that all of plotvars is in column space of df, modify if not
if (!all(plotvars %in% colnames(df))) {
  idx = plotvars %in% colnames(df)
  plotvars <- plotvars[idx]
  rm(idx)
}

# Create ggplot function for time-series plots
LP_tsplot <- function(var) {
  
  ylab <- filter(plotlabs,plotvar == {{var}}) %>% pull()
  p    <- ggplot(df, mapping = aes(x = Day, 
                           y = .data[[var]], 
                           color = .data[[groupvar]], 
                           group = .data[[groupvar]], 
                           fill = .data[[groupvar]])) + 
    #geom_point(alpha=0.5, size = 1) + 
    geom_line(aes(group = Subject), alpha = 0.5, linetype = 5,linewidth = 1) +
    facet_grid(~ .data[[facetvar]]) +
    stat_summary(fun = "mean",geom = "line", linewidth = 1.5) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.5, linetype = 0) +
    scale_color_brewer(palette = plt) +
    scale_fill_brewer(palette = plt) +
    theme_classic() + 
    labs(y = ylab)
  
  if (grepl("cum",var)) {
    p <- p + xlim(0, NA)
  } else {
    p <- p + xlim(-1, NA)
  }
  return(p)
}

# Loop through plotvars
ts.plots     <- vector(mode = "list", length = length(plotvars)) # initialize empty list
for (i in 1:length(plotvars)) {
  
  var <- plotvars[[i]]
  ts.plots[[i]] <- LP_tsplot(var)
  names(ts.plots)[i] <- var
  print(ts.plots[[i]]) 
  
  if (export & exists("fpath")) {
    ggsave(paste(var,ftype,sep='.'), width=5,height=3,units="in",path = fpath)
  }
}

## Box plots - Pre- and Post-Intervention --------------------------------------

# Grab first and last values of plotvars in df for paired box plots
df_summary <- df %>% 
  group_by(Subject) %>% 
  summarize(across(all_of(plotvars), list(Pre = first, Post = last))) %>%
  select(!contains("cum"))%>% # don't need the cumulative ones
  pivot_longer(!Subject, 
               names_to = c("Measure","Period"), 
               names_pattern = "(.*)_(.*)") %>%
  left_join(key, by = "Subject") %>%
  mutate(Period = factor(Period,levels = c("Pre","Post"),ordered = TRUE))

bpvars <- plotvars[!str_detect(plotvars,"cum")]

for (i in 1:length(bpvars)) {
  var <- bpvars[[i]]
  ylab <- filter(plotlabs,plotvar == {{var}}) %>% pull()
  
  p <- ggplot(data = df_summary %>% filter(Measure == {{var}}), aes(x = Period, y = value)) +
    facet_grid(~.data[[facetvar]]) +
    geom_boxplot(aes(fill = .data[[groupvar]]), alpha = 0.5) + 
    geom_point(aes(color = .data[[groupvar]], 
                   fill = .data[[groupvar]],
                   group = .data[[groupvar]]),
               color = "black", 
               position = position_dodge(width = 0.75),shape = 21, show.legend = FALSE) + 
    scale_color_brewer(palette = plt) +
    scale_fill_brewer(palette = plt) +
    ylab(ylab) + 
    theme_classic()
  print(p)
  
}
            
## Quality control & summary statistics ----------------------------------------

sum_stats <- vector(mode = "list", length = length(plotvars))
for (i in 1:length(plotvars)) {
  var <- plotvars[i]
  sum_stats[[i]] <- df %>% 
    group_by(Day,.data[[facetvar]],.data[[groupvar]]) %>% 
    get_summary_stats(.data[[var]])
  names(sum_stats)[i] <- var
}

# Confirm initial body weights/FI are normally distributed and not significantly different between groups
# https://www.datanovia.com/en/lessons/t-test-in-r/

plotvar <- "Body_mass_g"

# Extract initial body masses
BM_init <- df %>%
  filter(Day == 0) %>%
  select(c(Subject,Body_mass_g, all_of(c(facetvar,groupvar,plotvars)))) %>%
  select(!ends_with("cum"))

# Print summary statistics - save to var for export?
# BM_init %>%
#   group_by(enquo(facetvar), enquo(groupvar)) %>% 
#   get_summary_stats(Body_mass_g) # 

# Identify outliers & check for normality


# T-test
BM_init_ttest <- BM_init %>%
  group_by(.data[[facetvar]]) %>% # group by variable that will be used to facet. .data[[]] subsets variable name within string (see https://ggplot2.tidyverse.org/reference/tidyeval.html)
  t_test(as.formula(paste(plotvar, "~", groupvar))) %>% # 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>%
  add_xy_position()

# Box plots

BM_init_boxplot <- ggplot(BM_init, aes(x = .data[[groupvar]],y = .data[[plotvar]])) + 
  facet_grid(~.data[[facetvar]]) +
  geom_boxplot(aes(fill = .data[[groupvar]]), alpha = 0.5) + 
  scale_fill_brewer(palette = plt) +
  geom_point(aes(color = .data[[groupvar]], fill = .data[[groupvar]]), color = "black", shape = 21, position = "jitter", show.legend = FALSE) + 
  stat_pvalue_manual(BM_init_ttest,
                     bracket.nudge.y = -0.5,
                     label = "{p.adj.signif}") +
  labs(x = "Treatment", y = "Initial Body Mass (g)", fill = "Treatment") + 
  theme_classic()
BM_init_boxplot
