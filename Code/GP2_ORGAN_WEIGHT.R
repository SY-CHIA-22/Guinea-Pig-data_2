
############################################################
## Organ weights (g) — SMALL n, Sex pooled (recommended)
## Data format (LONG): diet, sex, replicate, organ, weight
## Diets: T1 (control), T2–T4
## Goal (high-impact, small n): Estimation + bootstrap uncertainty
##
## Outputs:
## 1) Tables:
##    - Summary means per diet/organ (n, mean, SD)
##    - Effect vs control (T1): Δg and %Δ with bootstrap 95% CI
## 2) Journal-ready figures:
##    - Fig 1: Raw points + mean ± bootstrap 95% CI (per organ)
##    - Fig 2: Forest plot of Δ vs T1 (per organ)
##
## Notes:
## - Sex is retained only as metadata (not used in models)
## - Robust to very small sample sizes (n=4 per diet when pooled)
############################################################

## ---- 0) Packages ----
req <- c("tidyverse","readr","readxl","janitor","scales")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(scales)

set.seed(123)  # reproducible bootstrap

## ---- 1) Load data ----
setwd("C:/Guinea-Pig-data_2/Data")
dat_raw <- read_excel("GP2_Organweight.xlsx") %>% clean_names()

## ---- 2) Validate + clean ----
needed <- c("diet","sex","replicate","organ","weight")
missing_cols <- setdiff(needed, names(dat_raw))
if(length(missing_cols) > 0){
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dat <- dat_raw %>%
  transmute(
    diet      = as.character(diet),
    sex       = case_when(
      str_to_lower(as.character(sex)) %in% c("m","male") ~ "M",
      str_to_lower(as.character(sex)) %in% c("f","female") ~ "F",
      TRUE ~ as.character(sex)
    ),
    replicate = as.character(replicate),
    organ     = str_squish(str_replace_all(as.character(organ), "_", " ")),
    weight_g  = suppressWarnings(as.numeric(weight))
  ) %>%
  filter(!is.na(diet), !is.na(organ), !is.na(weight_g)) %>%
  mutate(
    diet = factor(diet),
    sex  = factor(sex),
    organ = factor(organ)
  )

## Enforce diet order with T1 as control if present
diet_levels <- levels(dat$diet)
if("T1" %in% diet_levels){
  new_levels <- c("T1", setdiff(diet_levels, "T1"))
  dat <- dat %>% mutate(diet = factor(diet, levels = new_levels))
}

## Quick sample-size check (sex pooled)
cat("\nCounts by diet (sex pooled):\n")
print(dat %>% count(diet, name = "n") %>% arrange(diet))

cat("\nCounts by diet x sex (for reporting only):\n")
print(dat %>% count(diet, sex, name = "n") %>% arrange(diet, sex))

cat("\nCounts by organ x diet:\n")
print(dat %>% count(organ, diet, name = "n") %>% arrange(organ, diet))

## ---- 3) Bootstrap helpers ----
boot_ci_mean <- function(x, R = 10000, conf = 0.95){
  x <- x[is.finite(x)]
  n <- length(x)
  if(n < 2){
    return(tibble(mean = mean(x), lo = NA_real_, hi = NA_real_, n = n))
  }
  boots <- replicate(R, mean(sample(x, size = n, replace = TRUE)))
  alpha <- (1 - conf) / 2
  tibble(
    mean = mean(x),
    lo = unname(quantile(boots, probs = alpha, na.rm = TRUE)),
    hi = unname(quantile(boots, probs = 1 - alpha, na.rm = TRUE)),
    n = n
  )
}

boot_ci_diff_vs_control <- function(x_trt, x_ctl, R = 10000, conf = 0.95){
  x_trt <- x_trt[is.finite(x_trt)]
  x_ctl <- x_ctl[is.finite(x_ctl)]
  nt <- length(x_trt); nc <- length(x_ctl)
  if(nt < 2 || nc < 2){
    return(tibble(diff = mean(x_trt) - mean(x_ctl), lo = NA_real_, hi = NA_real_, nt = nt, nc = nc))
  }
  boots <- replicate(R, mean(sample(x_trt, nt, TRUE)) - mean(sample(x_ctl, nc, TRUE)))
  alpha <- (1 - conf) / 2
  tibble(
    diff = mean(x_trt) - mean(x_ctl),
    lo = unname(quantile(boots, probs = alpha, na.rm = TRUE)),
    hi = unname(quantile(boots, probs = 1 - alpha, na.rm = TRUE)),
    nt = nt, nc = nc
  )
}

## ---- 4) Summaries: means + bootstrap CI (per organ x diet) ----
sum_mean_ci <- dat %>%
  group_by(organ, diet) %>%
  summarise(
    tmp = list(boot_ci_mean(weight_g, R = 10000, conf = 0.95)),
    .groups = "drop"
  ) %>%
  unnest(tmp) %>%
  rename(mean_g = mean, ci_lo = lo, ci_hi = hi)

sum_sd <- dat %>%
  group_by(organ, diet) %>%
  summarise(
    sd_g = sd(weight_g, na.rm = TRUE),
    .groups = "drop"
  )

summary_table <- sum_mean_ci %>%
  left_join(sum_sd, by = c("organ","diet")) %>%
  arrange(organ, diet)

cat("\n=== Summary table (means ± bootstrap 95% CI) ===\n")
print(summary_table)


## ---- Bootstrap helper functions (MUST be defined first) ----

boot_ci_mean <- function(x, R = 10000, conf = 0.95){
  x <- x[is.finite(x)]
  n <- length(x)
  if(n < 2){
    return(tibble(mean = mean(x), lo = NA_real_, hi = NA_real_, n = n))
  }
  boots <- replicate(R, mean(sample(x, size = n, replace = TRUE)))
  alpha <- (1 - conf) / 2
  tibble(
    mean = mean(x),
    lo = unname(quantile(boots, probs = alpha, na.rm = TRUE)),
    hi = unname(quantile(boots, probs = 1 - alpha, na.rm = TRUE)),
    n = n
  )
}

boot_ci_diff_vs_control <- function(x_trt, x_ctl, R = 10000, conf = 0.95){
  x_trt <- x_trt[is.finite(x_trt)]
  x_ctl <- x_ctl[is.finite(x_ctl)]
  
  nt <- length(x_trt)
  nc <- length(x_ctl)
  
  if(nt < 2 || nc < 2){
    return(tibble(
      diff = mean(x_trt) - mean(x_ctl),
      lo = NA_real_, hi = NA_real_,
      nt = nt, nc = nc
    ))
  }
  
  boots <- replicate(
    R,
    mean(sample(x_trt, nt, replace = TRUE)) -
      mean(sample(x_ctl, nc, replace = TRUE))
  )
  
  alpha <- (1 - conf) / 2
  
  tibble(
    diff = mean(x_trt) - mean(x_ctl),
    lo = unname(quantile(boots, probs = alpha, na.rm = TRUE)),
    hi = unname(quantile(boots, probs = 1 - alpha, na.rm = TRUE)),
    nt = nt, nc = nc
  )
}


## ---- 5) Effects vs control (T1): Δg and %Δ with bootstrap 95% CI ----
if(!("T1" %in% levels(dat$diet))){
  stop("Diet level 'T1' (control) not found in diet column. Please ensure control is labeled T1.")
}

effects_vs_T1 <- dat %>%
  group_by(organ) %>%
  group_modify(~{
    df <- .x
    x_ctl <- df %>% filter(diet == "T1") %>% pull(weight_g)
    m_ctl <- mean(x_ctl, na.rm = TRUE)
    
    diets_other <- levels(df$diet)
    diets_other <- diets_other[diets_other != "T1"]
    
    out <- map_dfr(diets_other, function(d){
      x_trt <- df %>% filter(diet == d) %>% pull(weight_g)
      bt <- boot_ci_diff_vs_control(x_trt, x_ctl, R = 10000, conf = 0.95)
      
      # percent difference (relative to control mean); bootstrap CI for percent
      # derived from bootstrapped diffs / control mean (fixed) to keep stable at tiny n
      pct <- 100 * (bt$diff / m_ctl)
      pct_lo <- 100 * (bt$lo  / m_ctl)
      pct_hi <- 100 * (bt$hi  / m_ctl)
      
      tibble(
        diet = factor(d, levels = levels(df$diet)),
        control_mean_g = m_ctl,
        diff_g = bt$diff, diff_lo = bt$lo, diff_hi = bt$hi,
        pct_diff = pct, pct_lo = pct_lo, pct_hi = pct_hi,
        n_trt = bt$nt, n_ctl = bt$nc
      )
    })
    out
  }) %>%
  ungroup() %>%
  arrange(organ, diet)

cat("\n=== Effects vs T1 (Δg and %Δ with bootstrap 95% CI) ===\n")
print(effects_vs_T1)

## Optional: save tables
write_csv(summary_table, "Table_OrganWeights_SummaryMeans_CI.csv")
write_csv(effects_vs_T1, "Table_OrganWeights_Effects_vs_T1.csv")

## ---- 6) Journal-ready theme ----
theme_journal <- theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

## ---- 7) FIGURE 1: Raw points + mean ± bootstrap 95% CI (per organ) ----
p1 <- ggplot() +
  geom_point(
    data = dat,
    aes(x = diet, y = weight_g),
    position = position_jitter(width = 0.12, height = 0),
    alpha = 0.75,
    size = 2.1
  ) +
  geom_linerange(
    data = summary_table,
    aes(x = diet, ymin = ci_lo, ymax = ci_hi),
    linewidth = 0.9
  ) +
  geom_point(
    data = summary_table,
    aes(x = diet, y = mean_g),
    size = 3.1
  ) +
  facet_wrap(~ organ, scales = "free_y", ncol = 3) +
  labs(
    x = "Diet (T1 = control)",
    y = "Organ weight (g)",
    title = "Internal organ weights by diet (sex pooled)",
    subtitle = "Points = individual animals; dots/lines = mean ± bootstrap 95% CI (estimation-focused)"
  ) +
  theme_journal

print(p1)

## ---- 8) FIGURE 2: Forest plot of Δ vs T1 (per organ) ----
## Put organs in a nice order (by mean control weight)
organ_order <- dat %>%
  filter(diet == "T1") %>%
  group_by(organ) %>%
  summarise(m = mean(weight_g, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(m)) %>%
  pull(organ) %>% as.character()

forest_df <- effects_vs_T1 %>%
  mutate(
    organ = factor(as.character(organ), levels = organ_order),
    diet  = factor(as.character(diet), levels = setdiff(levels(dat$diet), "T1"))
  )

p2 <- ggplot(forest_df, aes(y = organ, x = diff_g)) +
  geom_vline(xintercept = 0, linewidth = 0.7) +
  geom_linerange(aes(xmin = diff_lo, xmax = diff_hi), linewidth = 0.9) +
  geom_point(size = 2.8) +
  facet_wrap(~ diet, nrow = 1, scales = "free_x") +
  labs(
    x = expression(Delta~"Organ weight vs T1 (g)"),
    y = NULL,
    title = "Diet effects relative to control (T1)",
    subtitle = "Mean difference ± bootstrap 95% CI; values > 0 indicate heavier organs than T1"
  ) +
  theme_journal +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

print(p2)

## ---- 9) Save high-resolution outputs (optional) ----
ggsave("Fig1_OrganWeights_Estimation.tiff", p1, width = 10.5, height = 8.0, dpi = 600, compression = "lzw")
ggsave("Fig2_OrganWeights_Forest_DeltaVsT1.pdf", p2, width = 12.5, height = 4.8, dpi = 600)

cat("\nDone. Recommended reporting: emphasize effect sizes + bootstrap CI; treat any p-values (if added) as exploratory.\n")




## =========================================================
## Color upgrade (journal-friendly):
## - Raw points: neutral grey
## - Mean points: colored by Diet (T1–T4)
## - CIs: darker grey/black for clarity
## - Forest plot: points + CIs colored by Diet facet (optional)
## =========================================================

## ---- Define a clean, color-blind friendly palette (Okabe–Ito) ----
diet_pal <- c(
  "T1" = "#000000",  # control: black
  "T2" = "#0072B2",  # blue
  "T3" = "#009E73",  # green
  "T4" = "#D55E00"   # orange
)

## ---- 7) FIGURE 1: Raw points + colored MEANS ± bootstrap 95% CI ----
p1 <- ggplot() +
  # Raw individual animals (neutral)
  geom_point(
    data = dat,
    aes(x = diet, y = weight_g),
    position = position_jitter(width = 0.12, height = 0),
    alpha = 0.55,
    size = 2.0,
    colour = "grey35"
  ) +
  # Bootstrap CI (neutral but strong)
  geom_linerange(
    data = summary_table,
    aes(x = diet, ymin = ci_lo, ymax = ci_hi),
    linewidth = 0.9,
    colour = "grey10"
  ) +
  # Mean point (COLORED by Diet)
  geom_point(
    data = summary_table,
    aes(x = diet, y = mean_g, colour = diet),
    size = 3.6
  ) +
  # Optional: add a ring around means to pop on busy panels
  geom_point(
    data = summary_table,
    aes(x = diet, y = mean_g),
    size = 4.2,
    shape = 21,
    stroke = 0.9,
    fill = NA,
    colour = "white"
  ) +
  facet_wrap(~ organ, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = diet_pal, drop = FALSE) +
  labs(
    x = "Diet (T1 = control)",
    y = "Organ weight (g)",
    title = "Internal organ weights by diet (sex pooled)",
    subtitle = "Grey points = individuals; colored dots = mean; bars = bootstrap 95% CI"
  ) +
  theme_journal +
  theme(legend.position = "top")

print(p1)

## ---- 8) FIGURE 2: Forest plot of Δ vs T1 (color by Diet) ----
## Put organs in a nice order (by mean control weight)
organ_order <- dat %>%
  filter(diet == "T1") %>%
  group_by(organ) %>%
  summarise(m = mean(weight_g, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(m)) %>%
  pull(organ) %>% as.character()

forest_df <- effects_vs_T1 %>%
  mutate(
    organ = factor(as.character(organ), levels = organ_order),
    diet  = factor(as.character(diet), levels = setdiff(levels(dat$diet), "T1"))
  )

p2 <- ggplot(forest_df, aes(y = organ, x = diff_g, colour = diet)) +
  geom_vline(xintercept = 0, linewidth = 0.7, colour = "grey25") +
  geom_linerange(aes(xmin = diff_lo, xmax = diff_hi), linewidth = 0.95) +
  geom_point(size = 3.0) +
  facet_wrap(~ diet, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = diet_pal, drop = FALSE) +
  labs(
    x = expression(Delta~"Organ weight vs T1 (g)"),
    y = NULL,
    title = "Diet effects relative to control (T1)",
    subtitle = "Point = mean difference; line = bootstrap 95% CI"
  ) +
  theme_journal +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

print(p2)

## ---- 9) Save high-resolution outputs (optional) ----
ggsave("Fig1_OrganWeights_Estimation.tiff", p1, width = 10.5, height = 8.0, dpi = 600, compression = "lzw")
ggsave("Fig2_OrganWeights_Forest_DeltaVsT1.pdf", p2, width = 12.5, height = 4.8, dpi = 600)

ggsave("Fig1_OrganWeights_Estimation.pdf", p1, width = 10.5, height = 8.0, dpi = 600)
ggsave("Fig2_OrganWeights_Forest_DeltaVsT1.tiff", p2, width = 12.5, height = 4.8, dpi = 600, compression = "lzw")

cat("\nDone. Colored mean points (by diet) added; raw points kept neutral for readability.\n")





