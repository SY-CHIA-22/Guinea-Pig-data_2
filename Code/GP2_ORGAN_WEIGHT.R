############################################################
## Organ weight analysis (LONG format)
## Columns required: diet, sex, replicate, organ, weight
## Units: grams (g)
## Design: Diet (T1–T4) x Sex (F/M); 2F + 2M per diet
## Output:
##  - Per-organ inference: LM (Type III ANOVA) + diagnostics
##  - Automatic fallback to ART (nonparametric factorial ANOVA)
##  - Post-hoc: Diet within Sex (Tukey) + compact letter display
##  - Journal-ready plots:
##      (1) Raw points + model-based marginal means ± 95% CI + letters
##      (2) Distribution view (violin + box + points)
############################################################

## ---- 0) Packages ----
req <- c("tidyverse","readr","readxl","janitor","car","emmeans","multcompView","ARTool","broom")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(car)
library(emmeans)
library(multcompView)
library(ARTool)
library(broom)

## ---- 1) Load data ----
## Option A: CSV
## dat_raw <- read_csv("organ_weights.csv") %>% clean_names()
##
## Option B: Excel
## dat_raw <- read_excel("organ_weights.xlsx", sheet = 1) %>% clean_names()
##
## Option C: already in R
## dat_raw <- your_data %>% clean_names()

## ---------- EDIT THIS LINE ----------
dat_raw <- dat_raw %>% clean_names()
## -----------------------------------

## ---- 2) Validate & standardize ----
needed <- c("diet","sex","replicate","organ","weight")
missing_cols <- setdiff(needed, names(dat_raw))
if(length(missing_cols) > 0){
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dat <- dat_raw %>%
  transmute(
    diet      = as.factor(diet),
    sex       = case_when(
      str_to_lower(as.character(sex)) %in% c("m","male") ~ "M",
      str_to_lower(as.character(sex)) %in% c("f","female") ~ "F",
      TRUE ~ as.character(sex)
    ) %>% factor(levels = c("F","M")),
    replicate = as.factor(replicate),
    organ     = as.factor(str_squish(str_replace_all(as.character(organ), "_", " "))),
    weight_g  = suppressWarnings(as.numeric(weight))
  ) %>%
  filter(!is.na(diet), !is.na(sex), !is.na(replicate), !is.na(organ), !is.na(weight_g))

## Force Diet order if it looks like T1..T4
if(all(c("T1","T2","T3","T4") %in% unique(as.character(dat$diet)))){
  dat <- dat %>% mutate(diet = factor(diet, levels = c("T1","T2","T3","T4")))
}

## Basic check: expected counts by Diet x Sex
counts <- dat %>% count(diet, sex, name = "n_obs")
print(counts)

## ---- 3) Analysis helpers ----
check_assumptions <- function(df){
  # LM model for one organ
  fit <- lm(weight_g ~ diet * sex, data = df)
  
  shap_p <- tryCatch(shapiro.test(residuals(fit))$p.value, error = function(e) NA_real_)
  
  df2 <- df %>% mutate(group = interaction(diet, sex, drop = TRUE))
  lev_p <- tryCatch(car::leveneTest(weight_g ~ group, data = df2)[["Pr(>F)"]][1],
                    error = function(e) NA_real_)
  
  list(fit = fit, shapiro_p = shap_p, levene_p = lev_p)
}

analyze_one_organ <- function(df_one, organ_name, alpha = 0.05){
  # Choose LM if residuals ~normal AND homogeneous; otherwise ART
  diag <- check_assumptions(df_one)
  
  use_lm <- isTRUE(!is.na(diag$shapiro_p) && !is.na(diag$levene_p) &&
                     diag$shapiro_p >= alpha && diag$levene_p >= alpha)
  
  if(use_lm){
    model_type <- "LM (Type III ANOVA)"
    fit <- diag$fit
    
    anova_tab <- car::Anova(fit, type = 3) %>%
      broom::tidy() %>%
      mutate(organ = organ_name, model = model_type)
    
    # EMMs: Diet within each Sex
    emm <- emmeans(fit, ~ diet | sex)
    emm_tab <- broom::tidy(emm) %>%
      mutate(organ = organ_name, model = model_type)
    
    # Letters (Diet comparisons within Sex)
    cld_tab <- multcomp::cld(emm, Letters = letters, adjust = "tukey") %>%
      as.data.frame() %>%
      mutate(
        organ = organ_name,
        model = model_type,
        .group = str_squish(.group)
      ) %>%
      rename(letter = .group)
    
  } else {
    model_type <- "ART (nonparametric factorial ANOVA)"
    fit_art <- ARTool::art(weight_g ~ diet * sex, data = df_one)
    
    anova_tab <- anova(fit_art) %>%
      broom::tidy() %>%
      mutate(organ = organ_name, model = model_type)
    
    # EMMs from aligned model; artlm enables emmeans for interaction
    fit_artlm <- ARTool::artlm(fit_art, "diet:sex")
    emm <- emmeans(fit_artlm, ~ diet | sex)
    
    emm_tab <- broom::tidy(emm) %>%
      mutate(organ = organ_name, model = model_type)
    
    cld_tab <- multcomp::cld(emm, Letters = letters, adjust = "tukey") %>%
      as.data.frame() %>%
      mutate(
        organ = organ_name,
        model = model_type,
        .group = str_squish(.group)
      ) %>%
      rename(letter = .group)
  }
  
  diag_tab <- tibble(
    organ = organ_name,
    shapiro_p = diag$shapiro_p,
    levene_p  = diag$levene_p,
    chosen_model = ifelse(use_lm, "LM", "ART")
  )
  
  list(anova = anova_tab, emmeans = emm_tab, cld = cld_tab, diagnostics = diag_tab)
}

## ---- 4) Run per-organ analyses ----
organs <- levels(dat$organ)

res_list <- lapply(organs, function(o){
  analyze_one_organ(dat %>% filter(organ == o), organ_name = as.character(o), alpha = 0.05)
})

anova_all <- bind_rows(lapply(res_list, `[[`, "anova"))
emm_all   <- bind_rows(lapply(res_list, `[[`, "emmeans"))
cld_all   <- bind_rows(lapply(res_list, `[[`, "cld"))
diag_all  <- bind_rows(lapply(res_list, `[[`, "diagnostics"))

cat("\n=== Diagnostics (per organ) ===\n")
print(diag_all)

cat("\n=== ANOVA tables (per organ) ===\n")
print(anova_all %>% arrange(organ, term))

## Optional: save results
# write_csv(diag_all,  "results_diagnostics.csv")
# write_csv(anova_all, "results_anova_by_organ.csv")
# write_csv(emm_all,   "results_emmeans_by_organ.csv")
# write_csv(cld_all,   "results_letters_by_organ.csv")

## ---- 5) Prepare plotting tables ----
plot_raw <- dat %>%
  mutate(
    sex  = factor(sex, levels = c("F","M")),
    diet = factor(diet, levels = levels(dat$diet))
  )

plot_emm <- emm_all %>%
  mutate(
    sex  = factor(sex, levels = c("F","M")),
    diet = factor(diet, levels = levels(dat$diet))
  ) %>%
  left_join(
    cld_all %>% select(organ, sex, diet, letter) %>%
      mutate(
        sex  = factor(sex, levels = c("F","M")),
        diet = factor(diet, levels = levels(dat$diet))
      ),
    by = c("organ","sex","diet")
  ) %>%
  group_by(organ, sex) %>%
  mutate(
    # Put letters just above the upper CI
    letter_y = max(conf.high, na.rm = TRUE) + 0.05 * diff(range(c(conf.low, conf.high), na.rm = TRUE))
  ) %>%
  ungroup()

## ---- 6) Journal-ready theme ----
theme_journal <- theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95", colour = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

## ---- 7) FIGURE 1 (Main): Raw points + EMM ± 95% CI + letters ----
p_main <- ggplot() +
  geom_point(
    data = plot_raw,
    aes(x = diet, y = weight_g),
    position = position_jitter(width = 0.12, height = 0),
    alpha = 0.70,
    size = 2.1
  ) +
  geom_linerange(
    data = plot_emm,
    aes(x = diet, ymin = conf.low, ymax = conf.high),
    linewidth = 0.9
  ) +
  geom_point(
    data = plot_emm,
    aes(x = diet, y = estimate),
    size = 3.1
  ) +
  geom_text(
    data = plot_emm,
    aes(x = diet, y = letter_y, label = letter),
    fontface = "bold",
    size = 3.6,
    vjust = 0
  ) +
  facet_grid(sex ~ organ, scales = "free_y") +
  labs(
    x = "Diet (Treatment)",
    y = "Organ weight (g)",
    title = "Internal organ weights by diet and sex",
    subtitle = "Points = individual animals; dots/lines = marginal means ± 95% CI; letters = Tukey groups within each sex"
  ) +
  theme_journal

print(p_main)

## ---- 8) FIGURE 2 (Optional): Violin + box + points (raw distribution) ----
p_dist <- ggplot(plot_raw, aes(x = diet, y = weight_g)) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_boxplot(width = 0.22, outlier.shape = NA, linewidth = 0.55) +
  geom_point(position = position_jitter(width = 0.12, height = 0), alpha = 0.75, size = 2.0) +
  facet_grid(sex ~ organ, scales = "free_y") +
  labs(
    x = "Diet (Treatment)",
    y = "Organ weight (g)",
    title = "Organ weight distributions (raw data)"
  ) +
  theme_journal

print(p_dist)

## ---- 9) Save high-res (optional) ----
# ggsave("Fig1_OrganWeights_EMM_CI.tiff", p_main, width = 14, height = 7, dpi = 600, compression = "lzw")
# ggsave("Fig2_OrganWeights_Distribution.tiff", p_dist, width = 14, height = 7, dpi = 600, compression = "lzw")

############################################################
## Notes:
## 1) Your design is very small (n=2 per sex per diet), so
##    power is limited; the plots are often more informative.
## 2) The code compares Diet levels within each Sex (Tukey).
##    If you prefer comparing Sex within each Diet, I can
##    swap the emmeans formula to: ~ sex | diet
############################################################
