############################################################
# Economic evaluation (aggregated per replicate per diet)
# FLOWED EXACTLY as requested:
#   data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()
# then: types -> response_vars -> sanity check
# then: assumptions -> analysis -> plots -> tables
############################################################

# ---------------------------
# 0) Packages (install if missing)
# ---------------------------
pkgs <- c(
  "tidyverse","readxl","janitor","car","emmeans",
  "effectsize","performance","broom","gt","patchwork"
)

to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(readxl)
library(janitor)
library(car)
library(emmeans)
library(effectsize)
library(performance)
library(broom)
library(gt)
library(patchwork)

# ---------------------------
# 1) Read data (YOUR REQUIRED FLOW)
# ---------------------------
# If you want, set your working directory first:
setwd("C:/Guinea-Pig-data_2/Data")

data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()

# Use df as the main analysis object
df <- data_econs

# ---------------------------
# 2) Ensure correct types (YOUR REQUIRED FLOW)
# ---------------------------
df <- df %>%
  mutate(
    diet = as.factor(diet),
    replicate = as.factor(replicate)
  )

response_vars <- c("feed_consumed_kg","feed_cost_kg","sales_kg","gpm","cbr","roi")

# Basic sanity check (YOUR REQUIRED FLOW)
stopifnot(all(c("diet","replicate", response_vars) %in% names(df)))

# ---------------------------
# 3) Descriptive table (mean ± SD by diet)
# ---------------------------
desc_tbl <- df %>%
  pivot_longer(all_of(response_vars), names_to = "outcome", values_to = "value") %>%
  group_by(outcome, diet) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(`Mean ± SD` = sprintf("%.3f ± %.3f", mean, sd)) %>%
  select(outcome, diet, n, `Mean ± SD`)

desc_gt <- desc_tbl %>%
  gt(groupname_col = "outcome") %>%
  tab_header(title = "Descriptive statistics by diet (aggregated per replicate)") %>%
  cols_label(diet = "Diet", n = "n")

print(desc_gt)
gtsave(desc_gt, "Table_Descriptives.html")



# ---------------------------
# 4) Assumptions + analysis function (per outcome)  [FORCED OUTPUT]
# ---------------------------

analyze_outcome <- function(data, y, alpha = 0.05){
  
  # Hard checks (these will STOP with a message if something is wrong)
  if(!("diet" %in% names(data))) stop("Column 'diet' not found.")
  if(!(y %in% names(data))) stop(paste0("Outcome column '", y, "' not found."))
  if(length(unique(na.omit(data$diet))) < 2) stop("Need at least 2 diet levels in 'diet' for analysis.")
  
  # Fit base model
  m0 <- lm(reformulate("diet", response = y), data = data)
  
  # Assumptions
  shapiro_p <- tryCatch(shapiro.test(residuals(m0))$p.value, error = function(e) NA_real_)
  levene_p  <- tryCatch(car::leveneTest(data[[y]] ~ data$diet)$`Pr(>F)`[1], error = function(e) NA_real_)
  
  # Cook's distance
  cd <- cooks.distance(m0)
  n  <- nrow(model.frame(m0))
  cooks_cut <- 4/n
  cooks_flag_n <- sum(cd > cooks_cut, na.rm = TRUE)
  
  # Decide transformation
  all_pos <- all(data[[y]] > 0, na.rm = TRUE)
  need_fix <- (is.finite(shapiro_p) && shapiro_p < alpha) || (is.finite(levene_p) && levene_p < alpha)
  use_log <- isTRUE(all_pos) && isTRUE(need_fix)
  
  model_label <- "LM"
  m <- m0
  
  if(use_log){
    data2 <- data %>% mutate(.y_log = log(.data[[y]]))
    m <- lm(.y_log ~ diet, data = data2)
    model_label <- "LM (log-transformed)"
  }
  
  # ANOVA + effect size
  a <- car::Anova(m, type = 2)
  p_val <- as.numeric(a$`Pr(>F)`[1])
  
  es <- tryCatch(effectsize::eta_squared(m, partial = TRUE), error = function(e) NULL)
  eta2 <- if(!is.null(es)) as.numeric(es$Eta2[1]) else NA_real_
  
  # EMMs + Tukey
  emm <- emmeans(m, ~ diet)
  
  # For plots/tables:
  # - always compute emmeans on the model scale
  # - if log model: exponentiate EMMs and CIs for interpretation
  emm_raw <- as.data.frame(emmeans(m, ~ diet))
  
  if(use_log){
    emm_plot <- emm_raw %>%
      transmute(
        diet,
        emmean   = exp(emmean),
        lower.CL = exp(lower.CL),
        upper.CL = exp(upper.CL),
        scale    = "back-transformed"
      )
  } else {
    emm_plot <- emm_raw %>%
      mutate(scale = "original")
  }
  
  
  pair_df <- as.data.frame(pairs(emm, adjust = "tukey"))
  
  # Plot
  p <- ggplot() +
    geom_point(
      data = data,
      aes(x = diet, y = .data[[y]]),
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.75,
      size = 2
    ) +
    geom_pointrange(
      data = emm_plot,
      aes(x = diet, y = emmean, ymin = lower.CL, ymax = upper.CL),
      linewidth = 0.9
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Diet",
      y = y,
      title = paste0(y, " | ", model_label,
                     " | ANOVA p=", signif(p_val, 3),
                     " | eta²p=", ifelse(is.na(eta2), "NA", signif(eta2, 3)))
    )
  
  out <- list(
    outcome = y,
    model = m,
    model_label = model_label,
    assumptions = tibble(
      outcome = y,
      model = model_label,
      shapiro_p = shapiro_p,
      levene_p = levene_p,
      cooks_cutoff = cooks_cut,
      cooks_flagged_n = cooks_flag_n,
      anova_p = p_val,
      eta2_partial = eta2
    ),
    emmeans = emm_plot %>% select(diet, emmean, lower.CL, upper.CL, scale),
    pairwise = pair_df,
    plot = p
  )
  
  return(out)
}

# ---------------------------
# 4B) FORCE OUTPUT RIGHT NOW (this is the part many people miss)
# ---------------------------

# ---------------------------
# analyze_outcome()  [NO LOG-TRANSFORM VERSION]
# ---------------------------
analyze_outcome <- function(data, y, alpha = 0.05){
  
  # Checks
  if(!("diet" %in% names(data))) stop("Column 'diet' not found.")
  if(!(y %in% names(data))) stop(paste0("Outcome column '", y, "' not found."))
  if(length(unique(na.omit(data$diet))) < 2) stop("Need at least 2 diet levels in 'diet' for analysis.")
  
  # Base model (original scale)
  m <- lm(reformulate("diet", response = y), data = data)
  model_label <- "LM (original scale)"
  
  # Assumptions
  shapiro_p <- tryCatch(shapiro.test(residuals(m))$p.value, error = function(e) NA_real_)
  levene_p  <- tryCatch(car::leveneTest(data[[y]] ~ data$diet)$`Pr(>F)`[1], error = function(e) NA_real_)
  
  # Influence
  cd <- cooks.distance(m)
  n  <- nrow(model.frame(m))
  cooks_cut <- 4/n
  cooks_flag_n <- sum(cd > cooks_cut, na.rm = TRUE)
  
  # ANOVA + effect size
  a <- car::Anova(m, type = 2)
  p_val <- as.numeric(a$`Pr(>F)`[1])
  
  es <- suppressMessages(tryCatch(effectsize::eta_squared(m, partial = TRUE), error = function(e) NULL))
  eta2 <- if(!is.null(es)) as.numeric(es$Eta2[1]) else NA_real_
  
  # EMMs + Tukey
  emm <- emmeans(m, ~ diet)
  emm_plot <- as.data.frame(emm) %>%
    mutate(scale = "original")
  
  pair_df <- as.data.frame(pairs(emm, adjust = "tukey"))
  
  # Plot: raw replicate points + model means ± 95% CI
  p <- ggplot() +
    geom_point(
      data = data,
      aes(x = diet, y = .data[[y]]),
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.75,
      size = 2
    ) +
    geom_pointrange(
      data = emm_plot,
      aes(x = diet, y = emmean, ymin = lower.CL, ymax = upper.CL),
      linewidth = 0.9
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Diet",
      y = y,
      title = paste0(
        y, " | ", model_label,
        " | ANOVA p=", signif(p_val, 3),
        " | eta²=", ifelse(is.na(eta2), "NA", signif(eta2, 3))
      )
    )
  
  list(
    outcome = y,
    model = m,
    model_label = model_label,
    assumptions = tibble(
      outcome = y,
      model = model_label,
      shapiro_p = shapiro_p,
      levene_p = levene_p,
      cooks_cutoff = cooks_cut,
      cooks_flagged_n = cooks_flag_n,
      anova_p = p_val,
      eta2 = eta2
    ),
    emmeans = emm_plot %>% select(diet, emmean, lower.CL, upper.CL, SE, df, scale),
    pairwise = pair_df,
    plot = p
  )
}

# Gross profit margin (gpm)
tmp1 <- analyze_outcome(df, "gpm")
print(tmp1$assumptions)
print(tmp1$emmeans)
print(tmp1$pairwise)
print(tmp1$plot)

# cbr
tmp2 <- analyze_outcome(df, "cbr")
print(tmp2$assumptions)
print(tmp2$emmeans)
print(tmp2$pairwise)
print(tmp2$plot)


# RoI
tmp3 <- analyze_outcome(df, "roi")
print(tmp3$assumptions)
print(tmp3$emmeans)
print(tmp3$pairwise)
print(tmp3$plot)

# feed_consumed_kg
tmp4 <- analyze_outcome(df, "feed_consumed_kg")
print(tmp4$assumptions)
print(tmp4$emmeans)
print(tmp4$pairwise)
print(tmp4$plot)


# feed_cost_kg
tmp5 <- analyze_outcome(df, "feed_cost_kg")
print(tmp5$assumptions)
print(tmp5$emmeans)
print(tmp5$pairwise)
print(tmp5$plot)


# sales_kg
tmp6 <- analyze_outcome(df, "sales_kg")
print(tmp6$assumptions)
print(tmp6$emmeans)
print(tmp6$pairwise)
print(tmp6$plot)


# ---------------------------
# 5) Run analysis for all outcomes
# ---------------------------
res <- setNames(lapply(response_vars, \(y) analyze_outcome(df, y)), response_vars)

# ---------------------------
# 6) Tables: assumptions, EMMs, pairwise Tukey
# ---------------------------
assum_tbl <- purrr::map_df(res, "assumptions")
assum_gt <- assum_tbl %>%
  gt() %>%
  tab_header(title = "Assumption checks & diet effect (per outcome)") %>%
  fmt_number(columns = c(shapiro_p, levene_p, anova_p, eta2_partial), decimals = 4)

print(assum_gt)
# gtsave(assum_gt, "Table_Assumptions_ANOVA.html")

emm_tbl <- purrr::map_df(names(res), function(y){
  res[[y]]$emmeans %>%
    mutate(outcome = y) %>%
    select(outcome, diet, emmean, lower.CL, upper.CL, scale)
})

emm_gt <- emm_tbl %>%
  gt(groupname_col = "outcome") %>%
  tab_header(title = "Estimated marginal means by diet (95% CI)") %>%
  fmt_number(columns = c(emmean, lower.CL, upper.CL), decimals = 3)

print(emm_gt)
# gtsave(emm_gt, "Table_EMMeans_95CI.html")

pair_tbl <- purrr::map_df(names(res), function(y){
  res[[y]]$pairwise %>%
    mutate(outcome = y) %>%
    select(outcome, contrast, estimate, SE, df, t.ratio, p.value)
})

pair_gt <- pair_tbl %>%
  gt(groupname_col = "outcome") %>%
  tab_header(title = "Pairwise diet comparisons (Tukey-adjusted)") %>%
  fmt_number(columns = c(estimate, SE, t.ratio, p.value), decimals = 4)

print(pair_gt)
# gtsave(pair_gt, "Table_Pairwise_Tukey.html")

# ---------------------------
# 7) Plots: individual + combined multi-panel
# ---------------------------
# Individual plots (save optional)
for(y in response_vars){
  print(res[[y]]$plot)
  # ggsave(paste0("Fig_", y, ".png"), res[[y]]$plot, width = 6.5, height = 4.5, dpi = 600)
}

# Combined figure (useful as supplementary)
combined <- wrap_plots(lapply(response_vars, \(y) res[[y]]$plot), ncol = 2)
print(combined)
# ggsave("Fig_All_Outcomes.png", combined, width = 10, height = 12, dpi = 600)

# ---------------------------
# 8) Optional: export CSVs
# ---------------------------
write_csv(assum_tbl, "Assumptions_ANOVA.csv")
write_csv(emm_tbl, "EMMeans_95CI.csv")
write_csv(pair_tbl, "Pairwise_Tukey.csv")

# ---------------------------
# 9) Optional: deep diagnostics for any one outcome
# ---------------------------
# performance::check_model(res[["gpm"]]$model)

############################################################
# End of script
############################################################




############################################################
# GP2 Economic evaluation - complete analysis script
# (No log-transform; ANOVA primary + robust sensitivity)
############################################################

# ---------------------------
# 0) Packages
# ---------------------------
pkgs <- c(
  "tidyverse","readxl","janitor","car","emmeans",
  "effectsize","broom","gt","patchwork","performance","WRS2"
)

to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(readxl)
library(janitor)
library(car)
library(emmeans)
library(effectsize)
library(broom)
library(gt)
library(patchwork)
library(performance)
library(WRS2)

# ---------------------------
# 1) Read data (keep your flow)
# ---------------------------
setwd("C:/Guinea-Pig-data_2/Data")
data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()
data_econs

# ---------------------------
# 2) Ensure correct types (keep your flow)
# ---------------------------
data_econs <- df %>%
  mutate(
    diet = as.factor(diet),
    replicate = as.factor(replicate)
  )

response_vars <- c("feed_consumed_kg","feed_cost_kg","sales_kg","gpm","cbr","roi")

stopifnot(all(c("diet","replicate", response_vars) %in% names(data_econs)))

# ---------------------------
# 3) Descriptive table (mean ± SD)
# ---------------------------
desc_tbl <- data_econs %>%
  pivot_longer(all_of(response_vars), names_to="outcome", values_to="value") %>%
  group_by(outcome, diet) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups="drop"
  ) %>%
  mutate(`Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd)) %>%
  select(outcome, diet, n, `Mean ± SD`)

desc_gt <- desc_tbl %>%
  gt(groupname_col="outcome") %>%
  tab_header(title="Descriptive statistics by diet (aggregated per replicate)")

print(desc_gt)
# gtsave(desc_gt, "Table_Descriptives.html")

# ---------------------------
# 4) Analysis function (NO log-transform)
# Primary: LM/ANOVA + EMM/Tukey + eta²
# Sensitivity: Welch ANOVA + Robust trimmed-means ANOVA (WRS2)
# ---------------------------
analyze_outcome <- function(data, y, alpha = 0.05){
  
  # Checks
  if(!("diet" %in% names(data))) stop("Column 'diet' not found.")
  if(!(y %in% names(data))) stop(paste0("Outcome column '", y, "' not found."))
  if(length(unique(na.omit(data$diet))) < 2) stop("Need at least 2 diet levels in 'diet' for analysis.")
  
  # Model (original scale)
  m <- lm(reformulate("diet", response = y), data = data)
  
  # Assumptions
  shapiro_p <- tryCatch(shapiro.test(residuals(m))$p.value, error=function(e) NA_real_)
  levene_p  <- tryCatch(car::leveneTest(data[[y]] ~ data$diet)$`Pr(>F)`[1], error=function(e) NA_real_)
  
  # Influence
  cd <- cooks.distance(m)
  n  <- nrow(model.frame(m))
  cooks_cut <- 4/n
  cooks_flag_n <- sum(cd > cooks_cut, na.rm = TRUE)
  
  # Primary ANOVA + effect size
  a <- car::Anova(m, type = 2)
  anova_p <- as.numeric(a$`Pr(>F)`[1])
  
  eta <- suppressMessages(tryCatch(effectsize::eta_squared(m, partial = TRUE), error=function(e) NULL))
  eta2 <- if(!is.null(eta)) as.numeric(eta$Eta2[1]) else NA_real_
  
  # EMMs + Tukey
  emm <- emmeans(m, ~ diet)
  emm_data_econs <- as.data.frame(emm)
  tukey_df <- as.data.frame(pairs(emm, adjust="tukey"))
  
  # Sensitivity 1: Welch ANOVA (handles heteroskedasticity; also robust-ish)
  welch <- oneway.test(reformulate("diet", response = y), data = data, var.equal = FALSE)
  welch_df <- tibble(
    statistic = unname(welch$statistic),
    df1 = unname(welch$parameter[1]),
    df2 = unname(welch$parameter[2]),
    p_value = unname(welch$p.value)
  )
  
  # Sensitivity 2: Robust trimmed-means ANOVA (recommended for non-normality)
  # WRS2::t1way expects formula response ~ group
  robust <- WRS2::t1way(reformulate("diet", response = y), data = data)
  robust_data_econs <- tibble(
    statistic = robust$test,
    data_econs = robust$df,
    p_value = robust$p.value
  )
  
  # Plot: raw points + EMM mean ± 95% CI
  p <- ggplot() +
    geom_point(
      data = data,
      aes(x = diet, y = .data[[y]]),
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.75,
      size = 2
    ) +
    geom_pointrange(
      data = emm_df,
      aes(x = diet, y = emmean, ymin = lower.CL, ymax = upper.CL),
      linewidth = 0.9
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    labs(
      x = "Diet",
      y = y,
      title = paste0(
        y,
        " | ANOVA p=", signif(anova_p, 3),
        " | Welch p=", signif(welch_data_econs$p_value, 3),
        " | Robust p=", signif(robust_data_econs$p_value, 3)
      )
    )
  
  list(
    outcome = y,
    model = m,
    assumptions = tibble(
      outcome = y,
      shapiro_p = shapiro_p,
      levene_p = levene_p,
      cooks_cutoff = cooks_cut,
      cooks_flagged_n = cooks_flag_n
    ),
    primary = tibble(
      outcome = y,
      anova_p = anova_p,
      eta2 = eta2
    ),
    emmeans = emm_data_econs %>% select(diet, emmean, lower.CL, upper.CL, SE, data_econs),
    tukey = tukey_data_econs,
    welch = welch_data_econs %>% mutate(outcome = y),
    robust = robust_data_econs %>% mutate(outcome = y),
    plot = p
  )
}

# ---------------------------
# 5) Run all outcomes
# ---------------------------
res <- setNames(lapply(response_vars, \(y) analyze_outcome(data_econs, y)), response_vars)

# ---------------------------
# 6) Combine tables  [FIXED: rename df columns to avoid gt conflict]
# ---------------------------
assum_tbl   <- purrr::map_data_econs(res, "assumptions")
primary_tbl <- purrr::map_data_econs(res, "primary")

emm_tbl <- purrr::map_data_econs(names(res), \(y){
  res[[y]]$emmeans %>%
    mutate(outcome = y) %>%
    rename(data_econs_emm = data_econs)   # <<< FIX
})

tukey_tbl <- purrr::map_data_econs(names(res), \(y){
  res[[y]]$tukey %>%
    mutate(outcome = y) %>%
    rename(df_tukey = data_econs) # <<< FIX
})

welch_tbl  <- purrr::map_data_econs(res, "welch")
robust_tbl <- purrr::map_data_econs(res, "robust")

# ---- GT tables ----
assum_gt <- assum_tbl %>%
  gt() %>%
  tab_header(title="Assumptions check (per outcome)") %>%
  fmt_number(columns=c(shapiro_p, levene_p, cooks_cutoff), decimals=4)

primary_gt <- primary_tbl %>%
  gt() %>%
  tab_header(title="Primary diet effect (ANOVA) + effect size") %>%
  fmt_number(columns=c(anova_p, eta2), decimals=4)

emm_gt <- emm_tbl %>%
  gt(groupname_col="outcome") %>%
  tab_header(title="Estimated marginal means (95% CI)") %>%
  fmt_number(columns=c(emmean, lower.CL, upper.CL, SE, data_econs_emm), decimals=4)

tukey_gt <- tukey_tbl %>%
  gt(groupname_col="outcome") %>%
  tab_header(title="Tukey pairwise comparisons") %>%
  fmt_number(columns=c(estimate, SE, t.ratio, p.value, data_econs_tukey), decimals=4)

welch_gt <- welch_tbl %>%
  gt() %>%
  tab_header(title="Welch ANOVA sensitivity test") %>%
  fmt_number(columns=c(statistic, df1, df2, p_value), decimals=4)

robust_gt <- robust_tbl %>%
  gt() %>%
  tab_header(title="Robust trimmed-means ANOVA (WRS2::t1way) sensitivity test") %>%
  fmt_number(columns = c("statistic", "df", "p_value"), decimals = 4)


assum_tbl
primary_tbl
emm_tbl
tukey_tbl
welch_tbl
robust_tbl


# Optional CSV exports (also updated names)
write_csv(assum_tbl,  "Assumptions.csv")
write_csv(primary_tbl,"ANOVA_EffectSize.csv")
write_csv(emm_tbl,    "EMMeans_95CI.csv")
write_csv(tukey_tbl,  "Tukey_Pairwise.csv")
write_csv(welch_tbl,  "Welch_ANOVA.csv")
write_csv(robust_tbl, "Robust_TrimmedMeans_ANOVA.csv")


# ---------------------------
# 7) Plots (individual + multi-panel)
# ---------------------------
for(y in response_vars){
  print(res[[y]]$plot)
  ggsave(paste0("Fig_", y, ".png"), res[[y]]$plot, width=6.5, height=4.5, dpi=600)
}

combined <- wrap_plots(lapply(response_vars, \(y) res[[y]]$plot), ncol = 2)
print(combined)
ggsave("Fig_All_Outcomes.png", combined, width=10, height=12, dpi=600)

# ---------------------------
# 8) Optional: deep diagnostics for any one outcome
# ---------------------------
# performance::check_model(res[["gpm"]]$model)

############################################################
# End
############################################################





############################################################
# FINAL CLEAN ANALYSIS SCRIPT — GP2 ECONOMIC DATA
############################################################

library(tidyverse)
library(readxl)
library(janitor)
library(car)
library(emmeans)
library(effectsize)
library(WRS2)
library(broom)

# ---------------------------
# 1) Read data
# ---------------------------
data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()
dat <- data_econs

# ---------------------------
# 2) Types
# ---------------------------
dat <- dat %>%
  mutate(
    diet = factor(diet),
    replicate = factor(replicate)
  )

response_vars <- c("feed_consumed_kg","feed_cost_kg","sales_kg","gpm","cbr","roi")

# ---------------------------
# 3) Analysis function (no transform)
# ---------------------------
analyze_one <- function(y){
  
  m <- lm(reformulate("diet", y), data = dat)
  
  # assumptions
  shapiro_p <- shapiro.test(residuals(m))$p.value
  levene_p  <- car::leveneTest(dat[[y]] ~ dat$diet)$`Pr(>F)`[1]
  
  # ANOVA
  a <- car::Anova(m, type=2)
  anova_p <- a$`Pr(>F)`[1]
  
  eta_obj <- tryCatch(effectsize::eta_squared(m, partial = TRUE), error = function(e) NULL)
  
  eta <- NA_real_
  if(!is.null(eta_obj)){
    num_cols <- sapply(eta_obj, is.numeric)
    if(any(num_cols)){
      eta <- as.numeric(eta_obj[1, which(num_cols)[1]])
    }
  }
  
  
  # Welch
  welch <- oneway.test(reformulate("diet", y), data=dat)
  
  # EMM + Tukey
  emm <- emmeans(m, ~ diet)
  emm_df <- as.data.frame(emm)
  tukey_df <- as.data.frame(pairs(emm, adjust="tukey"))
  
  # robust trimmed means
  rob <- WRS2::t1way(reformulate("diet", y), data=dat)
  
  # plot
  p <- ggplot(dat, aes(diet, .data[[y]])) +
    geom_jitter(width=.12, size=2, alpha=.7) +
    geom_pointrange(
      data = emm_df,
      aes(y=emmean, ymin=lower.CL, ymax=upper.CL),
      linewidth=.9
    ) +
    theme_classic() +
    labs(
      title = paste0(
        y,
        " | ANOVA p=", signif(anova_p,3),
        " | Welch p=", signif(welch$p.value,3)
      ),
      y = y
    )
  
  list(
    assumptions = tibble(outcome=y, shapiro_p, levene_p),
    primary = tibble(outcome=y, anova_p, eta2=eta),
    welch = tibble(outcome=y, welch_p=welch$p.value),
    robust = tibble(outcome=y, robust_p=rob$p.value),
    emmeans = emm_df %>% mutate(outcome=y),
    tukey = tukey_df %>% mutate(outcome=y),
    plot = p
  )
}

# ---------------------------
# 4) Run all
# ---------------------------
res <- lapply(response_vars, analyze_one)

assum_tbl  <- map_df(res, "assumptions")
primary_tbl <- map_df(res, "primary")
welch_tbl  <- map_df(res, "welch")
robust_tbl <- map_df(res, "robust")
emm_tbl    <- map_df(res, "emmeans")
tukey_tbl  <- map_df(res, "tukey")

# ---------------------------
# 5) Console output
# ---------------------------
assum_tbl
primary_tbl
welch_tbl
robust_tbl
emm_tbl
tukey_tbl

# ---------------------------
# 6) Plots
# ---------------------------
for(r in res) print(r$plot)

############################################################
# END
############################################################






############################################################
# FINAL STABLE ANALYSIS — GP2 ECONOMIC DATA
############################################################

library(tidyverse)
library(readxl)
library(janitor)
library(car)
library(emmeans)
library(WRS2)

# ---------------------------
# Read data
# ---------------------------
data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()
dat <- data_econs

dat <- dat %>%
  mutate(
    diet = factor(diet),
    replicate = factor(replicate)
  )

response_vars <- c("feed_consumed_kg","feed_cost_kg","sales_kg","gpm","cbr","roi")

# ---------------------------
# Function
# ---------------------------
analyze_one <- function(y){
  
  m <- lm(reformulate("diet", y), data = dat)
  
  # assumptions
  shapiro_p <- shapiro.test(residuals(m))$p.value
  levene_p  <- car::leveneTest(dat[[y]] ~ dat$diet)$`Pr(>F)`[1]
  
  # ANOVA table
  aov_tab <- anova(m)
  
  ss_between <- aov_tab$`Sum Sq`[1]
  ss_total   <- sum(aov_tab$`Sum Sq`)
  eta2 <- ss_between / ss_total
  
  anova_p <- aov_tab$`Pr(>F)`[1]
  
  # Welch
  welch_p <- oneway.test(reformulate("diet", y), data=dat)$p.value
  
  # Robust trimmed means
  robust_p <- WRS2::t1way(reformulate("diet", y), data=dat)$p.value
  
  # EMM + Tukey
  emm <- emmeans(m, ~ diet)
  emm_df <- as.data.frame(emm)
  tukey_df <- as.data.frame(pairs(emm, adjust="tukey"))
  
  # plot
  p <- ggplot(dat, aes(diet, .data[[y]])) +
    geom_jitter(width=.12, size=2, alpha=.7) +
    geom_pointrange(
      data = emm_df,
      aes(y=emmean, ymin=lower.CL, ymax=upper.CL),
      linewidth=.9
    ) +
    theme_classic() +
    labs(
      title = paste0(
        y,
        " | ANOVA=", signif(anova_p,3),
        " | Welch=", signif(welch_p,3),
        " | Robust=", signif(robust_p,3)
      ),
      y = y
    )
  
  list(
    assumptions = tibble(outcome=y, shapiro_p, levene_p),
    primary = tibble(outcome=y, anova_p, eta2),
    welch = tibble(outcome=y, welch_p),
    robust = tibble(outcome=y, robust_p),
    emmeans = emm_df %>% mutate(outcome=y),
    tukey = tukey_df %>% mutate(outcome=y),
    plot = p
  )
}

# ---------------------------
# Run
# ---------------------------
res <- lapply(response_vars, analyze_one)

assum_tbl  <- map_df(res, "assumptions")
primary_tbl <- map_df(res, "primary")
welch_tbl  <- map_df(res, "welch")
robust_tbl <- map_df(res, "robust")
emm_tbl    <- map_df(res, "emmeans")
tukey_tbl  <- map_df(res, "tukey")

# ---------------------------
# Console tables
# ---------------------------
assum_tbl
primary_tbl
welch_tbl
robust_tbl
emm_tbl
tukey_tbl

car::Anova(lm(gpm ~ diet, data = dat), type=2)
car::Anova(lm(cbr ~ diet, data = dat), type=2)
car::Anova(lm(roi ~ diet, data = dat), type=2)
car::Anova(lm(feed_consumed_kg ~ diet, data = dat), type=2)
car::Anova(lm(feed_cost_kg ~ diet, data = dat), type=2)
car::Anova(lm(sales_kg ~ diet, data = dat), type=2)


# ---------------------------
# Plots
# ---------------------------
for(r in res) print(r$plot)

############################################################
# END
############################################################



############################################################
# JOURNAL-READY FIGURES (single complete code block)
# - Reads your Excel
# - Fits lm(outcome ~ diet) on original scale (no transforms)
# - Computes emmeans + 95% CI
# - Produces publication-quality single plots + one multi-panel figure
# - Saves 600 dpi PNG + TIFF (LZW) suitable for journals
############################################################

# ---------------------------
# 0) Packages
# ---------------------------
pkgs <- c("tidyverse","readxl","janitor","emmeans","car","WRS2","patchwork")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(readxl)
library(janitor)
library(emmeans)
library(car)
library(WRS2)
library(patchwork)

# ---------------------------
# 1) Read data (your flow)
# ---------------------------
# setwd("C:/Guinea-Pig-data_2/Data")
data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()
dat <- data_econs %>%
  mutate(
    diet = factor(diet),
    replicate = factor(replicate)
  )

response_vars <- c("feed_consumed_kg","feed_cost_kg","sales_kg","gpm","cbr","roi")
stopifnot(all(c("diet","replicate", response_vars) %in% names(dat)))

# ---------------------------
# 2) Labels (edit units/wording as needed)
# ---------------------------
y_labels <- c(
  feed_consumed_kg = "Feed consumed (kg)",
  feed_cost_kg     = "Feed cost (currency units)",
  sales_kg         = "Total sales (kg)",
  gpm              = "Gross profit margin",
  cbr              = "Cost–benefit ratio",
  roi              = "Return on investment"
)

# ---------------------------
# 3) Helper: p-value formatting
# ---------------------------
fmt_p <- function(p){
  if(is.na(p)) return("NA")
  if(p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# ---------------------------
# 4) Build one journal-ready plot for one outcome
# ---------------------------
make_outcome_plot <- function(y){
  
  # model (original scale)
  m <- lm(reformulate("diet", y), data = dat)
  
  # ANOVA p (Type II is fine; one-way equals standard)
  anova_p <- car::Anova(m, type = 2)$`Pr(>F)`[1]
  
  # Welch p (sensitivity)
  welch_p <- oneway.test(reformulate("diet", y), data = dat)$p.value
  
  # Robust trimmed-means p (sensitivity)
  robust_p <- WRS2::t1way(reformulate("diet", y), data = dat)$p.value
  
  # EMMs + 95% CI
  emm_df <- as.data.frame(emmeans(m, ~ diet))
  
  # Journal-ready plot: grayscale-safe, raw points + mean CI, clean border
  p <- ggplot(dat, aes(x = diet, y = .data[[y]])) +
    geom_point(
      shape = 21, fill = "white", colour = "black",
      size = 2.6, stroke = 0.4,
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.85
    ) +
    geom_pointrange(
      data = emm_df,
      aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
      linewidth = 0.9
    ) +
    labs(
      x = "Diet",
      y = unname(y_labels[[y]]),
      title = unname(y_labels[[y]]),
      subtitle = paste0(
        "ANOVA p=", fmt_p(anova_p),
        " | Welch p=", fmt_p(welch_p),
        " | Robust p=", fmt_p(robust_p)
      )
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.line = element_line(linewidth = 0.6),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.6),
      plot.margin = margin(8, 8, 8, 8)
    )
  
  list(plot = p, emm = emm_df, model = m)
}

# ---------------------------
# 5) Generate all plots
# ---------------------------
plot_list <- setNames(vector("list", length(response_vars)), response_vars)

for(y in response_vars){
  plot_list[[y]] <- make_outcome_plot(y)
}

# Display individual plots in console/viewer
for(y in response_vars){
  print(plot_list[[y]]$plot)
}

# ---------------------------
# 6) Combined multi-panel figure (submission figure)
# ---------------------------
combined_fig <- wrap_plots(lapply(response_vars, \(y) plot_list[[y]]$plot), ncol = 2) +
  plot_annotation(tag_levels = "A")

print(combined_fig)

# ---------------------------
# 7) Save figures (journal-ready exports)
# ---------------------------
# Individual figures
for(y in response_vars){
  ggsave(
    filename = paste0("Fig_", y, ".png"),
    plot = plot_list[[y]]$plot,
    width = 6.8, height = 4.6, dpi = 600
  )
  ggsave(
    filename = paste0("Fig_", y, ".tiff"),
    plot = plot_list[[y]]$plot,
    width = 6.8, height = 4.6, dpi = 600,
    compression = "lzw"
  )
}

# Combined figure
ggsave(
  filename = "Fig_Economic_Outcomes_Combined.png",
  plot = combined_fig,
  width = 8.2, height = 10.2, dpi = 600
)
ggsave(
  filename = "Fig_Economic_Outcomes_Combined.pdf",
  plot = combined_fig,
  width = 8.2, height = 10.2, dpi = 600
)

############################################################
# End
############################################################





############################################################
# GP2 ECONOMIC DATA — COMPLETE PIPELINE (CALC + ANALYSIS + FIGURES)
# - Reads Excel
# - Cleans names
# - Calculates: gross margin (gpm), cost–benefit ratio (cbr), feed ROI (%) (roi)
#   using cost + revenue columns (auto-detects common names)
# - Runs: assumption checks + ANOVA + Welch + robust trimmed-means + EMM/Tukey
# - Produces: journal-ready figures + combined multi-panel + exports
############################################################

# ---------------------------
# 0) Packages
# ---------------------------
pkgs <- c("tidyverse","readxl","janitor","car","emmeans","WRS2","patchwork")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(readxl)
library(janitor)
library(car)
library(emmeans)
library(WRS2)
library(patchwork)

# ---------------------------
# 1) Read + clean
# ---------------------------
setwd("C:/Guinea-Pig-data_2/Data")
data_econs <- read_excel("GP2_ECONOMIC_EVALUATION.xlsx") |> clean_names()
dat <- data_econs %>%
  mutate(
    diet = factor(diet),
    replicate = factor(replicate)
  )

# ---------------------------
# 2) Calculate economics from cost + revenue (robust to column names)
#    Definitions:
#      gpm (gross margin, absolute) = revenue - cost
#      cbr = revenue / cost
#      roi (feed ROI, %) = (revenue - cost)/cost * 100
# ---------------------------
# Try to detect cost/revenue columns if you didn't name them exactly "cost" and "revenue"
possible_cost <- c("cost")
possible_rev  <- c("revenue")

cost_col <- intersect(possible_cost, names(dat)) |> head(1)
rev_col  <- intersect(possible_rev,  names(dat)) |> head(1)

if(length(cost_col) == 0 || length(rev_col) == 0){
  stop(
    paste0(
      "Could not detect cost/revenue columns.\n",
      "Please rename your columns to 'cost' and 'revenue', or ensure one of these exists:\n",
      "Cost candidates: ", paste(possible_cost, collapse=", "), "\n",
      "Revenue candidates: ", paste(possible_rev, collapse=", ")
    )
  )
}


# ---------------------------
# 3) Economic calculations (FINAL DEFINITIONS)
# ---------------------------
dat <- dat %>%
  mutate(
    cost = as.numeric(.data[[cost_col]]),
    revenue = as.numeric(.data[[rev_col]]),
    
    # Absolute gross margin (profit)
    gpm = revenue - cost,
    
    # Cost–benefit ratio
    cbr = revenue / cost,
    
    # Feed ROI (%)
    roi = (revenue - cost) / cost * 100,
    
    # ✅ TRUE gross profit margin (% of revenue)
    gpm_percent = (revenue - cost) / revenue * 100
  )

# ---------------------------
# 4) Outcomes for analysis
# ---------------------------
response_vars <- c(
  "feed_consumed_kg",
  "cost",
  "revenue",
  "gpm",
  "gpm_percent",
  "cbr"
)

missing_vars <- setdiff(c("diet","replicate",response_vars), names(dat))
if(length(missing_vars)>0){
  stop("Missing required columns: ", paste(missing_vars, collapse=", "))
}


# 3) Descriptive table (mean ± SD)
# ---------------------------
desc_tbl <- dat %>%
  pivot_longer(all_of(response_vars), names_to="outcome", values_to="value") %>%
  group_by(outcome, diet) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups="drop"
  ) %>%
  mutate(`Mean ± SD` = sprintf("%.4f ± %.4f", mean, sd)) %>%
  select(outcome, diet, n, `Mean ± SD`)

desc_gt <- desc_tbl %>%
  gt(groupname_col="outcome") %>%
  tab_header(title="Descriptive statistics by diet (aggregated per replicate)")

print(desc_gt)
# gtsave(desc_gt, "Table_Descriptives.html")


# ---------------------------
# 5) Analysis function
# ---------------------------
analyze_one <- function(data, y){
  
  m <- lm(reformulate("diet", y), data=data)
  
  shapiro_p <- tryCatch(shapiro.test(residuals(m))$p.value, error=function(e) NA)
  levene_p  <- tryCatch(car::leveneTest(data[[y]] ~ data$diet)$`Pr(>F)`[1], error=function(e) NA)
  
  aov_tab <- anova(m)
  anova_p <- aov_tab$`Pr(>F)`[1]
  eta2 <- aov_tab$`Sum Sq`[1] / sum(aov_tab$`Sum Sq`)
  
  welch_p  <- oneway.test(reformulate("diet", y), data=data)$p.value
  robust_p <- WRS2::t1way(reformulate("diet", y), data=data)$p.value
  
  emm <- emmeans(m, ~ diet)
  emm_df <- as.data.frame(emm)
  tukey_df <- as.data.frame(pairs(emm, adjust="tukey"))
  
  p <- ggplot(data, aes(diet, .data[[y]])) +
    geom_jitter(width=.12, size=2, alpha=.7) +
    geom_pointrange(
      data=emm_df,
      aes(y=emmean, ymin=lower.CL, ymax=upper.CL),
      linewidth=.9
    ) +
    theme_classic() +
    labs(title=y, y=y, x="Diet")
  
  list(
    assumptions=tibble(outcome=y, shapiro_p, levene_p),
    primary=tibble(outcome=y, anova_p, eta2),
    welch=tibble(outcome=y, welch_p),
    robust=tibble(outcome=y, robust_p),
    emmeans=emm_df %>% mutate(outcome=y),
    tukey=tukey_df %>% mutate(outcome=y),
    plot=p
  )
}


# ---------------------------
# 6) Run analysis  (REVISED: keep stats + attach to plots)
# ---------------------------

# helper for p-value formatting on figure subtitles
fmt_p <- function(p){
  if(is.na(p)) return("NA")
  if(p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# Run analysis (unchanged call)
res <- lapply(response_vars, \(y) analyze_one(dat, y))

# Tables (unchanged)
assum_tbl   <- map_df(res, "assumptions")
primary_tbl <- map_df(res, "primary")
welch_tbl   <- map_df(res, "welch")
robust_tbl  <- map_df(res, "robust")
emm_tbl     <- map_df(res, "emmeans")
tukey_tbl   <- map_df(res, "tukey")

assum_tbl
primary_tbl
welch_tbl
robust_tbl

# ---------------------------
# 7) Journal-ready figures WITH statistics on subtitle (REVISED)
# ---------------------------
y_labels <- c(
  feed_consumed_kg = "Feed consumed (kg)",
  cost   = "Feed cost ($)",
  revenue         = "Total sales ($)",
  gpm              = "Gross margin ($)",
  gpm_percent      = "Gross profit margin (%)",
  cbr              = "Cost–benefit ratio"
)

# Build one journal-ready plot for one outcome INCLUDING:
# ANOVA p, Welch p, Robust p, eta2
make_outcome_plot <- function(y){
  
  # model
  m <- lm(reformulate("diet", y), data = dat)
  
  # ANOVA p (Type II; same as one-way standard but fine for reporting)
  anova_p <- car::Anova(m, type = 2)$`Pr(>F)`[1]
  
  # eta2 from SS (robust across package versions)
  aov_tab <- anova(m)
  eta2 <- aov_tab$`Sum Sq`[1] / sum(aov_tab$`Sum Sq`)
  
  # sensitivity tests
  welch_p  <- oneway.test(reformulate("diet", y), data = dat)$p.value
  robust_p <- WRS2::t1way(reformulate("diet", y), data = dat)$p.value
  
  # EMMs + CI
  emm_df <- as.data.frame(emmeans(m, ~ diet))
  
  # Plot with statistics
  p <- ggplot(dat, aes(x = diet, y = .data[[y]])) +
    geom_point(
      shape = 21, fill = "white", colour = "black",
      size = 2.6, stroke = 0.4,
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.85
    ) +
    geom_pointrange(
      data = emm_df,
      aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
      linewidth = 0.9
    ) +
    labs(
      x = "Diet",
      y = unname(y_labels[[y]]),
      title = unname(y_labels[[y]]),
      subtitle = paste0(
        "ANOVA p=", fmt_p(anova_p),
        " | η²=", sprintf("%.3f", eta2),
        " | Welch p=", fmt_p(welch_p),
        " | Robust p=", fmt_p(robust_p)
      )
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black"),
      axis.line = element_line(linewidth = 0.6),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.6),
      plot.margin = margin(8, 8, 8, 8)
    )
  
  # return both plot and stats (optional, useful later)
  list(
    plot = p,
    stats = tibble(
      outcome = y,
      anova_p = anova_p,
      eta2 = eta2,
      welch_p = welch_p,
      robust_p = robust_p
    )
  )
}

# Generate plots + keep stats
plot_list <- setNames(vector("list", length(response_vars)), response_vars)
fig_stats <- vector("list", length(response_vars))

for(y in response_vars){
  tmp <- make_outcome_plot(y)
  plot_list[[y]] <- tmp$plot
  fig_stats[[y]] <- tmp$stats
}

fig_stats_tbl <- bind_rows(fig_stats)
fig_stats_tbl

# Display individual plots
for(y in response_vars){
  print(plot_list[[y]])
}

# Combined multi-panel figure (A–G depending on outcomes)
combined_fig <- wrap_plots(plot_list, ncol = 2) +
  plot_annotation(tag_levels = "A")

print(combined_fig)

# Save
ggsave("Fig_new_Economic_Combined.png",  combined_fig, width = 8, height = 10, dpi = 600)
ggsave("Fig_new_Economic_Combined.pdf", combined_fig, width = 8, height = 10, dpi = 600)











