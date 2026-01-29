```r
############################################################
# Feeding trial (aggregated per replicate per diet) - EXCEL
# Complete, run-ready R script:
#  - imports Excel safely
#  - cleans/validates columns
#  - assumption checks
#  - analysis (LM/Type II ANOVA + eta² + EMMs + Tukey)
#  - automatic log-transform fallback (only if >0 and assumptions fail)
#  - publication plots + journal tables
#  - exports tables + figures
############################################################

# ---------------------------
# 0) Packages (install if missing)
# ---------------------------
req <- c(
  "readxl","janitor","tidyverse","car","emmeans","effectsize",
  "broom","performance","gt","patchwork"
)

to_install <- req[!req %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(readxl)
library(janitor)
library(tidyverse)
library(car)
library(emmeans)
library(effectsize)
library(broom)
library(performance)
library(gt)
library(patchwork)

# ---------------------------
# 1) User settings (EDIT THESE)
# ---------------------------
excel_path <- "feeding_trial.xlsx"   # <-- change to your file name/path
sheet_name <- 1                      # can be 1 or "Sheet1" etc.
out_dir <- "outputs"                 # where to save tables/figures

dir.create(out_dir, showWarnings = FALSE)

# ---------------------------
# 2) Import from Excel + clean names
# ---------------------------
df <- readxl::read_excel(excel_path, sheet = sheet_name) %>%
  janitor::clean_names()

# ---------------------------
# 3) Validate required columns (after clean_names)
# ---------------------------
# Expecting these columns AFTER clean_names():
# diet, replicate, feed_consumed_kg, feed_cost_kg, sales_kg, gpm, cbr, roi
needed <- c("diet","replicate","feed_consumed_kg","feed_cost_kg","sales_kg","gpm","cbr","roi")

missing_cols <- setdiff(needed, names(df))
if(length(missing_cols) > 0){
  stop(
    paste0(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      "\n\nTip: In Excel, ensure your headers match exactly:\n",
      "diet, replicate, feed_consumed_kg, feed_cost_kg, sales_kg, gpm, cbr, roi\n",
      "Then re-import. Current columns are:\n",
      paste(names(df), collapse = ", ")
    )
  )
}

# ---------------------------
# 4) Coerce types (safe) + basic checks
# ---------------------------
df <- df %>%
  mutate(
    diet = as.factor(diet),
    replicate = as.factor(replicate),
    across(all_of(needed[3:8]), ~ suppressWarnings(as.numeric(.)))
  )

# Missingness summary
miss_tbl <- df %>%
  summarise(across(all_of(needed[3:8]), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")

print(miss_tbl)
readr::write_csv(miss_tbl, file.path(out_dir, "Missingness_summary.csv"))

response_vars <- needed[3:8]

# ---------------------------
# 5) Descriptive table (Mean ± SD by diet)
# ---------------------------
desc_tbl <- df %>%
  pivot_longer(cols = all_of(response_vars), names_to = "outcome", values_to = "value") %>%
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
gtsave(desc_gt, file.path(out_dir, "Table_Descriptives.html"))
readr::write_csv(desc_tbl, file.path(out_dir, "Table_Descriptives.csv"))

# ---------------------------
# 6) Per-outcome analysis function
# ---------------------------
analyze_outcome <- function(data, y, alpha = 0.05){
  
  # Base model
  f <- as.formula(paste0(y, " ~ diet"))
  m0 <- lm(f, data = data)
  
  # Assumption checks (guarded against edge cases)
  shapiro_p <- tryCatch(shapiro.test(residuals(m0))$p.value, error = function(e) NA_real_)
  levene_p  <- tryCatch(car::leveneTest(data[[y]] ~ data$diet)$`Pr(>F)`[1], error = function(e) NA_real_)
  
  # Influence: Cook's distance
  cd <- tryCatch(cooks.distance(m0), error = function(e) rep(NA_real_, nrow(data)))
  n <- nrow(model.frame(m0))
  cooks_cut <- ifelse(n > 0, 4 / n, NA_real_)
  n_cooks_flag <- sum(cd > cooks_cut, na.rm = TRUE)
  
  # Decide on log-transform only if strictly positive and assumptions look bad
  all_pos <- all(data[[y]] > 0, na.rm = TRUE)
  
  assumption_failed <- (is.finite(shapiro_p) && shapiro_p < alpha) ||
    (is.finite(levene_p)  && levene_p  < alpha)
  
  use_log <- isTRUE(all_pos) && isTRUE(assumption_failed)
  
  model_type <- "LM"
  m <- m0
  data_used <- data
  
  if(use_log){
    data_used <- data %>% mutate(.y = log(.data[[y]]))
    m <- lm(.y ~ diet, data = data_used)
    model_type <- "LM (log-transformed; back-transformed EMMs)"
  }
  
  # Type II ANOVA
  a <- car::Anova(m, type = 2)
  p_val <- as.numeric(a$`Pr(>F)`[1])
  
  # Effect size (eta^2 partial)
  eta2 <- tryCatch({
    es <- effectsize::eta_squared(m, partial = TRUE)
    as.numeric(es$Eta2[1])
  }, error = function(e) NA_real_)
  
  # EMMs + Tukey pairwise
  if(use_log){
    emm <- emmeans(m, ~ diet)
    # back-transform EMMs (geometric means on original scale)
    emm_plot <- as.data.frame(emmeans(m, ~ diet, type = "response")) %>%
      transmute(
        diet = diet,
        emmean = response,
        lower.CL = lower.CL,
        upper.CL = upper.CL
      )
    emm_df <- as.data.frame(emm)
  } else {
    emm <- emmeans(m, ~ diet)
    emm_df <- as.data.frame(emm)
    emm_plot <- emm_df %>%
      transmute(diet = diet, emmean = emmean, lower.CL = lower.CL, upper.CL = upper.CL)
  }
  
  pairs_df <- as.data.frame(pairs(emm, adjust = "tukey"))
  
  # Assumption summary row
  assum_row <- tibble(
    outcome = y,
    model = model_type,
    shapiro_p = shapiro_p,
    levene_p = levene_p,
    cooks_cutoff = cooks_cut,
    cooks_flagged_n = n_cooks_flag,
    anova_p = p_val,
    eta2_partial = eta2
  )
  
  # Plot: raw points + EMMs (95% CI)
  p <- ggplot(data, aes(x = diet, y = .data[[y]])) +
    geom_point(
      position = position_jitter(width = 0.12, height = 0),
      alpha = 0.75,
      size = 2
    ) +
    geom_pointrange(
      data = emm_plot,
      aes(x = diet, y = emmean, ymin = lower.CL, ymax = upper.CL),
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Diet",
      y = if(use_log) paste0(y, " (back-transformed mean ± 95% CI)") else y,
      title = paste0(y, " | ", model_type,
                     " | Diet p=", signif(p_val, 3),
                     " | eta²p=", ifelse(is.na(eta2), "NA", signif(eta2, 3)))
    )
  
  list(
    outcome = y,
    model = m,
    assumptions = assum_row,
    emmeans = emm_plot,
    pairwise = pairs_df,
    plot = p
  )
}

# ---------------------------
# 7) Run analysis for all outcomes
# ---------------------------
results <- setNames(lapply(response_vars, \(y) analyze_outcome(df, y)), response_vars)

# Assumptions + main effects table
assum_all <- map_df(results, "assumptions")

assum_gt <- assum_all %>%
  gt() %>%
  tab_header(title = "Assumptions + Diet effect (per outcome)") %>%
  fmt_number(columns = c(shapiro_p, levene_p, anova_p, eta2_partial), decimals = 4)

print(assum_gt)
gtsave(assum_gt, file.path(out_dir, "Table_Assumptions_ANOVA.html"))
readr::write_csv(assum_all, file.path(out_dir, "Assumptions_ANOVA.csv"))

# EMM table (means + 95% CI)
emm_all <- map_df(names(results), function(y){
  results[[y]]$emmeans %>%
    mutate(outcome = y) %>%
    select(outcome, diet, emmean, lower.CL, upper.CL)
})

emm_gt <- emm_all %>%
  gt(groupname_col = "outcome") %>%
  tab_header(title = "Estimated marginal means by diet (95% CI)") %>%
  fmt_number(columns = c(emmean, lower.CL, upper.CL), decimals = 3)

print(emm_gt)
gtsave(emm_gt, file.path(out_dir, "Table_EMMeans_95CI.html"))
readr::write_csv(emm_all, file.path(out_dir, "EMMeans_95CI.csv"))

# Pairwise Tukey table
pair_all <- map_df(names(results), function(y){
  results[[y]]$pairwise %>%
    mutate(outcome = y) %>%
    select(outcome, contrast, estimate, SE, df, t.ratio, p.value)
})

pair_gt <- pair_all %>%
  gt(groupname_col = "outcome") %>%
  tab_header(title = "Pairwise diet contrasts (Tukey-adjusted)") %>%
  fmt_number(columns = c(estimate, SE, t.ratio, p.value), decimals = 4)

print(pair_gt)
gtsave(pair_gt, file.path(out_dir, "Table_Pairwise_Tukey.html"))
readr::write_csv(pair_all, file.path(out_dir, "Pairwise_Tukey.csv"))

# ---------------------------
# 8) Save plots (individual + combined)
# ---------------------------
plot_list <- lapply(response_vars, \(y) results[[y]]$plot)

# Individual figures
for(y in response_vars){
  ggsave(
    filename = file.path(out_dir, paste0("Fig_", y, ".png")),
    plot = results[[y]]$plot,
    width = 6.5, height = 4.5, dpi = 600
  )
}

# Combined multi-panel figure
combined <- wrap_plots(plot_list, ncol = 2)
ggsave(
  filename = file.path(out_dir, "Fig_All_Outcomes.png"),
  plot = combined,
  width = 10, height = 12, dpi = 600
)

# ---------------------------
# 9) Optional: Save diagnostic plots per outcome (residual plots)
# ---------------------------
# These are useful for supplementary materials / reviewer requests
for(y in response_vars){
  m <- results[[y]]$model
  png(file.path(out_dir, paste0("Diag_", y, "_check_model.png")), width = 1800, height = 1400, res = 250)
  try(performance::check_model(m), silent = TRUE)
  dev.off()
}

message("Done. Outputs saved in: ", normalizePath(out_dir))
############################################################
# End of script
############################################################
```
