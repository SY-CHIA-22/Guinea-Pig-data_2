############################################################
## Sensory analysis script (Nature Food-ready)
## - Ordinal models for 9-point hedonic attributes
## - Logistic model for willingness to buy
## - Publication figures + tables
############################################################

## 0) Packages ------------------------------------------------
pkgs <- c(
  "tidyverse", "janitor", "stringr", "readr", "ordinal",
  "emmeans", "broom", "broom.helpers", "scales"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

install.packages("tidyverse", dependencies = TRUE)
install.packages("ordinal", dependencies = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(stringr)
  library(readr)
  library(ordinal)
  library(emmeans)
  library(broom)
  library(broom.helpers)
  library(scales)
})

## 1) Read data (Excel) ---------------------------------------
## Put your Excel file in the working directory.
## Change file_path (and sheet, if needed).

## 1) READ DATA (EDIT FILE/PATH)
setwd("C:/Guinea-Pig-data_2/Data")

library(readxl)
library(janitor)

sensory_raw <- read_excel(
  path = "GP2_SENSORY_EVALUATION.xlsx",
  sheet = "Sheet1",
  col_types = "text"
) %>%
  clean_names()


# Sanity checks (CRITICAL)
stopifnot(is.data.frame(raw))
stopifnot(ncol(raw) > 0)

# Inspect column names
print(colnames(raw))



## Clean column names safely ---------------------------------
## Fix empty / NA column names BEFORE clean_names()

# Convert column names to character
colnames(raw) <- as.character(colnames(raw))

# Replace missing or empty names with placeholders
colnames(raw)[is.na(colnames(raw)) | colnames(raw) == ""] <-
  paste0("x", which(is.na(colnames(raw)) | colnames(raw) == ""))

# Now clean names safely
dat <- raw %>%
  janitor::clean_names() %>%
  mutate(
    diet = as.factor(diet),
    id = as.character(id),
    gender = as.factor(gender),
    age_group = as.factor(age_group)
  )


## 3) Robust hedonic parser (9-point) -------------------------
## Target 9-point hedonic:
## 1 Dislike extremely
## 2 Dislike very much
## 3 Dislike moderately
## 4 Dislike somewhat
## 5 Neither like nor dislike
## 6 Like somewhat
## 7 Like moderately
## 8 Like very much
## 9 Like extremely

parse_hedonic9 <- function(x) {
  x0 <- x
  
  # Keep NA as NA
  if (is.factor(x0)) x0 <- as.character(x0)
  if (length(x0) == 0) return(x0)
  out <- rep(NA_integer_, length(x0))
  
  # Normalize
  s <- toupper(trimws(x0))
  s <- str_replace_all(s, "_", " ")
  s <- str_replace_all(s, "\\s+", " ")
  s <- str_replace_all(s, "[^A-Z\\s]", " ")
  
  # Helper flags
  has_like     <- str_detect(s, "\\bLIKE\\b")
  has_dislike  <- str_detect(s, "\\bDISLIKE\\b")
  has_neither  <- str_detect(s, "NEITHER") | str_detect(s, "LIKE OR DISLIKE") | str_detect(s, "NOR DISLIKE")
  
  # Handle extremely (prefer explicit direction if both appear)
  out[str_detect(s, "DISLIKE") & str_detect(s, "EXTREM")] <- 1L
  out[str_detect(s, "LIKE")    & str_detect(s, "EXTREM")] <- 9L
  
  # Very much
  out[is.na(out) & str_detect(s, "DISLIKE") & str_detect(s, "VERY\\s*MUCH")] <- 2L
  out[is.na(out) & str_detect(s, "LIKE")    & str_detect(s, "VERY\\s*MUCH")] <- 8L
  
  # Moderately
  out[is.na(out) & str_detect(s, "DISLIKE") & str_detect(s, "MODERAT")] <- 3L
  out[is.na(out) & str_detect(s, "LIKE")    & str_detect(s, "MODERAT")] <- 7L
  
  # Somewhat
  out[is.na(out) & str_detect(s, "DISLIKE") & str_detect(s, "SOMEWHAT")] <- 4L
  out[is.na(out) & str_detect(s, "LIKE")    & str_detect(s, "SOMEWHAT")] <- 6L
  
  # Neither like nor dislike (catch messy strings)
  out[is.na(out) & has_neither] <- 5L
  
  # If still NA but contains LIKE only (no qualifier), default to 6 (Like somewhat)
  out[is.na(out) & has_like & !has_dislike & !has_neither] <- 6L
  
  # If still NA but contains DISLIKE only, default to 4 (Dislike somewhat)
  out[is.na(out) & has_dislike & !has_like & !has_neither] <- 4L
  
  # If still NA but blank / empty, keep NA
  out
}

## 4) Identify sensory attributes & recode --------------------
sensory_vars <- c("colour","taste","tenderness","juiciness","flavour","overall_quality")

missing_vars <- setdiff(sensory_vars, names(dat))
if(length(missing_vars) > 0) {
  stop("Missing expected columns: ", paste(missing_vars, collapse = ", "))
}

dat2 <- dat %>%
  mutate(across(all_of(sensory_vars), parse_hedonic9)) %>%
  mutate(
    willingness_to_buy = case_when(
      is.na(willingness_to_buy) ~ NA_character_,
      toupper(trimws(as.character(willingness_to_buy))) %in% c("YES","Y","1","TRUE") ~ "YES",
      toupper(trimws(as.character(willingness_to_buy))) %in% c("NO","N","0","FALSE") ~ "NO",
      TRUE ~ NA_character_
    ),
    willingness_to_buy = factor(willingness_to_buy, levels = c("NO","YES"))
  )

## Optional: check recoding success
recoding_summary <- dat2 %>%
  summarise(across(all_of(sensory_vars), ~mean(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "attribute", values_to = "prop_missing")

message("Proportion missing after recoding (by attribute):")
print(recoding_summary)

## 5) Long format for modelling/plots --------------------------
long <- dat2 %>%
  select(diet, id, gender, age_group, all_of(sensory_vars), willingness_to_buy) %>%
  pivot_longer(cols = all_of(sensory_vars), names_to = "attribute", values_to = "score") %>%
  mutate(
    attribute = factor(attribute, levels = sensory_vars),
    score_f = ordered(score, levels = 1:9)  # ordinal outcome
  )

## 6) Ordinal model per attribute ------------------------------
## We use cumulative link model (CLM) with logit link:
## score ~ diet + gender + age_group
## (If you truly have the same panelists rating multiple diets, upgrade to CLMM with random intercept for id.)
fit_one_attribute <- function(df_one) {
  df_one <- df_one %>% filter(!is.na(score_f))
  if(nrow(df_one) < 20 || n_distinct(df_one$diet) < 2) return(NULL)
  
  m <- tryCatch(
    ordinal::clm(score_f ~ diet + gender + age_group, data = df_one, link = "logit"),
    error = function(e) NULL
  )
  m
}

models <- long %>%
  group_by(attribute) %>%
  group_map(~fit_one_attribute(.x), .keep = TRUE) %>%
  set_names(levels(long$attribute))

## 7) Diet EMMs (on latent scale) + global p-values ------------
emm_table_list <- list()
pvals <- tibble(attribute = character(), p_diet = numeric())

for(att in names(models)) {
  m <- models[[att]]
  if(is.null(m)) next
  
  # Global test for diet
  an <- tryCatch(anova(m), error = function(e) NULL)
  if(!is.null(an) && "diet" %in% rownames(an)) {
    p_diet <- an["diet", "Pr(>Chisq)"]
  } else {
    p_diet <- NA_real_
  }
  pvals <- bind_rows(pvals, tibble(attribute = att, p_diet = p_diet))
  
  # EMMs by diet (latent scale; consistent across ordinal models)
  em <- emmeans::emmeans(m, ~ diet)
  em_df <- as.data.frame(em) %>%
    as_tibble() %>%
    mutate(attribute = att) %>%
    relocate(attribute)
  
  emm_table_list[[att]] <- em_df
}

emm_table <- bind_rows(emm_table_list)

# FDR adjustment across attributes for the global diet test
pvals <- pvals %>%
  mutate(p_diet_fdr = p.adjust(p_diet, method = "fdr"))

## 8) Pairwise diet contrasts (optional, manuscript supplement) -
pairwise_list <- list()
for(att in names(models)) {
  m <- models[[att]]
  if(is.null(m)) next
  pw <- tryCatch(
    emmeans::contrast(emmeans(m, ~ diet), method = "pairwise", adjust = "tukey"),
    error = function(e) NULL
  )
  if(is.null(pw)) next
  pairwise_list[[att]] <- as.data.frame(pw) %>% as_tibble() %>% mutate(attribute = att)
}
pairwise_contrasts <- bind_rows(pairwise_list)

## 9) Willingness-to-buy model (logistic regression) -----------
wtb <- dat2 %>%
  filter(!is.na(willingness_to_buy)) %>%
  mutate(wtb = as.integer(willingness_to_buy == "YES"))

wtb_model <- NULL
if(nrow(wtb) >= 20 && n_distinct(wtb$diet) >= 2) {
  wtb_model <- glm(wtb ~ diet + gender + age_group, data = wtb, family = binomial())
}

wtb_table <- NULL
if(!is.null(wtb_model)) {
  wtb_table <- broom::tidy(wtb_model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      term = str_replace(term, "^diet", "Diet: "),
      term = str_replace(term, "^gender", "Gender: "),
      term = str_replace(term, "^age_group", "Age group: ")
    ) %>%
    select(term, estimate, conf.low, conf.high, p.value) %>%
    rename(OR = estimate, CI_low = conf.low, CI_high = conf.high, p = p.value)
}

## 10) Figures -------------------------------------------------
dir.create("outputs", showWarnings = FALSE)

## Figure 1: Likert distribution (stacked proportions) by Diet
likert_df <- long %>%
  filter(!is.na(score)) %>%
  mutate(
    score_label = factor(score, levels = 1:9,
                         labels = c("1 Dislike\nextremely","2 Dislike\nvery much","3 Dislike\nmoderately",
                                    "4 Dislike\nsomewhat","5 Neither","6 Like\nsomewhat",
                                    "7 Like\nmoderately","8 Like\nvery much","9 Like\nextremely"))
  ) %>%
  count(attribute, diet, score_label) %>%
  group_by(attribute, diet) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

fig1 <- ggplot(likert_df, aes(x = diet, y = prop, fill = score_label)) +
  geom_col(color = "black", linewidth = 0.2) +
  facet_wrap(~ attribute, ncol = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Diet", y = "Proportion of responses", fill = "Hedonic rating") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/Figure1_LikertDistribution_byDiet.tiff",
       fig1, width = 10.5, height = 7.5, dpi = 600, compression = "lzw")
ggsave("outputs/Figure1_LikertDistribution_byDiet.pdf",
       fig1, width = 10.5, height = 7.5)

## Figure 2: Mean ± 95% CI (descriptive, very common in food papers)
summary_means <- long %>%
  filter(!is.na(score)) %>%
  group_by(attribute, diet) %>%
  summarise(
    n = n(),
    mean = mean(score),
    sd = sd(score),
    se = sd/sqrt(n),
    ci = 1.96*se,
    .groups = "drop"
  )

fig2 <- ggplot(summary_means, aes(x = diet, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ attribute, ncol = 3) +
  scale_y_continuous(limits = c(1,9), breaks = 1:9) +
  labs(x = "Diet", y = "Mean hedonic score (± 95% CI)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("outputs/Figure2_Means95CI_byDiet.tiff",
       fig2, width = 10.5, height = 7.0, dpi = 600, compression = "lzw")
ggsave("outputs/Figure2_Means95CI_byDiet.pdf",
       fig2, width = 10.5, height = 7.0)


### 11) Tables (manuscript-ready exports) -----------------------
dir.create("outputs", showWarnings = FALSE)

## Make sure emm_table exists
stopifnot(exists("emm_table"))

## Ensure CI columns exist (robust across emmeans versions)
## If your emm_table came from as.data.frame(emmeans(...)) without infer=TRUE,
## it will NOT contain lower.CL/upper.CL.
if(!all(c("lower.CL","upper.CL") %in% names(emm_table))) {
  
  # Recompute emmeans properly with CIs from the fitted models
  emm_table_list <- list()
  
  for(att in names(models)) {
    m <- models[[att]]
    if(is.null(m)) next
    
    em <- emmeans::emmeans(m, ~ diet)
    em_sum <- as.data.frame(summary(em, infer = c(TRUE, TRUE))) %>% as_tibble()
    em_sum$attribute <- att
    emm_table_list[[att]] <- em_sum
  }
  
  emm_table <- bind_rows(emm_table_list)
}

## Now safely build Table 1 (use a unique object name to avoid collisions)

## --- Robust rebuild of emm_table with confidence intervals --------------------
## This guarantees CI columns exist (and handles different CI column names)

emm_table <- purrr::imap_dfr(models, function(m, att) {
  if (is.null(m)) return(NULL)
  
  em <- emmeans::emmeans(m, ~ diet)
  
  # infer=c(TRUE, TRUE) requests confidence intervals
  s <- as.data.frame(summary(em, infer = c(TRUE, TRUE))) %>% tibble::as_tibble()
  s$attribute <- att
  s
})

# Check what columns we got (useful for debugging)
print(names(emm_table))

## --- Detect CI column names robustly -----------------------------------------
ci_low_candidates  <- c("lower.CL", "asymp.LCL", "LCL")
ci_high_candidates <- c("upper.CL", "asymp.UCL", "UCL")

ci_low_col  <- intersect(ci_low_candidates,  names(emm_table))[1]
ci_high_col <- intersect(ci_high_candidates, names(emm_table))[1]

if (is.na(ci_low_col) || is.na(ci_high_col)) {
  stop(
    "Could not find CI columns in emm_table. Found columns:\n",
    paste(names(emm_table), collapse = ", ")
  )
}

## --- Build Table 1 safely -----------------------------------------------------
table_sensory1 <- emm_table %>%
  dplyr::rename(
    EMM = emmean,
    SE  = SE
  ) %>%
  dplyr::mutate(
    CI_low  = .data[[ci_low_col]],
    CI_high = .data[[ci_high_col]]
  ) %>%
  dplyr::left_join(pvals, by = "attribute") %>%
  dplyr::arrange(attribute, diet) %>%
  dplyr::mutate(
    attribute  = stringr::str_to_title(stringr::str_replace_all(attribute, "_", " ")),
    Diet_p     = p_diet,
    Diet_p_FDR = p_diet_fdr
  ) %>%
  dplyr::select(attribute, diet, EMM, SE, CI_low, CI_high, Diet_p, Diet_p_FDR)

readr::write_csv(table_sensory1, "outputs/Table1_OrdinalEMMs_byDiet.csv")
print(table_sensory1)

## Table 1b: Descriptive means (often main table instead of EMMs for Nature-style)
table1_desc <- summary_means %>%
  mutate(attribute = str_to_title(str_replace_all(attribute, "_", " "))) %>%
  select(attribute, diet, n, mean, sd, se, ci) %>%
  arrange(attribute, diet)

write_csv(table1_desc, "outputs/Table1b_DescriptiveMeans_byDiet.csv")

## Table S1 (supplement): Pairwise contrasts (Tukey)
if(nrow(pairwise_contrasts) > 0) {
  pairwise_out <- pairwise_contrasts %>%
    mutate(attribute = str_to_title(str_replace_all(attribute, "_", " "))) %>%
    arrange(attribute, p.value)
  write_csv(pairwise_out, "outputs/TableS1_PairwiseDietContrasts_Tukey.csv")
}

## Table 2: Willingness-to-buy (Odds ratios)
if(!is.null(wtb_table)) {
  write_csv(wtb_table, "outputs/Table2_WillingnessToBuy_OR.csv")
}

## 12) Quick console summary ----------------------------------
cat("\n==== Global Diet effects (ordinal models) ====\n")
print(pvals %>% mutate(attribute = str_to_title(str_replace_all(attribute, "_", " "))))

if(!is.null(wtb_model)) {
  cat("\n==== Willingness-to-buy logistic model summary ====\n")
  print(summary(wtb_model))
}

cat("\nOutputs written to: ", normalizePath("outputs"), "\n")
cat("Figures:\n - Figure1_LikertDistribution_byDiet (tiff/pdf)\n - Figure2_Means95CI_byDiet (tiff/pdf)\n")
cat("Tables:\n - Table1_OrdinalEMMs_byDiet.csv (model-based)\n - Table1b_DescriptiveMeans_byDiet.csv (descriptive)\n - TableS1_PairwiseDietContrasts_Tukey.csv (optional)\n - Table2_WillingnessToBuy_OR.csv (if model fit)\n")
############################################################

### figures

## ===================== FIGURE EXPORT (ROBUST + DIAGNOSTIC) =====================
# Required packages
pkgs <- c("ggplot2", "dplyr", "tidyr", "stringr", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(stringr); library(scales)
})

# 0) Sanity: required objects exist?
req <- c("dat2","long","summary_means","pvals")
missing_req <- req[!vapply(req, exists, logical(1))]
if(length(missing_req) > 0){
  stop("These objects are missing in your session: ",
       paste(missing_req, collapse=", "),
       "\nRe-run the script from the top up to creation of these objects, then re-run this block.")
}

# 1) Ensure outputs folder exists + show absolute path
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
cat("\nWorking directory (getwd):\n", getwd(), "\n")
cat("Outputs folder (absolute path):\n", normalizePath("outputs"), "\n\n")

# 2) Quick check that there is plottable data
cat("Rows in long:", nrow(long), "\n")
cat("Non-missing scores in long:", sum(!is.na(long$score)), "\n\n")
if(sum(!is.na(long$score)) == 0) {
  stop("All sensory scores are NA in `long$score`. This means the hedonic recoding failed.\n",
       "Check your raw text values (spelling/format) and the parse_hedonic9() function.")
}
# 3) Theme (journal-clean)
theme_nature <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2),
    strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.3),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
# 4) Robust save function (prints success/fail)
save_fig <- function(plot, filename_base, width, height) {
  tiff_path <- file.path("outputs", paste0(filename_base, ".tiff"))
  pdf_path  <- file.path("outputs", paste0(filename_base, ".pdf"))
  
  ok_tiff <- TRUE
  ok_pdf  <- TRUE
  
  tryCatch({
    ggsave(tiff_path, plot = plot, width = width, height = height,
           dpi = 600, compression = "lzw")
  }, error = function(e){
    ok_tiff <<- FALSE
    message("TIFF save failed for ", filename_base, ": ", e$message)
  })
  
  tryCatch({
    ggsave(pdf_path, plot = plot, width = width, height = height) # default PDF device (most robust)
  }, error = function(e){
    ok_pdf <<- FALSE
    message("PDF save failed for ", filename_base, ": ", e$message)
  })
  
  cat("SAVE STATUS -", filename_base, "\n",
      "  TIFF:", if(ok_tiff) "OK" else "FAILED", "->", tiff_path, "\n",
      "  PDF :", if(ok_pdf)  "OK" else "FAILED", "->", pdf_path,  "\n\n")
}

# 5) Recreate plot data (so the block is self-contained)

# Figure 1 data: Likert distribution
likert_df <- long %>%
  filter(!is.na(score)) %>%
  mutate(
    score = as.integer(score),
    score_label = factor(
      score, levels = 1:9,
      labels = c("1 Dislike extremely","2 Dislike very much","3 Dislike moderately",
                 "4 Dislike somewhat","5 Neither","6 Like somewhat",
                 "7 Like moderately","8 Like very much","9 Like extremely")
    )
  ) %>%
  count(attribute, diet, score_label, name = "n") %>%
  group_by(attribute, diet) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Figure 2 data: means + p-values
summary_means_plot <- summary_means %>%
  mutate(attribute = factor(attribute, levels = levels(long$attribute)))

pvals_for_plot <- pvals %>%
  mutate(
    attribute = factor(attribute, levels = levels(long$attribute)),
    p_label = case_when(
      is.na(p_diet_fdr) ~ "FDR p=NA",
      p_diet_fdr < 0.001 ~ "FDR p<0.001",
      TRUE ~ paste0("FDR p=", formatC(p_diet_fdr, format = "f", digits = 3))
    )
  )

# 6) Build plots

fig1 <- ggplot(likert_df, aes(x = diet, y = prop, fill = score_label)) +
  geom_col(color = "black", linewidth = 0.15) +
  facet_wrap(~ attribute, ncol = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Diet", y = "Proportion of responses", fill = "Hedonic rating") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_nature

fig2 <- ggplot(summary_means_plot, aes(x = diet, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.15, linewidth = 0.6) +
  facet_wrap(~ attribute, ncol = 3, scales = "fixed") +
  scale_y_continuous(limits = c(1, 9), breaks = 1:9,
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Diet", y = "Mean hedonic score (± 95% CI)") +
  theme_nature +
  geom_text(
    data = pvals_for_pl ot,
    aes(x = -Inf, y = 8.9, label = p_label),
    inherit.aes = FALSE,
    hjust = -0.05, vjust = 1, size = 3.2
  )
# 7) Save plots
save_fig(fig1, "Figure1_LikertDistribution_byDiet", width = 10.5, height = 7.2)
save_fig(fig2, "Figure2_Means95CI_byDiet_withPvalues", width = 10.5, height = 7.0)

# 8) Show files that exist right now
cat("Files currently in outputs/:\n")
print(list.files("outputs", full.names = TRUE))




## =====================NEW=======================================================##
################################################################################
################################################################################

# 1. Recode the 9-point hedonic scale (clean & safe)
## Step 1: Load packages

library(tidyverse)
library(janitor)
library(forcats)

## Step 2: Read and clean data
setwd("C:/Guinea-Pig-data_2/Data")

library(readxl)
library(janitor)

sensory_raw <- read_excel(
  path = "GP2_SENSORY_EVALUATION.xlsx",
  col_types = "text"
) %>%
  clean_names()

sensory_raw <- sensory_raw %>%
  clean_names() %>%
  mutate(across(everything(), ~na_if(., "N/A"))) # Replaces "N/A" text entries with proper missing values (NA)

##Step 3: Define the hedonic scale mapping
### Important: Use numeric direction correctly
#### Higher = more liking (as per sensory science standards)

hedonic_map <- c(
  "DISLIKE EXTREMELY"       = 1,
  "DISLIKE VERY MUCH"       = 2,
  "DISLIKE MODERATELY"      = 3,
  "DISLIKE SOMEWHAT"        = 4,
  "NEITHER LIKE NOR DISLIKE"= 5,
  "LIKE SOMEWHAT"           = 6,
  "LIKE MODERATELY"         = 7,
  "LIKE VERY MUCH"          = 8,
  "LIKE EXTREMELY"          = 9
)

## Step 4: Apply recoding to all sensory attributes
sensory_num <- sensory_raw %>%
  mutate(
    across(
      c(colour, flavour, tendrness, juiciness, taste, overall_quality),
      ~ hedonic_map[toupper(.)] %>% as.numeric()
    )
  )

## Step 5: Recode WILLINGNESS_TO_BUY (binary outcome)
sensory_num <- sensory_num %>%
  mutate(
    willingness_to_buy = case_when(
      willingness_to_buy == "YES" ~ 1,
      willingness_to_buy == "NO"  ~ 0,
      TRUE ~ NA_real_
    )
  )

# Export new dataset (recoded data)
install.packages("writexl")   # run once
library(writexl)

write_xlsx(
  sensory_num,
  path = "sensory_data_recoded.xlsx"
)


## Step 6: Convert grouping variables to factors
sensory_num <- sensory_num %>%
  mutate(
    diet       = factor(diet),
    gender     = factor(gender),
    age_group  = factor(age_group),
    id         = factor(id)
  )


# DIAGNOSIS FOR MODEL SUITABILITY
install.packages(c("see", "performance", "insight", "parameters"))  # run once
# Restart R session after installing

library(lme4)
library(lmerTest)
library(performance)
library(see)

m1 <- lmer(overall_quality ~ diet + gender + age_group + (1|id),
           data = sensory_num, REML = TRUE)
print(summary(m1))

# plots
check_model(m1)

# printed checks
print(check_normality(m1)) # OK: residuals appear as normally distributed (p = 0.557).
print(check_heteroscedasticity(m1)) # OK: Error variance appears to be homoscedastic (p = 0.362).

# collinearity (optional)
print(check_collinearity(m1)) # Low correlation: Multicollinearity is not a concern.


# 2. Descriptive analysis (first message)
## Attribute means ± SD by diet
sensory_summary <- sensory_num %>%
  group_by(diet) %>%
  summarise(
    across(
      c(colour, flavour, tendrness, juiciness, taste, overall_quality),
      list(mean = ~mean(., na.rm = TRUE),
           sd   = ~sd(., na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    n = n()
  )

sensory_summary


library(dplyr)

journal_table <- journal_table %>%
  rename(
    willingness_to_buy_yes = willingness_to_buy_yes_pct
  )

write_xlsx(journal_table, "Table_Sensory_byDiet_JournalReady.xlsx")
install.packages(c("xfun","gt","writexl","emmeans","lme4","lmerTest","performance","see"),
                 type = "binary")
library(gt)



gt_tbl <- journal_table %>%
  gt(rowname_col = "diet") %>%
  tab_header(
    title = "Sensory evaluation summary by diet",
    subtitle = "Sensory attributes presented as Mean ± SD (n=non-missing). Gender/Age presented as n (%). Willingness-to-buy presented as Yes n (%)."
  ) %>%
  cols_label(
    n_respondents = "Respondents (unique ID, n)",
    colour = "Colour",
    flavour = "Flavour",
    tendrness = "Tenderness",
    juiciness = "Juiciness",
    taste = "Taste",
    overall_quality = "Overall quality",
    willingness_to_buy_yes = "Willingness-to-buy (Yes)"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 11
  )

gt_tbl

# Save to working directory
gtsave(gt_tbl, "Table_Sensory_byDiet_JournalReady.html")




#Fix: correct n per attribute (non-missing counts)

library(tidyverse)
library(janitor)

sensory_vars <- c("colour","flavour","tendrness","juiciness","taste","overall_quality")

sensory_summary_fixed <- sensory_num %>%
  group_by(diet) %>%
  summarise(
    n_respondents = n_distinct(id),
    
    # Means
    across(all_of(sensory_vars),
           ~ mean(.x, na.rm = TRUE),
           .names = "{.col}_mean"),
    
    # SDs
    across(all_of(sensory_vars),
           ~ sd(.x, na.rm = TRUE),
           .names = "{.col}_sd"),
    
    # Correct non-missing n
    across(all_of(sensory_vars),
           ~ sum(!is.na(.x)),
           .names = "{.col}_n"),
    
    # Willingness to buy: %Yes and n (non-missing)
    willingness_to_buy_n = sum(!is.na(willingness_to_buy)),
    willingness_to_buy_yes_pct = 100 * mean(willingness_to_buy == 1, na.rm = TRUE),
    
    .groups = "drop"
  )

sensory_summary_fixed



# Optional: add “Mean ± SD” formatted columns (journal-friendly)
sensory_summary_fixed_pretty <- sensory_summary_fixed %>%
  mutate(
    across(ends_with("_mean"), ~ round(.x, 2)),
    across(ends_with("_sd"),   ~ round(.x, 2)),
    across(ends_with("_n"),    ~ as.integer(.x)),
    willingness_to_buy_yes_pct = round(willingness_to_buy_yes_pct, 1)
  ) %>%
  mutate(
    ## Create Mean ± SD text columns
    colour = sprintf("%.2f ± %.2f", colour_mean, colour_sd),
    flavour = sprintf("%.2f ± %.2f", flavour_mean, flavour_sd),
    tendrness = sprintf("%.2f ± %.2f", tendrness_mean, tendrness_sd),
    juiciness = sprintf("%.2f ± %.2f", juiciness_mean, juiciness_sd),
    taste = sprintf("%.2f ± %.2f", taste_mean, taste_sd),
    overall_quality = sprintf("%.2f ± %.2f", overall_quality_mean, overall_quality_sd)
  ) %>%
  ## Keep a clean set of columns (edit as you like)
  select(
    diet, n_respondents,
    colour, flavour, tendrness, juiciness, taste, overall_quality,
    colour_n, flavour_n, tendrness_n, juiciness_n, taste_n, overall_quality_n,
    willingness_to_buy_yes_pct, willingness_to_buy_n
  )

sensory_summary_fixed_pretty


#Export the corrected table to Excel (working directory)
install.packages("writexl")  # run once if needed
library(writexl)

write_xlsx(sensory_summary_fixed_pretty, "Table1_Sensory_Descriptives_FIXED.xlsx")


# Factor in Gender###############
library(tidyverse)

sensory_vars <- c("colour","flavour","tendrness","juiciness","taste","overall_quality")

# ---- 1) Summary by Diet x Gender (correct n, mean, sd) ----
sensory_summary_diet_gender <- sensory_num %>%
  group_by(diet, gender) %>%
  summarise(
    n_respondents = n_distinct(id),
    
    across(all_of(sensory_vars),
           ~ mean(.x, na.rm = TRUE),
           .names = "{.col}_mean"),
    
    across(all_of(sensory_vars),
           ~ sd(.x, na.rm = TRUE),
           .names = "{.col}_sd"),
    
    across(all_of(sensory_vars),
           ~ sum(!is.na(.x)),
           .names = "{.col}_n"),
    
    willingness_to_buy_n = sum(!is.na(willingness_to_buy)),
    willingness_to_buy_yes_pct = 100 * mean(willingness_to_buy == 1, na.rm = TRUE),
    
    .groups = "drop"
  )

# ---- 2) Pretty formatting (Mean ± SD + clean columns) ----
sensory_summary_fixed_pretty_gender <- sensory_summary_diet_gender %>%
  mutate(
    across(ends_with("_mean"), ~ round(.x, 2)),
    across(ends_with("_sd"),   ~ round(.x, 2)),
    across(ends_with("_n"),    ~ as.integer(.x)),
    willingness_to_buy_yes_pct = round(willingness_to_buy_yes_pct, 1)
  ) %>%
  mutate(
    colour = sprintf("%.2f ± %.2f", colour_mean, colour_sd),
    flavour = sprintf("%.2f ± %.2f", flavour_mean, flavour_sd),
    tendrness = sprintf("%.2f ± %.2f", tendrness_mean, tendrness_sd),
    juiciness = sprintf("%.2f ± %.2f", juiciness_mean, juiciness_sd),
    taste = sprintf("%.2f ± %.2f", taste_mean, taste_sd),
    overall_quality = sprintf("%.2f ± %.2f", overall_quality_mean, overall_quality_sd)
  ) %>%
  select(
    diet, gender, n_respondents,
    colour, flavour, tendrness, juiciness, taste, overall_quality,
    colour_n, flavour_n, tendrness_n, juiciness_n, taste_n, overall_quality_n,
    willingness_to_buy_yes_pct, willingness_to_buy_n
  ) %>%
  arrange(diet, gender)

sensory_summary_fixed_pretty_gender

##export to Excel (working directory)
writexl::write_xlsx(
  sensory_summary_fixed_pretty_gender,
  "Table1_Sensory_Descriptives_byDiet_byGender.xlsx"
)


## =========================
## Journal-ready SIMPLE panel (facet_wrap)
## - Bar = mean (raw) per diet
## - Error bars = ± SE
## - Points = individual responses (jittered)
## - Color-friendly palette (Okabe–Ito)
## - Facet by attribute (single panel layout)
## =========================


library(tidyverse)
library(janitor)
library(ggplot2)

# 1) Long format
sensory_vars <- c("colour","flavour","tendrness","juiciness","taste","overall_quality")

sensory_long <- sensory_num %>%
  clean_names() %>%
  pivot_longer(
    cols = all_of(sensory_vars),
    names_to = "attribute",
    values_to = "score"
  ) %>%
  filter(!is.na(score), score >= 1, score <= 9) %>%
  mutate(
    diet = factor(diet),
    attribute = factor(
      attribute,
      levels = sensory_vars,
      labels = c("Colour","Flavour","Tenderness","Juiciness","Taste","Overall quality")
    )
  )

# 2) Summary stats (Mean ± SE)
sum_se <- sensory_long %>%
  group_by(attribute, diet) %>%
  summarise(
    mean = mean(score),
    se   = sd(score) / sqrt(n()),
    .groups = "drop"
  )

# 3) Color-blind friendly palette (Okabe–Ito)
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#0072B2")
names(okabe_ito) <- levels(sum_se$diet)

# 4) TRUE bar graph panel — NO gridlines, neutral line kept
p_bar_panel <- ggplot(sum_se, aes(x = diet, y = mean, fill = diet)) +
  
  # Neutral reference line (5 = neither like nor dislike)
  geom_hline(
    yintercept = 5,
    linetype = "dashed",
    linewidth = 0.8,
    colour = "grey40"
  ) +
  
  # Bars
  geom_col(
    width = 0.65,
    alpha = 0.90,
    colour = "black",
    linewidth = 0.45
  ) +
  
  # Error bars (± SE)
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 0.85
  ) +
  
  facet_wrap(~ attribute, ncol = 3) +
  
  scale_fill_manual(values = okabe_ito) +
  scale_y_continuous(breaks = 1:9) +
  coord_cartesian(ylim = c(1, 9)) +
  
  labs(
    x = "Diet (treatment)",
    y = "Hedonic score (mean ± SE; 1–9)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    # 🔑 REMOVE ALL GRIDLINES
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Keep clean panel border
    panel.border = element_rect(colour = "black", linewidth = 0.6),
    
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

p_bar_panel



## Export - journal ready
ggsave(
  "Fig_Sensory_BarGraph_Panel_NoGrid_NeutralLine.png",
  p_bar_panel,
  width = 10.8,
  height = 7.0,
  dpi = 600
)

ggsave(
  "Fig_Sensory_BarGraph_Panel_NoGrid_NeutralLine.pdf",
  p_bar_panel,
  width = 10.8,
  height = 7.0,
  dpi = 600
)


################# key message or observation to note: Journal message

#“All sensory attributes scored above the neutral point (5), 
# indicating overall consumer acceptance across treatments (diets).” - True 


## ============================================================
## ALL-IN-ONE (robust): Sensory analysis for ALL attributes
## - Mixed model per attribute: score ~ diet + gender + age_group + (1|id)
## - Outputs: diet p-value (Type III), EMMs ± 95% CI, Tukey letters, R2
## - Exports: Excel tables + journal-ready EMM bar-panel with neutral line at 5
## ============================================================

## 0) Packages ----
req <- c("tidyverse","janitor","lme4","lmerTest","emmeans","car","performance","multcomp","writexl","ggplot2")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

## 1) Settings ----
sensory_vars <- c("colour","flavour","tendrness","juiciness","taste","overall_quality")

# Okabe–Ito color-blind friendly palette
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#0072B2",
               "#D55E00","#CC79A7","#F0E442","#000000")

## 2) Clean + types + checks ----
sensory_num <- sensory_num %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    diet      = factor(diet),
    gender    = factor(gender),
    age_group = factor(age_group),
    id        = factor(id)
  )

missing_cols <- setdiff(sensory_vars, names(sensory_num))
if(length(missing_cols) > 0){
  stop("Missing sensory columns in sensory_num: ", paste(missing_cols, collapse = ", "))
}

non_numeric <- sensory_vars[!sapply(sensory_num[sensory_vars], is.numeric)]
if(length(non_numeric) > 0){
  stop("These sensory columns are not numeric (recode to 1–9 first): ", paste(non_numeric, collapse = ", "))
}

## 3) Long data ----
sensory_long <- sensory_num %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(sensory_vars),
    names_to = "attribute",
    values_to = "score"
  ) %>%
  dplyr::filter(!is.na(score), score >= 1, score <= 9) %>%
  dplyr::mutate(
    attribute = factor(
      attribute,
      levels = sensory_vars,
      labels = c("Colour","Flavour","Tenderness","Juiciness","Taste","Overall quality")
    )
  )

## 4) Helper: get Type III p-value for diet ----
get_diet_p <- function(m){
  a <- car::Anova(m, type = 3)
  if(!"diet" %in% rownames(a)) return(NA_real_)
  as.numeric(a["diet","Pr(>Chisq)"])
}

## 5) Fit models per attribute (store model objects) ----
models_tbl <- sensory_long %>%
  dplyr::group_by(attribute) %>%
  dplyr::group_modify(~{
    m <- lme4::lmer(score ~ diet + gender + age_group + (1|id),
                    data = .x, REML = TRUE)
    r2 <- performance::r2_nakagawa(m)
    
    tibble::tibble(
      model = list(m),
      diet_p = get_diet_p(m),
      r2_marginal = r2$R2_marginal,
      r2_conditional = r2$R2_conditional
    )
  }) %>%
  dplyr::ungroup()

## 6) EMMs ± 95% CI per attribute × diet  (NO unnest of emmGrid) ----
emm_long <- purrr::pmap_dfr(
  list(models_tbl$attribute, models_tbl$model, models_tbl$diet_p, models_tbl$r2_marginal, models_tbl$r2_conditional),
  function(attribute, model, diet_p, r2_marginal, r2_conditional){
    
    em <- emmeans::emmeans(model, ~ diet)
    em_df <- as.data.frame(em)  # converts emmGrid -> data.frame safely
    
    em_df %>%
      dplyr::transmute(
        attribute = attribute,
        diet = diet,
        diet_p = diet_p,
        r2_marginal = r2_marginal,
        r2_conditional = r2_conditional,
        EMM = emmean,
        SE = SE,
        df = df,
        CI_low = lower.CL,
        CI_high = upper.CL
      )
  }
)

## 7) Tukey letters per attribute (NO unnest of emmGrid) ----
letters_long <- purrr::pmap_dfr(
  list(models_tbl$attribute, models_tbl$model),
  function(attribute, model){
    em <- emmeans::emmeans(model, ~ diet)
    cld_df <- as.data.frame(multcomp::cld(em, adjust = "tukey", Letters = letters))
    cld_df %>%
      dplyr::transmute(
        attribute = attribute,
        diet = diet,
        letters = gsub("\\s+", "", .group)
      )
  }
)

## 8) Final long results table (best for Supplementary) ----
results_long <- emm_long %>%
  dplyr::left_join(letters_long, by = c("attribute","diet")) %>%
  dplyr::arrange(attribute, diet) %>%
  dplyr::mutate(
    EMM = round(EMM, 2),
    SE = round(SE, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    diet_p = signif(diet_p, 3),
    r2_marginal = round(r2_marginal, 3),
    r2_conditional = round(r2_conditional, 3),
    emm_ci = sprintf("%.2f (%.2f–%.2f)", EMM, CI_low, CI_high)
  )

## 9) Wide Table 2: cells = EMM (CI) + letters ----
table2_wide <- results_long %>%
  dplyr::mutate(cell = paste0(emm_ci, " ", letters)) %>%
  dplyr::select(attribute, diet, cell) %>%
  tidyr::pivot_wider(names_from = attribute, values_from = cell) %>%
  dplyr::arrange(diet)

## 10) Export tables (Excel) ----
writexl::write_xlsx(
  list(
    Results_long = results_long,
    Table2_EMM_CI_Letters = table2_wide
  ),
  path = "Sensory_AllAttributes_MixedModel_Tables.xlsx"
)

## 11) Journal-ready EMM bar-panel plot + neutral line at 5 ----
plot_df <- results_long %>%
  dplyr::mutate(
    diet = factor(diet, levels = levels(sensory_num$diet)),
    attribute = factor(attribute, levels = levels(sensory_long$attribute))
  )

diet_levels <- levels(plot_df$diet)
fill_cols <- setNames(okabe_ito[seq_len(min(length(diet_levels), length(okabe_ito)))], diet_levels)

p_bar_emm_panel <- ggplot2::ggplot(plot_df, ggplot2::aes(x = diet, y = EMM, fill = diet)) +
  ggplot2::geom_hline(yintercept = 5, linetype = "dashed", linewidth = 0.8, colour = "grey40") +
  ggplot2::geom_col(width = 0.65, alpha = 0.90, colour = "black", linewidth = 0.45) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = EMM - SE, ymax = EMM + SE),
                         width = 0.18, linewidth = 0.85) +
  ggplot2::facet_wrap(~ attribute, ncol = 3) +
  ggplot2::scale_fill_manual(values = fill_cols) +
  ggplot2::scale_y_continuous(breaks = 1:9) +
  ggplot2::coord_cartesian(ylim = c(1, 9)) +
  ggplot2::labs(x = "Diet (treatment)", y = "Model-adjusted hedonic score (EMM ± SE; 1–9)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", linewidth = 0.6),
    legend.position = "none",
    strip.text = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(face = "bold")
  )

print(p_bar_emm_panel)

## 12) Export figure ----
ggplot2::ggsave("Fig_Sensory_EMM_BarPanel_NeutralLine.png",
                p_bar_emm_panel, width = 10.8, height = 7.0, dpi = 600)

ggplot2::ggsave("Fig_Sensory_EMM_BarPanel_NeutralLine.tiff",
                p_bar_emm_panel, width = 10.8, height = 7.0, dpi = 600, compression = "lzw")

message("Done. Saved in: ", getwd(),
        "\n- Sensory_AllAttributes_MixedModel_Tables.xlsx",
        "\n- Fig_Sensory_EMM_BarPanel_NeutralLine.png",
        "\n- Fig_Sensory_EMM_BarPanel_NeutralLine.tiff")



## 
## =========================
## Extract reportable model statistics
## =========================

install.packages("broom.mixed", type = "binary")

library(performance)
library(car)
library(broom.mixed)

library(performance)
library(car)
library(dplyr)

# ---- robust extractor for the "diet" row from car::Anova ----
extract_diet_test <- function(model){
  a <- car::Anova(model, type = 3)
  
  # Convert to data.frame safely
  a_df <- as.data.frame(a)
  a_df$term <- rownames(a_df)
  
  # Find the row for diet
  if(!any(a_df$term == "diet")){
    return(tibble::tibble(diet_stat = NA_real_, diet_df = NA_real_, diet_p = NA_real_, test = NA_character_))
  }
  
  row <- a_df[a_df$term == "diet", , drop = FALSE]
  
  # Identify which test statistic is present
  if("Chisq" %in% names(row)){
    tibble::tibble(
      diet_stat = as.numeric(row$Chisq[1]),
      diet_df   = as.numeric(row$Df[1]),
      diet_p    = as.numeric(row$`Pr(>Chisq)`[1]),
      test      = "Chisq"
    )
  } else if("F value" %in% names(row)) {
    # Some setups produce F-tests
    pcol <- if("Pr(>F)" %in% names(row)) "Pr(>F)" else names(row)[grepl("^Pr\\(", names(row))][1]
    tibble::tibble(
      diet_stat = as.numeric(row$`F value`[1]),
      diet_df   = as.numeric(row$Df[1]),
      diet_p    = as.numeric(row[[pcol]][1]),
      test      = "F"
    )
  } else {
    # Unknown format
    tibble::tibble(diet_stat = NA_real_, diet_df = NA_real_, diet_p = NA_real_, test = "unknown")
  }
}

# ---- build model statistics table from models_tbl ----
model_stats <- purrr::map_dfr(seq_len(nrow(models_tbl)), function(i){
  
  att <- models_tbl$attribute[i]
  m   <- models_tbl$model[[i]]
  
  # diet test
  dt <- extract_diet_test(m)
  
  # R2
  r2 <- performance::r2_nakagawa(m)
  
  # ICC
  icc <- performance::icc(m)$ICC_adjusted
  
  # variance components
  vc <- as.data.frame(VarCorr(m))
  var_id <- vc$vcov[vc$grp == "id"][1]
  var_resid <- sigma(m)^2
  
  tibble::tibble(
    attribute = att,
    test = dt$test,
    diet_stat = dt$diet_stat,
    diet_df = dt$diet_df,
    diet_p = dt$diet_p,
    R2_marginal = r2$R2_marginal,
    R2_conditional = r2$R2_conditional,
    ICC = icc,
    var_id = var_id,
    var_resid = var_resid
  )
}) %>%
  dplyr::mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    diet_p = signif(diet_p, 3)
  )

model_stats

##################################
model_stats[, c("attribute", "test", "diet_stat", "diet_df", "diet_p")]

overall_tests <- model_stats %>%
  dplyr::transmute(
    Attribute = attribute,
    Test = ifelse(test == "Chisq", "χ²", "F"),
    Statistic = diet_stat,
    df = diet_df,
    p_value = diet_p
  )

overall_tests

overall_tests %>%
  dplyr::mutate(Significant = p_value < 0.05)
###############################

## ============================================================
## Single code: annotate each facet with overall test statistic + p-value
## Requires: model_stats (attribute, test, diet_stat, diet_df, diet_p)
##           p_bar_emm_panel (your existing bar-panel plot object)
## Outputs: p_bar_emm_panel_annotated + saved PNG/TIFF
## ============================================================

library(dplyr)
library(ggplot2)

# 1) Build panel labels: overall test statistic + p-value per attribute
pval_labels <- model_stats %>%
  dplyr::mutate(
    label = dplyr::if_else(
      test == "Chisq",
      paste0("\u03C7\u00B2(", diet_df, ") = ", round(diet_stat, 2), ", p = ", signif(diet_p, 2)),
      paste0("F(", diet_df, ") = ", round(diet_stat, 2), ", p = ", signif(diet_p, 2))
    )
  ) %>%
  dplyr::select(attribute, label)

# 2) Set y-position inside each facet (fixed position for 1–9 hedonic scale)
pval_labels <- pval_labels %>%
  dplyr::mutate(y = 8.6)  # adjust (e.g., 8.4–8.8) if you want it higher/lower

# 3) Add annotation layer onto your existing panel plot
p_bar_emm_panel_annotated <- p_bar_emm_panel +
  ggplot2::geom_text(
    data = pval_labels,
    ggplot2::aes(x = 1, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3.4,
    fontface = "italic"
  )

print(p_bar_emm_panel_annotated)

# 4) Export to working directory
ggplot2::ggsave("Fig_Sensory_EMM_BarPanel_Pvalues.png",
                p_bar_emm_panel_annotated, width = 10.8, height = 7.0, dpi = 600)

ggplot2::ggsave("Fig_Sensory_EMM_BarPanel_Pvalues.pdf",
                p_bar_emm_panel_annotated, width = 10.8, height = 7.0, dpi = 600)




## ============================================================
## SINGLE SCRIPT: Willingness-to-buy (binary) — MODEL-PREDICTED PROBABILITIES
## - Fits logistic model: willingness_to_buy ~ diet + gender + age_group
## - Computes predicted probability (adjusted) for each diet (marginal over gender/age_group)
## - Plots predicted probability ± 95% CI (journal-ready)
## - Annotates plot with overall diet test statistic + p-value (Type III LR test)
## - Exports PNG + TIFF + Excel tables
## ============================================================

## 0) Packages ----
req <- c("dplyr","tidyr","ggplot2","janitor","car","emmeans","writexl")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

## 1) Clean + ensure variables exist/typed ----
sensory_num <- sensory_num %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    diet      = factor(diet),
    gender    = factor(gender),
    age_group = factor(age_group),
    id        = factor(id)
  )

if(!"willingness_to_buy" %in% names(sensory_num)){
  stop("Column 'willingness_to_buy' not found in sensory_num. It should be coded 0/1.")
}

wtb_dat <- sensory_num %>%
  dplyr::filter(!is.na(willingness_to_buy)) %>%
  dplyr::mutate(
    willingness_to_buy = as.integer(willingness_to_buy),
    willingness_to_buy = dplyr::if_else(willingness_to_buy %in% c(0L,1L), willingness_to_buy, NA_integer_)
  ) %>%
  dplyr::filter(!is.na(willingness_to_buy))

if(nlevels(wtb_dat$diet) < 2) stop("diet must have at least 2 levels for modelling.")

## 2) Fit logistic regression (adjusted) ----
m_wtb <- stats::glm(
  willingness_to_buy ~ diet + gender + age_group,
  data = wtb_dat,
  family = stats::binomial()
)

## 3) Overall diet test (Type III LR Chi-square) for annotation ----
a3 <- car::Anova(m_wtb, type = 3, test.statistic = "LR")
a3_df <- as.data.frame(a3)
a3_df$term <- rownames(a3_df)

diet_row <- a3_df[a3_df$term == "diet", , drop = FALSE]
if(nrow(diet_row) != 1) stop("Could not uniquely identify 'diet' row in Type III table.")

# Robust column handling across car versions
if("LR Chisq" %in% names(diet_row)){
  stat <- as.numeric(diet_row$`LR Chisq`[1])
  df1  <- as.numeric(diet_row$Df[1])
  pval <- as.numeric(diet_row$`Pr(>Chisq)`[1])
} else if("Chisq" %in% names(diet_row)){
  stat <- as.numeric(diet_row$Chisq[1])
  df1  <- as.numeric(diet_row$Df[1])
  pval <- as.numeric(diet_row$`Pr(>Chisq)`[1])
} else {
  statcol <- names(diet_row)[grepl("Chisq", names(diet_row))][1]
  pcol <- names(diet_row)[grepl("Pr\\(>Chisq\\)", names(diet_row))][1]
  stat <- as.numeric(diet_row[[statcol]][1])
  df1  <- as.numeric(diet_row$Df[1])
  pval <- as.numeric(diet_row[[pcol]][1])
}

test_label <- paste0("\u03C7\u00B2(", df1, ") = ", round(stat, 2), ", p = ", signif(pval, 2))

## 4) Predicted probabilities by diet (marginal over gender + age_group) ----
# emmeans on response scale gives predicted probabilities
pp <- emmeans::emmeans(m_wtb, ~ diet, type = "response")  # response = probability scale
pp_df <- as.data.frame(pp) %>%
  dplyr::transmute(
    diet,
    prob = prob,
    SE = SE,
    CI_low = asymp.LCL,
    CI_high = asymp.UCL
  ) %>%
  dplyr::mutate(
    pct = 100 * prob,
    CI_low_pct = 100 * CI_low,
    CI_high_pct = 100 * CI_high,
    pct_ci = sprintf("%.1f%% (%.1f–%.1f)", pct, CI_low_pct, CI_high_pct)
  )

## 5) Plot: Predicted probability ± 95% CI (journal-ready) ----
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#0072B2",
               "#D55E00","#CC79A7","#F0E442","#000000")
diet_levels <- levels(wtb_dat$diet)
fill_cols <- setNames(okabe_ito[seq_len(min(length(diet_levels), length(okabe_ito)))], diet_levels)

p_predprob <- ggplot2::ggplot(pp_df, ggplot2::aes(x = diet, y = pct, fill = diet)) +
  ggplot2::geom_col(width = 0.65, alpha = 0.90, colour = "black", linewidth = 0.45) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_low_pct, ymax = CI_high_pct),
                         width = 0.18, linewidth = 0.85) +
  ggplot2::scale_fill_manual(values = fill_cols) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    x = "Diet (treatment)",
    y = "Predicted willingness to buy (Yes, %)",
    caption = "Bars = model-predicted probability of YES (marginal over gender and age group); error bars = 95% CI (logistic model)."
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", linewidth = 0.6),
    legend.position = "none",
    axis.text.x = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(hjust = 0)
  ) +
  ggplot2::annotate(
    "text",
    x = 1, y = 98,
    label = test_label,
    hjust = 0, vjust = 1,
    fontface = "italic",
    size = 3.8
  )

print(p_predprob)

## 6) Export outputs ----
writexl::write_xlsx(
  list(
    PredictedProb_byDiet = pp_df,
    TypeIII_LR_Tests = a3_df
  ),
  path = "WillingnessToBuy_PredictedProb_Results.xlsx"
)

ggplot2::ggsave("Fig_WTB_PredictedProb_byDiet.png", p_predprob, width = 7.5, height = 5.2, dpi = 600)
ggplot2::ggsave("Fig_WTB_PredictedProb_byDiet.pdf", p_predprob, width = 7.5, height = 5.2, dpi = 600)

message("Done. Saved in: ", getwd(),
        "\n- WillingnessToBuy_PredictedProb_Results.xlsx",
        "\n- Fig_WTB_PredictedProb_byDiet.png",
        "\n- Fig_WTB_PredictedProb_byDiet.tiff",
        "\nDiet test shown on plot: ", test_label)


# consider age_group

## ============================================================
## SINGLE SCRIPT (revised): Willingness-to-buy — diet plot + age-group effect in plot
## - Fits logistic model: willingness_to_buy ~ diet + gender + age_group
## - Makes TWO journal-ready predicted-probability plots:
##     (A) Predicted %YES by diet (marginal over gender+age_group) + diet χ²,p annotation
##     (B) Predicted %YES by age_group (marginal over diet+gender) + age_group χ²,p annotation
##   Optional (C): Facet by age_group to show diet within each age group
## - Exports: Excel tables + PNG + TIFF (and PDF if you want)
## ============================================================

## 0) Packages ----
req <- c("dplyr","tidyr","ggplot2","janitor","car","emmeans","writexl","patchwork")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

## 1) Clean + ensure variables exist/typed ----
sensory_num <- sensory_num %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    diet      = factor(diet),
    gender    = factor(gender),
    age_group = factor(age_group),
    id        = factor(id)
  )

if(!"willingness_to_buy" %in% names(sensory_num)){
  stop("Column 'willingness_to_buy' not found in sensory_num. It should be coded 0/1.")
}

wtb_dat <- sensory_num %>%
  dplyr::filter(!is.na(willingness_to_buy)) %>%
  dplyr::mutate(
    willingness_to_buy = as.integer(willingness_to_buy),
    willingness_to_buy = dplyr::if_else(willingness_to_buy %in% c(0L,1L), willingness_to_buy, NA_integer_)
  ) %>%
  dplyr::filter(!is.na(willingness_to_buy))

if(nlevels(wtb_dat$diet) < 2) stop("diet must have at least 2 levels for modelling.")
if(nlevels(wtb_dat$age_group) < 2) stop("age_group must have at least 2 levels for modelling.")

## 2) Fit logistic regression (adjusted) ----
m_wtb <- stats::glm(
  willingness_to_buy ~ diet + gender + age_group,
  data = wtb_dat,
  family = stats::binomial()
)

## 3) Type III LR tests for annotation (diet + age_group) ----
a3 <- car::Anova(m_wtb, type = 3, test.statistic = "LR")
a3_df <- as.data.frame(a3); a3_df$term <- rownames(a3_df)

make_lr_label <- function(term_name){
  row <- a3_df[a3_df$term == term_name, , drop = FALSE]
  if(nrow(row) != 1) return(paste0(term_name, ": NA"))
  if("LR Chisq" %in% names(row)){
    stat <- as.numeric(row$`LR Chisq`[1])
    df1  <- as.numeric(row$Df[1])
    pval <- as.numeric(row$`Pr(>Chisq)`[1])
  } else {
    stat <- as.numeric(row$Chisq[1])
    df1  <- as.numeric(row$Df[1])
    pval <- as.numeric(row$`Pr(>Chisq)`[1])
  }
  paste0("\u03C7\u00B2(", df1, ") = ", round(stat, 2), ", p = ", signif(pval, 2))
}

diet_label <- make_lr_label("diet")
age_label  <- make_lr_label("age_group")

## 4) Predicted probabilities (response scale) ----
# (A) diet marginal over gender + age_group
pp_diet <- emmeans::emmeans(m_wtb, ~ diet, type = "response")
pp_diet_df <- as.data.frame(pp_diet) %>%
  dplyr::transmute(
    diet,
    prob = prob,
    CI_low = asymp.LCL,
    CI_high = asymp.UCL
  ) %>%
  dplyr::mutate(
    pct = 100 * prob,
    lo  = 100 * CI_low,
    hi  = 100 * CI_high
  )

# (B) age_group marginal over diet + gender
pp_age <- emmeans::emmeans(m_wtb, ~ age_group, type = "response")
pp_age_df <- as.data.frame(pp_age) %>%
  dplyr::transmute(
    age_group,
    prob = prob,
    CI_low = asymp.LCL,
    CI_high = asymp.UCL
  ) %>%
  dplyr::mutate(
    pct = 100 * prob,
    lo  = 100 * CI_low,
    hi  = 100 * CI_high
  )

# (C) OPTIONAL: diet within each age_group (shows age effect visually in the same figure)
pp_diet_by_age <- emmeans::emmeans(m_wtb, ~ diet | age_group, type = "response")
pp_diet_by_age_df <- as.data.frame(pp_diet_by_age) %>%
  dplyr::transmute(
    age_group,
    diet,
    prob = prob,
    CI_low = asymp.LCL,
    CI_high = asymp.UCL
  ) %>%
  dplyr::mutate(
    pct = 100 * prob,
    lo  = 100 * CI_low,
    hi  = 100 * CI_high
  )

## 5) Color-blind friendly palette (Okabe–Ito) ----
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#0072B2",
               "#D55E00","#CC79A7","#F0E442","#000000")

diet_levels <- levels(wtb_dat$diet)
fill_cols <- setNames(okabe_ito[seq_len(min(length(diet_levels), length(okabe_ito)))], diet_levels)

## 6) PLOTS (more appealing: Dot + CI; you can switch to bars if you insist) ----
# (A) Predicted by diet (dot + CI)
p_diet <- ggplot2::ggplot(pp_diet_df, ggplot2::aes(x = diet, y = pct, colour = diet)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), linewidth = 1.05) +
  ggplot2::geom_point(size = 4.2) +
  ggplot2::scale_colour_manual(values = fill_cols) +
  ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
  ggplot2::labs(
    x = "Diet (treatment)",
    y = "Predicted willingness to buy (Yes, %)",
    title = "Effect of diet",
    caption = "Points = model-predicted probability of YES (marginal over gender + age group); lines = 95% CI (logistic model)."
  ) +
  ggplot2::annotate("text", x = 1, y = 98, label = paste0("Diet: ", diet_label),
                    hjust = 0, vjust = 1, fontface = "italic", size = 4.1) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(face = "bold"),
                 plot.caption = ggplot2::element_text(hjust = 0))

# (B) Predicted by age_group (dot + CI) — highlights the significant effect
p_age <- ggplot2::ggplot(pp_age_df, ggplot2::aes(x = age_group, y = pct)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), linewidth = 1.05, colour = "black") +
  ggplot2::geom_point(size = 4.2, colour = "black") +
  ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
  ggplot2::labs(
    x = "Age group",
    y = "Predicted willingness to buy (Yes, %)",
    title = "Effect of age group",
    caption = "Points = model-predicted probability of YES (marginal over diet + gender); lines = 95% CI."
  ) +
  ggplot2::annotate("text", x = 1, y = 98, label = paste0("Age group: ", age_label),
                    hjust = 0, vjust = 1, fontface = "italic", size = 4.1) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold"),
                 plot.caption = ggplot2::element_text(hjust = 0))

# (C) OPTIONAL: diet within each age group (facet) — shows age-group differences “in the plot”
p_diet_by_age <- ggplot2::ggplot(pp_diet_by_age_df, ggplot2::aes(x = diet, y = pct, colour = diet)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), linewidth = 0.95) +
  ggplot2::geom_point(size = 3.6) +
  ggplot2::facet_wrap(~ age_group, nrow = 1) +
  ggplot2::scale_colour_manual(values = fill_cols) +
  ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
  ggplot2::labs(
    x = "Diet (treatment)",
    y = "Predicted willingness to buy (Yes, %)",
    title = "Diet effect within each age group",
    caption = "Points = model-predicted probability of YES; lines = 95% CI (logistic model)."
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(face = "bold"),
                 plot.caption = ggplot2::element_text(hjust = 0))

## 7) Combine into one figure (A + B) ----
fig_AB <- p_diet + p_age + patchwork::plot_layout(ncol = 2)
print(fig_AB)

## If you prefer the age-facet version instead (C), print it:
print(p_diet_by_age)

## 8) Export outputs ----
writexl::write_xlsx(
  list(
    PredProb_byDiet = pp_diet_df,
    PredProb_byAgeGroup = pp_age_df,
    PredProb_Diet_by_AgeGroup = pp_diet_by_age_df,
    TypeIII_LR_Tests = a3_df
  ),
  path = "WillingnessToBuy_PredictedProb_Diet_AgeGroup.xlsx"
)

ggplot2::ggsave("Fig_WTB_PredProb_Diet_and_AgeGroup.png", fig_AB, width = 11.0, height = 5.2, dpi = 600)
ggplot2::ggsave("Fig_WTB_PredProb_Diet_and_AgeGroup.tiff", fig_AB, width = 11.0, height = 5.2, dpi = 600, compression = "lzw")

# Optional export of facet plot (C)
ggplot2::ggsave("Fig_WTB_PredProb_Diet_by_AgeGroup_Facet.png", p_diet_by_age, width = 11.0, height = 4.4, dpi = 600)
ggplot2::ggsave("Fig_WTB_PredProb_Diet_by_AgeGroup_Facet.tiff", p_diet_by_age, width = 11.0, height = 4.4, dpi = 600, compression = "lzw")

message("Done. Saved in: ", getwd(),
        "\n- WillingnessToBuy_PredictedProb_Diet_AgeGroup.xlsx",
        "\n- Fig_WTB_PredProb_Diet_and_AgeGroup.png/.tiff",
        "\n- Fig_WTB_PredProb_Diet_by_AgeGroup_Facet.png/.tiff",
        "\nAnnotations: Diet=", diet_label, " | Age group=", age_label)



## ============================================================
## SINGLE FIGURE (as requested): Facet by AGE GROUP only, diet on x
## - Logistic model: willingness_to_buy ~ diet + gender + age_group
## - Plot: predicted %YES by diet WITHIN each age_group (facet)
## - Adds: overall Diet test (LR χ²,p) + overall Age-group test (LR χ²,p) on the figure
## - Exports: PNG + TIFF + Excel tables
## ============================================================

## 0) Packages ----
req <- c("dplyr","tidyr","ggplot2","janitor","car","emmeans","writexl")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

## 1) Clean + ensure variables exist/typed ----
sensory_num <- sensory_num %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    diet      = factor(diet),
    gender    = factor(gender),
    age_group = factor(age_group),
    id        = factor(id)
  )

if(!"willingness_to_buy" %in% names(sensory_num)){
  stop("Column 'willingness_to_buy' not found in sensory_num. It should be coded 0/1.")
}

wtb_dat <- sensory_num %>%
  dplyr::filter(!is.na(willingness_to_buy)) %>%
  dplyr::mutate(
    willingness_to_buy = as.integer(willingness_to_buy),
    willingness_to_buy = dplyr::if_else(willingness_to_buy %in% c(0L,1L), willingness_to_buy, NA_integer_)
  ) %>%
  dplyr::filter(!is.na(willingness_to_buy))

if(nlevels(wtb_dat$diet) < 2) stop("diet must have at least 2 levels for modelling.")
if(nlevels(wtb_dat$age_group) < 2) stop("age_group must have at least 2 levels for modelling.")

## 2) Fit logistic regression (adjusted) ----
m_wtb <- stats::glm(
  willingness_to_buy ~ diet + gender + age_group,
  data = wtb_dat,
  family = stats::binomial()
)

## 3) Type III LR tests (diet + age_group) for annotation ----
a3 <- car::Anova(m_wtb, type = 3, test.statistic = "LR")
a3_df <- as.data.frame(a3); a3_df$term <- rownames(a3_df)

make_lr_label <- function(term_name){
  row <- a3_df[a3_df$term == term_name, , drop = FALSE]
  if(nrow(row) != 1) return(paste0(term_name, ": NA"))
  if("LR Chisq" %in% names(row)){
    stat <- as.numeric(row$`LR Chisq`[1])
    df1  <- as.numeric(row$Df[1])
    pval <- as.numeric(row$`Pr(>Chisq)`[1])
  } else {
    stat <- as.numeric(row$Chisq[1])
    df1  <- as.numeric(row$Df[1])
    pval <- as.numeric(row$`Pr(>Chisq)`[1])
  }
  paste0("\u03C7\u00B2(", df1, ") = ", round(stat, 2), ", p = ", signif(pval, 2))
}

diet_label <- make_lr_label("diet")
age_label  <- make_lr_label("age_group")

## 4) Predicted probabilities: diet within each age_group ----
pp_diet_by_age <- emmeans::emmeans(m_wtb, ~ diet | age_group, type = "response")
pp_df <- as.data.frame(pp_diet_by_age) %>%
  dplyr::transmute(
    age_group,
    diet,
    prob = prob,
    CI_low = asymp.LCL,
    CI_high = asymp.UCL
  ) %>%
  dplyr::mutate(
    pct = 100 * prob,
    lo  = 100 * CI_low,
    hi  = 100 * CI_high
  )

# Keep factor ordering consistent
pp_df <- pp_df %>%
  dplyr::mutate(
    diet = factor(diet, levels = levels(wtb_dat$diet)),
    age_group = factor(age_group, levels = levels(wtb_dat$age_group))
  )

## 5) Optional: observed %YES (faint hollow points) within each age_group × diet ----
obs_df <- wtb_dat %>%
  dplyr::group_by(age_group, diet) %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_obs = 100 * mean(willingness_to_buy == 1L),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    diet = factor(diet, levels = levels(wtb_dat$diet)),
    age_group = factor(age_group, levels = levels(wtb_dat$age_group))
  )

## 6) Colors (Okabe–Ito, color-blind friendly) ----
okabe_ito <- c("#E69F00","#56B4E9","#009E73","#0072B2",
               "#D55E00","#CC79A7","#F0E442","#000000")
diet_levels <- levels(wtb_dat$diet)
cols <- setNames(okabe_ito[seq_len(min(length(diet_levels), length(okabe_ito)))], diet_levels)

## 7) Build annotation data (repeat text across facets, placed top-left) ----
ann_df <- data.frame(
  age_group = levels(wtb_dat$age_group),
  x = levels(wtb_dat$diet)[1],
  y = 98,
  label = paste0("Diet: ", diet_label, " | Age group: ", age_label)
)

## 8) Single PANEL figure: facet by age_group; diet on x (Dot + CI) ----
p_facet_age <- ggplot2::ggplot(pp_df, ggplot2::aes(x = diet, y = pct, colour = diet)) +
  # CI
  ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), linewidth = 1.0) +
  # model-predicted point
  ggplot2::geom_point(size = 3.8) +
  # observed point (faint hollow) - comment out if you prefer model-only
  ggplot2::geom_point(
    data = obs_df,
    ggplot2::aes(x = diet, y = pct_obs),
    inherit.aes = FALSE,
    shape = 21, fill = "white", colour = "grey35",
    size = 2.9, stroke = 0.9, alpha = 0.85
  ) +
  ggplot2::facet_wrap(~ age_group, nrow = 1) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    x = "Diet (treatment)",
    y = "Predicted willingness to buy (Yes, %)"
  ) +
  # overall annotation shown inside each facet (top-left)
  ggplot2::geom_text(
    data = ann_df,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,
    fontface = "italic",
    size = 3.4
  ) +
  ggplot2::theme_classic(base_size = 13) +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold"),
    plot.margin = ggplot2::margin(8, 10, 8, 10)
  )

print(p_facet_age)

## 9) Export tables + figure ----
writexl::write_xlsx(
  list(
    PredProb_Diet_by_AgeGroup = pp_df,
    ObservedPct_Diet_by_AgeGroup = obs_df,
    TypeIII_LR_Tests = a3_df
  ),
  path = "WTB_PredProb_FacetByAgeGroup.xlsx"
)

ggplot2::ggsave("Fig_WTB_PredProb_FacetByAgeGroup.png", p_facet_age, width = 12.0, height = 4.6, dpi = 600)
ggplot2::ggsave("Fig_WTB_PredProb_FacetByAgeGroup.pdf", p_facet_age, width = 12.0, height = 4.6, dpi = 600)

message("Done. Saved in: ", getwd(),
        "\n- WTB_PredProb_FacetByAgeGroup.xlsx",
        "\n- Fig_WTB_PredProb_FacetByAgeGroup.png",
        "\n- Fig_WTB_PredProb_FacetByAgeGroup.tiff")

