## =========================================================
## MORPHOMETRICS + PANEL FIGURES (Nature Food–style, print-safe)
## - Mixed models: trait ~ diet * sex * period + (1|replicate)
## - Outputs: panel growth trajectories, PCA panel, significance map (supplement)
## - Print-safe aesthetics: black/grey, linetype & shape (no reliance on color)
## =========================================================

## 0) Packages (install if missing)
pkgs <- c("readxl","janitor","stringr","ggplot2","scales",
          "dplyr","tidyr","purrr","readr","tibble",
          "lme4","lmerTest","emmeans",
          "patchwork")  # <- panel assembly
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

## 1) READ DATA (EDIT FILE/PATH)
setwd("C:/Guinea-Pig-data_2/Data")
file_name <- "GP2_MORPHOMETRICS.xlsx"   # << EDIT if needed
morph_raw <- readxl::read_excel("GP2_MORPHOMETRICS.xlsx") %>% janitor::clean_names()
cat("Columns read:\n"); print(names(morph_raw))

## 2) MAP COLUMNS robustly (edit mapping only if needed)
morph <- morph_raw %>%
  dplyr::rename(
    diet      = dplyr::any_of(c("diet")),
    sex       = dplyr::any_of(c("sex")),
    replicate = dplyr::any_of(c("replicate","animal","animal_id","id","rep")),
    period    = dplyr::any_of(c("period","time","phase","stage")),
    height    = dplyr::any_of(c("height")),
    width     = dplyr::any_of(c("width")),
    length    = dplyr::any_of(c("length")),
    girth     = dplyr::any_of(c("girth"))
  )

required <- c("diet","sex","replicate","period","height","width","length","girth")
missing_req <- setdiff(required, names(morph))
if(length(missing_req) > 0){
  stop("Missing columns after mapping: ", paste(missing_req, collapse=", "),
       "\nAvailable columns: ", paste(names(morph_raw), collapse=", "),
       "\nFix by editing the rename() mapping in Section 2.")
}

traits <- c("height","width","length","girth")

## 3) CLEAN DATA (Period labels + numeric traits)
morph <- morph %>%
  dplyr::mutate(
    diet      = as.factor(diet),
    sex       = as.factor(sex),
    replicate = as.factor(replicate),
    period    = toupper(stringr::str_trim(as.character(period))),
    period    = dplyr::case_when(
      stringr::str_detect(period, "INIT|BASE|PRE") ~ "INITIAL",
      stringr::str_detect(period, "FIN|POST|END")  ~ "FINAL",
      TRUE ~ period
    ),
    period = factor(period, levels = c("INITIAL","FINAL"))
  ) %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(traits), ~ suppressWarnings(as.numeric(.))))

if(any(is.na(morph$period))){
  cat("\nUnique raw period values:\n"); print(unique(morph_raw$period))
  stop("Some Period values could not be mapped to INITIAL/FINAL. Fix Period labels in Excel or mapping.")
}

cat("\nN rows:", nrow(morph), "\n")
cat("N animals:", dplyr::n_distinct(morph$replicate), "\n")
cat("Period counts:\n"); print(table(morph$period, useNA="ifany"))

## 4) JOURNAL THEME (print-safe; consistent across panels)
theme_journal <- ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face="bold"),
    plot.subtitle = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(face="bold"),
    axis.text  = ggplot2::element_text(color="black"),
    axis.line  = ggplot2::element_line(linewidth=0.4),
    axis.ticks = ggplot2::element_line(linewidth=0.4),
    legend.position = "top",
    legend.title = ggplot2::element_text(face="bold"),
    strip.background = ggplot2::element_rect(fill="white", color="black", linewidth=0.4),
    strip.text = ggplot2::element_text(face="bold")
  )

## 5) MIXED MODELS (Diet × Sex × Period) per trait
fit_trait <- function(tr){
  fml <- stats::as.formula(paste0(tr, " ~ diet * sex * period + (1|replicate)"))
  lmerTest::lmer(fml, data = morph, REML = TRUE, na.action = na.omit)
}
mods <- stats::setNames(lapply(traits, fit_trait), traits)

## ANOVA tables (fixed effects) — tidy
anova_tables <- purrr::imap(mods, function(m, tr){
  a <- as.data.frame(anova(m))  # lmerTest ANOVA
  a$trait <- tr
  a$term  <- rownames(a)
  rownames(a) <- NULL
  dplyr::select(a, trait, term, dplyr::everything())
}) %>% dplyr::bind_rows()

## 6) Create morph_wide (Δ, %Δ, indices)
morph_wide <- morph %>%
  dplyr::select(diet, sex, replicate, period, dplyr::all_of(traits)) %>%
  tidyr::pivot_wider(
    id_cols = c(replicate, diet, sex),
    names_from = period,
    values_from = dplyr::all_of(traits),
    names_sep = "_"
  )

need_cols <- as.vector(outer(traits, c("INITIAL","FINAL"), paste, sep="_"))
miss_cols <- setdiff(need_cols, names(morph_wide))
if(length(miss_cols) > 0){
  stop("Missing INITIAL/FINAL columns after pivot_wider: ",
       paste(miss_cols, collapse=", "),
       "\nCheck that each animal has both INITIAL and FINAL rows.")
}

for(tr in traits){
  ini <- paste0(tr, "_INITIAL"); fin <- paste0(tr, "_FINAL")
  morph_wide[[paste0(tr, "_delta")]] <- morph_wide[[fin]] - morph_wide[[ini]]
  morph_wide[[paste0(tr, "_pct")]]   <- 100 * (morph_wide[[fin]] - morph_wide[[ini]]) / morph_wide[[ini]]
}

morph_wide <- morph_wide %>%
  dplyr::mutate(
    robustness_INITIAL  = girth_INITIAL / length_INITIAL,
    robustness_FINAL    = girth_FINAL   / length_FINAL,
    compactness_INITIAL = girth_INITIAL / height_INITIAL,
    compactness_FINAL   = girth_FINAL   / height_FINAL,
    elongation_INITIAL  = length_INITIAL / height_INITIAL,
    elongation_FINAL    = length_FINAL   / height_FINAL,
    
    robustness_delta  = robustness_FINAL  - robustness_INITIAL,
    compactness_delta = compactness_FINAL - compactness_INITIAL,
    elongation_delta  = elongation_FINAL  - elongation_INITIAL
  )

## 7) Growth trajectories (Diet × Period) using EMMs + 95% CI
## Color + linetype for diet (clear, color-blind safe, journal acceptable)

plot_growth_trajectories <- function(trait_name, y_units = "units") {
 
   diet_palette <- c(
    "#000000",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#CC79A7"
  )
  
  em <- emmeans::emmeans(mods[[trait_name]], ~ diet * period | sex)
  
  df <- as.data.frame(em) %>%
    dplyr::mutate(
      period = factor(period, levels = c("INITIAL","FINAL")),
      sex = as.factor(sex)
    )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = period,
      y = emmean,
      group = diet,
      color = diet,
      linetype = diet
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
      width = 0.08,
      linewidth = 0.6
    ) +
    ggplot2::facet_wrap(~sex, nrow = 1) +
    ggplot2::scale_color_manual(
      values = diet_palette,
      name = "Diet"
    ) +
    ggplot2::scale_linetype_discrete(
      name = "Diet"
    ) +
    ggplot2::labs(
      x = "Measurement period",
      y = paste0(stringr::str_to_title(trait_name), " (", y_units, ")"),
      title = stringr::str_to_title(trait_name)
    ) +
    theme_journal +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.2, "cm")
    )
}


## Set units once (edit)
UNIT_HEIGHT <- "cm"
UNIT_WIDTH  <- "cm"
UNIT_LENGTH <- "cm"
UNIT_GIRTH  <- "cm"

p_height <- plot_growth_trajectories("height", UNIT_HEIGHT)
p_width  <- plot_growth_trajectories("width",  UNIT_WIDTH)
p_length <- plot_growth_trajectories("length", UNIT_LENGTH)
p_girth  <- plot_growth_trajectories("girth",  UNIT_GIRTH)


## =========================================================
## E) PCA (shape changes): Δ traits + Δ indices
## + Diet centroids (mean ± SE)
## + PC1 loadings inset
## + Color-blind safe colors
## =========================================================

## 8) PCA of Δ shape (Δ traits + Δ indices) — COLOR + CLEAR CLUSTERING
pca_vars <- c("height_delta","width_delta","length_delta","girth_delta",
              "robustness_delta","compactness_delta","elongation_delta")

pca_df <- morph_wide %>%
  dplyr::select(diet, sex, replicate, dplyr::all_of(pca_vars)) %>%
  tidyr::drop_na()

if(nrow(pca_df) < 5){
  stop("Too few complete cases for PCA after drop_na(). Check missing data.")
}

X <- scale(pca_df[, pca_vars])
pca <- stats::prcomp(X, center = FALSE, scale. = FALSE)

scores <- as.data.frame(pca$x[,1:2]) %>%
  dplyr::bind_cols(pca_df %>% dplyr::select(diet, sex))

var_exp <- 100 * summary(pca)$importance[2,1:2]

## Color-blind safe palette (Okabe–Ito)
diet_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#CC79A7",
  "#0072B2", "#D55E00", "#F0E442", "#000000"
)

## Diet centroids (mean) for clearer clustering (per sex)
centroids <- scores %>%
  dplyr::group_by(sex, diet) %>%
  dplyr::summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")

## PCA plot: points colored by diet + centroid markers
p_pca <- ggplot2::ggplot(scores, ggplot2::aes(x = PC1, y = PC2, color = diet)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.25) +
  ggplot2::geom_point(size = 2.7, alpha = 0.85) +
  ## centroid overlay (bigger points with black outline)
  ggplot2::geom_point(
    data = centroids,
    ggplot2::aes(x = PC1, y = PC2, color = diet),
    inherit.aes = FALSE,
    size = 4.0,
    shape = 21,
    fill = "white",
    stroke = 0.9
  ) +
  ggplot2::facet_wrap(~sex) +
  ggplot2::scale_color_manual(values = diet_palette) +
  ggplot2::labs(
    x = paste0("PC1 (", round(var_exp[1], 1), "% variance)"),
    y = paste0("PC2 (", round(var_exp[2], 1), "% variance)"),
   title = "Shape change (Δ Final − Initial): PCA",
    #subtitle = "Points = animals; larger outlined points = diet centroids (mean) within sex",
    color = "Diet"
  ) +
  theme_journal +
  ggplot2::theme(legend.position = "bottom")

## Optional companion panel: Δ compactness — colored by diet, shaped by sex
p_index <- ggplot2::ggplot(morph_wide, ggplot2::aes(x = diet, y = compactness_delta, color = diet, shape = sex)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(width = 0.12, height = 0),
    size = 2.2,
    alpha = 0.80
  ) +
  ## mean as white-filled point with black border
  ggplot2::stat_summary(
    fun = mean,
    geom = "point",
    size = 3.6,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 0.9
  ) +
  ggplot2::scale_color_manual(values = diet_palette) +
  ggplot2::labs(
    x = "Diet",
    y = "Δ Compactness (girth/height)",
    title = "Conformation index change",
    #subtitle = "Points = animals (jittered); white points = diet means",
    color = "Diet",
    shape = "Sex"
  ) +
  theme_journal +
  ggplot2::theme(legend.position = "bottom")




## 9) Significance heatmap (better suited to Supplement)
heat_df <- anova_tables %>%
  dplyr::mutate(
    Trait = stringr::str_to_title(as.character(trait)),
    Effect = dplyr::recode(as.character(term),
                           "diet" = "Diet",
                           "sex" = "Sex",
                           "period" = "Period (Initial vs Final)",
                           "diet:sex" = "Diet × Sex",
                           "diet:period" = "Diet × Period",
                           "sex:period" = "Sex × Period",
                           "diet:sex:period" = "Diet × Sex × Period",
                           .default = as.character(term)
    ),
    p = `Pr(>F)`,
    Sig = dplyr::case_when(
      is.na(p) ~ NA_character_,
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "†",
      TRUE      ~ "ns"
    ),
    p_lab = dplyr::case_when(
      is.na(p) ~ "",
      p < 0.001 ~ "<0.001",
      TRUE ~ formatC(p, format = "f", digits = 3)
    )
  ) %>%
  dplyr::select(Trait, Effect, Sig, p_lab)

effects_order <- c("Diet","Sex","Period (Initial vs Final)",
                   "Diet × Sex","Diet × Period","Sex × Period","Diet × Sex × Period")

heat_df <- heat_df %>%
  dplyr::mutate(
    Effect = factor(Effect, levels = effects_order),
    Trait  = factor(Trait, levels = unique(Trait)),
    Sig = factor(Sig, levels = c("***","**","*","†","ns"))
  )

p_heat <- ggplot2::ggplot(heat_df, ggplot2::aes(x = Effect, y = Trait, fill = Sig)) +
  ggplot2::geom_tile(color = "black", linewidth = 0.25) +
  ggplot2::geom_text(ggplot2::aes(label = Sig), size = 4.6, fontface = "bold") +
  ggplot2::geom_text(ggplot2::aes(label = p_lab), size = 3.0, vjust = 2.0) +
  ggplot2::scale_fill_brewer(palette = "Greys", drop = FALSE, na.value = "white") +
  ggplot2::labs(
    x = "Model fixed effects",
    y = "Morphometric traits",
    title = "",
  
  ) +
  theme_journal +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, face="bold"),
    legend.position = "none"
  )

## =========================================================
## 10) PANEL ASSEMBLY with patchwork (Nature Food–style)
## =========================================================

## Main Figure candidate: 2×2 grid of growth trajectories
fig_growth_panel <- (p_height + p_width) / (p_length + p_girth) +
  patchwork::plot_annotation(
   # title = "Growth trajectories across diets and sexes",
  #  subtitle = "Points are estimated marginal means from mixed models; error bars show 95% confidence intervals",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", size=13),
      plot.subtitle = ggplot2::element_text(size=10)
    )
  ) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

## Mechanism Figure candidate: PCA + one index
fig_shape_panel <- p_pca + p_index +
  patchwork::plot_layout(widths = c(2.2, 1)) +
  patchwork::plot_annotation(
    #title = "Diet-associated morphometric shape change",
    subtitle = "Shape Change: PCA",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", size=13),
      plot.subtitle = ggplot2::element_text(size=10)
    )
  )


## Supplement Figure: significance map
fig_sigmap <- p_heat +
  patchwork::plot_annotation(
    title = "",
    subtitle = "† p<0.10; * p<0.05; ** p<0.01; *** p<0.001",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face="bold", size=13),
      plot.subtitle = ggplot2::element_text(size=10)
    )
  )

## Print to screen
print(fig_growth_panel)
print(fig_shape_panel)
print(fig_sigmap)

## =========================================================
## 11) EXPORT  (high-res TIFF; panel-ready)
## =========================================================
if(!dir.exists("outputs")) dir.create("outputs")

## Tables for reproducibility
readr::write_csv(anova_tables, "outputs/morph_ANOVA_tidy.csv")
readr::write_csv(heat_df, "outputs/morph_SigMap_data.csv")
readr::write_csv(as.data.frame(pca$rotation[,1:2]) %>% tibble::rownames_to_column("variable"),
                 "outputs/morph_PCA_loadings_PC1_PC2.csv")

## Panel figures (TIFF 600 dpi)
ggplot2::ggsave("outputs/Fig1_GrowthTrajectories_PANEL.tiff",
                fig_growth_panel, width = 180, height = 170, units = "mm",
                dpi = 600, compression = "lzw")
ggplot2::ggsave("outputs/Fig1_GrowthTrajectories_PANEL.pdf",
                 fig_growth_panel, width  = 180, height = 170, units  = "mm")


ggplot2::ggsave("outputs/Fig2_ShapeChange_PANEL.pdf",
                fig_shape_panel, width = 180, height = 110, units = "mm")

ggplot2::ggsave("outputs/FigS1_SignificanceMap.pdf",
                fig_sigmap, width = 180, height = 120, units = "mm")

cat("\n✅ DONE. Panel figures saved to 'outputs/' as pdf).\n")

