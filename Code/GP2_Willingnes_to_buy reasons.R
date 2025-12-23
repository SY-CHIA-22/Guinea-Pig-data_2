
## ============================================================
## HIGH-IMPACT JOURNAL SCRIPT (NO external CI packages)
## Reasons for willingness to buy (all YES)
## - Data: Respondent | Response | Reason
## - Analysis: counts, proportions, Wilson 95% CI
## - Plot: Dot + 95% CI (primary, journal-ready)
## - Exports: Excel + PNG + TIFF
## ============================================================

## 0) Packages ----
req <- c("tidyverse","janitor","writexl")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

## 1) Prepare data ----
## Columns: Respondent | Response | Reason

reasons <- wtb_reasons %>%
  janitor::clean_names() %>%
  mutate(
    response = toupper(trimws(response)),
    reason   = trimws(reason)
  )

stopifnot(all(c("respondent","response","reason") %in% names(reasons)))

# Keep YES only
reasons_yes <- reasons %>%
  filter(response == "YES", !is.na(reason), reason != "")

if(nrow(reasons_yes) == 0) stop("No valid YES responses with reasons found.")

# Standardise reason labels
reasons_yes <- reasons_yes %>%
  mutate(
    reason_std = case_when(
      str_detect(str_to_lower(reason), "taste")   ~ "Pleasant taste",
      str_detect(str_to_lower(reason), "health")  ~ "Perceived health benefits",
      str_detect(str_to_lower(reason), "novel|curio") ~ "Novelty/curiosity",
      str_detect(str_to_lower(reason), "sustain") ~ "Sustainable production method",
      TRUE ~ reason
    ),
    reason_std = factor(
      reason_std,
      levels = c(
        "Pleasant taste",
        "Perceived health benefits",
        "Novelty/curiosity",
        "Sustainable production method"
      )
    )
  )

## 2) Summary statistics ----
sum_tab <- reasons_yes %>%
  count(reason_std, name = "n") %>%
  mutate(
    N = sum(n),
    prop = n / N
  )

## 3) Wilson 95% confidence intervals (base R) ----
wilson_ci <- function(x, n, conf = 0.95){
  z <- qnorm(1 - (1 - conf)/2)
  p <- x / n
  denom <- 1 + z^2/n
  centre <- (p + z^2/(2*n)) / denom
  half <- z * sqrt((p*(1 - p) + z^2/(4*n)) / n) / denom
  c(lower = centre - half, upper = centre + half)
}

ci_mat <- t(mapply(wilson_ci, sum_tab$n, sum_tab$N))

sum_tab <- sum_tab %>%
  mutate(
    ci_low = ci_mat[,1],
    ci_high = ci_mat[,2],
    pct = 100 * prop,
    pct_low = 100 * ci_low,
    pct_high = 100 * ci_high,
    pct_ci = sprintf("%.1f%% (%.1f–%.1f)", pct, pct_low, pct_high)
  ) %>%
  arrange(desc(prop))


## 4) JOURNAL-READY PLOT: Horizontal BAR (no CI) ----
# Color-blind safe palette (Okabe–Ito)
okabe_ito <- c("#0072B2","#009E73","#D55E00","#CC79A7")
cols <- setNames(okabe_ito, levels(sum_tab$reason_std))

p_bar <- ggplot(
  sum_tab,
  aes(
    y = forcats::fct_reorder(reason_std, pct),
    x = pct,
    fill = reason_std
  )
) +
  
  # Bars = observed proportions
  geom_col(
    width = 0.65,
    colour = "black",
    linewidth = 0.4,
    alpha = 0.9
  ) +
  
  # Labels (n/N and %)
  geom_text(
    aes(label = paste0(n, "/", N, " (", sprintf("%.1f", pct), "%)")),
    hjust = -0.05,
    size = 3.6
  ) +
  
  scale_fill_manual(values = cols) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  
  labs(
    x = "Respondents selecting reason (%)",
    y = NULL,
    title = "Primary reasons for willingness to buy BSF oil-fed meat"
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(p_bar)


#EXPORT GRAPHS
ggsave("Fig_WTB_Reasons_Bar.png",
       p_bar, width = 8.5, height = 4.8, dpi = 600)

ggsave("Fig_WTB_Reasons_Bar.pdf",
       p_bar, width = 8.5, height = 4.8, dpi = 600)


# Other options for plotting ##############
req <- c("tidyverse","treemapify","patchwork")
to_install <- req[!req %in% rownames(installed.packages())]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

okabe_ito <- c("#0072B2","#009E73","#D55E00","#CC79A7")
cols <- setNames(okabe_ito, levels(sum_tab$reason_std))


## LOLIPOP OPTION
p_lollipop <- ggplot(
  sum_tab,
  aes(
    y = forcats::fct_reorder(reason_std, pct),
    x = pct,
    colour = reason_std
  )
) +
  geom_segment(
    aes(x = 0, xend = pct, yend = reason_std),
    linewidth = 1.2,
    colour = "grey70"
  ) +
  geom_point(size = 4.8) +
  geom_text(
    aes(label = paste0(n, "/", N, " (", sprintf("%.1f", pct), "%)")),
    hjust = -0.05,
    size = 3.6
  ) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  labs(
    x = "Respondents selecting reason (%)",
    y = NULL,
    title = "Primary reasons for willingness to buy BSF oil-fed meat"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(p_lollipop)


#TREEMAP OPTION
p_treemap <- ggplot(
  sum_tab,
  aes(
    area = pct,
    fill = reason_std,
    label = paste0(reason_std, "\n", sprintf("%.1f%%", pct))
  )
) +
  treemapify::geom_treemap(colour = "white", linewidth = 1) +
  treemapify::geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE,
    fontface = "bold",
    reflow = TRUE
  ) +
  scale_fill_manual(values = cols) +
  labs(title = "Motivations underlying willingness to buy") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_treemap)


## TWO PANEL OPTION WITH LOLIPOP AND TREAMAP
p_two_panel <- p_lollipop + p_treemap +
  patchwork::plot_layout(
    widths = c(2.2, 1),
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A",
    title = "Reasons for willingness to buy BSF oil-fed meat"
  )

print(p_two_panel)
