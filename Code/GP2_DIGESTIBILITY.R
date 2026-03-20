# --------------------------------------------
# UNREPLICATED DIGESTIBILITY: DESCRIPTIVE PLOTS
# --------------------------------------------
library(tidyverse)

# 1) Enter your single-measurement data
dig <- tribble(
  ~parameter,              ~`0%`,  ~`50%`,  ~`75%`,  ~`100%`,
  "Carbohydrates",          71.57,  88.06,   69.95,   63.71,
  "Crude fat",              69.98,  72.77,   76.69,   80.27,
  "Crude protein",          33.75,  41.71,   37.47,   47.66,
  "Metabolizable energy",   63.27,  61.61,   61.06,   63.07
)

# 2) Reshape + compute % change and index (control=100)
dig_long <- dig %>%
  pivot_longer(cols = c(`0%`,`50%`,`75%`,`100%`),
               names_to = "bsfo_level", values_to = "value") %>%
  mutate(
    bsfo_level = factor(bsfo_level, levels = c("0%","50%","75%","100%"))
  ) %>%
  group_by(parameter) %>%
  mutate(
    control = value[bsfo_level == "0%"][1],
    pct_change = (value - control) / control * 100,
    index_100  = (value / control) * 100
  ) %>%
  ungroup()

# -------------------------------
# FIGURE A: % change from control
# -------------------------------
fig_pct <- ggplot(dig_long %>% filter(bsfo_level != "0%"),
                  aes(x = bsfo_level, y = pct_change, group = parameter)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~ parameter, scales = "free_y") +
  theme_classic(base_size = 12) +
  labs(
    x = "BSFO inclusion level (replacement of soybean oil)",
    y = "Percent change from control (0% BSFO)",
    title = "Digestibility response to BSFO inclusion (descriptive, unreplicated)",
    subtitle = "Values shown as % change relative to control; no replication, no error bars."
  )

print(fig_pct)

# -------------------------------
# FIGURE B: Index plot (control=100)
# -------------------------------
fig_index <- ggplot(dig_long, aes(x = bsfo_level, y = index_100, group = parameter)) +
  geom_hline(yintercept = 100, linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  facet_wrap(~ parameter, scales = "free_y") +
  theme_classic(base_size = 12) +
  labs(
    x = "BSFO inclusion level (replacement of soybean oil)",
    y = "Index (control = 100)",
    title = "Digestibility index (control = 100) across BSFO inclusion levels",
    subtitle = "Descriptive comparison based on single measurements."
  )

print(fig_index)

# -------------------------------
# Save high-resolution (journal-ready)
# -------------------------------
ggsave("Fig_Digestibility_PercentChange.png", fig_pct, width = 9, height = 6, dpi = 600)
ggsave("Fig_Digestibility_Index100.png",      fig_index, width = 9, height = 6, dpi = 600)
