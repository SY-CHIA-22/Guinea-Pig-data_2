library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)

# -----------------------------
# 1. Read and clean data
# -----------------------------
setwd("C:/Guinea-Pig-data_2/Data")

df_raw <- read_excel("gp2_bodyweight_v2.xlsx", na = c("NA", "N/A", ""))

df <- df_raw %>%
  rename(
    Diet          = diet,
    Compartment   = compartment,
    ID_label      = id,
    Week          = week,
    Sex           = sex,
    InitialWeight = initial_weight,
    Weight        = weekly_weight
  ) %>%
  unite("AnimalID", Diet, Compartment, ID_label, Sex, remove = FALSE) %>%
  mutate(
    Diet          = factor(Diet),
    Compartment   = factor(Compartment),
    Sex           = factor(Sex),
    AnimalID      = factor(AnimalID),
    Week          = as.numeric(Week),
    InitialWeight = as.numeric(InitialWeight),
    Weight        = as.numeric(Weight)
  ) %>%
  mutate(
    Week_c = Week - mean(Week, na.rm = TRUE)
  )

# Optional: enforce order if needed
df$Diet <- factor(df$Diet, levels = c("T1", "T2", "T3", "T4"))
df$Sex  <- factor(df$Sex, levels = c("FEMALES", "MALES"))

# -----------------------------
# 2. Fit mixed-effects model
# -----------------------------
mod_plot <- lmer(
  Weight ~ Diet * Week_c + Sex + InitialWeight + (1 | AnimalID),
  data = df
)

summary(mod_plot)
anova(mod_plot)

# -----------------------------
# 3. Build prediction dataset
# -----------------------------
newdat <- expand.grid(
  Diet   = levels(df$Diet),
  Sex    = levels(df$Sex),
  Week   = seq(min(df$Week, na.rm = TRUE), max(df$Week, na.rm = TRUE), length.out = 100)
)

newdat$Week_c <- newdat$Week - mean(df$Week, na.rm = TRUE)
newdat$InitialWeight <- mean(df$InitialWeight, na.rm = TRUE)

# Fixed-effects predictions only
newdat$Pred <- predict(mod_plot, newdata = newdat, re.form = NA)

# -----------------------------
# 4. Extract sex p-value
# -----------------------------
anova_tab <- anova(mod_plot)
sex_p <- anova_tab["Sex", "Pr(>F)"]

sex_label <- if (sex_p < 0.001) {
  "Sex effect: p < 0.001"
} else {
  paste0("Sex effect: p = ", signif(sex_p, 3))
}

# -----------------------------
# 5. Plot
# -----------------------------
diet_cols <- c(
  "T1" = "#1f77b4",  # blue
  "T2" = "#1b9e77",  # green
  "T3" = "#d95f02",  # orange
  "T4" = "#cc79a7"   # pink/purple
)

sex_ltys <- c(
  "FEMALES" = "solid",
  "MALES"   = "dashed"
)

ggplot(newdat, aes(x = Week, y = Pred, colour = Diet, linetype = Sex)) +
  geom_line(linewidth = 1.8) +
  scale_colour_manual(values = diet_cols) +
  scale_linetype_manual(values = sex_ltys) +
  labs(
    x = "Week of feeding trial",
    y = "Predicted body weight (g)",
    colour = "Diet",
    linetype = "Sex"
  ) +
  annotate(
    "text",
    x = 1.0, y = max(newdat$Pred) - 10,
    hjust = 0, vjust = 1,
    size = 6,
    label = paste(
      "Mixed-effects model:",
      "Adjusted for initial body weight",
      sex_label,
      sep = "\n"
    )
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.background  = element_rect(fill = "grey92", colour = NA),
    panel.background = element_rect(fill = "grey92", colour = NA),
    legend.position = "top",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14),
    axis.line = element_line(linewidth = 1.2),
    axis.ticks = element_line(linewidth = 1.2)
  ) +
  guides(
    linetype = guide_legend(order = 1, override.aes = list(colour = "black")),
    colour   = guide_legend(order = 2)
  )