# Code for the "logistic regression analysis" slides

# Libraries ####################################################################
library(tidyverse)
library(kableExtra)
options(digits = 3) # print up to 3 digits

# Custom functions #############################################################
lnplot <- function(df, x, y, title) {
  ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_line() +
    geom_point() + 
    theme_minimal(base_size = 20) +
    theme(aspect.ratio = 1) +
    labs(title = title)
}
kb_style <- function(df) {
  df %>% head(10) %>% kbl() %>%
    kable_paper(full_width = FALSE, font_size = 22)
}

# Logistic regression ##########################################################
## Set up data =================================================================
set.seed(2022)
gonna <- tibble(gt = rep("gonna", 50), comp_length = rnorm(50, 6, 0.8))
going_to <- tibble(gt = rep("going_to", 50), comp_length = rnorm(50, 3, 1.2))
gt <- bind_rows(gonna, going_to) %>%
  mutate(
    comp_length = if_else(comp_length > 1, comp_length, 1),
    gt = fct_relevel(gt, "gonna"), gt_num = as.numeric(gt)-1
  )
m3 <- glm(gt ~ comp_length, data = gt, family = binomial(logit))
gt$fit <- m3$fitted.values

## Plot ========================================================================
gonna_plot <- ggplot(gt, aes(x = comp_length, y = gt_num)) +
  geom_point(size = 3) + ylim(0, 1) +
  labs(x = "Length of complement", y = "Choice of construction") +
  scale_y_continuous(breaks = c(0, 1), labels = c("gonna", "going to")) +
  theme_minimal(base_size = 20) + theme(aspect.ratio = 1)
gonna_plot

### With linear fitting ----
gonna_plot + 
  geom_smooth(method = "lm", se = FALSE, color = "darkolivegreen4")

### With logistic fitting ----
gonna_plot +
  geom_line(aes(y = fit), color = "goldenrod", size = 2)

## Model =======================================================================
summary(m3)

# Probabilities, odds and logits ###############################################
## Set up simulated data =======================================================
library(MASS) # to print fractions
probabilities <- c(1/c(7:2), 1-(1/c(3:7)))
probs <- tibble(
  P = probabilities,
  P_frac = as.character(fractions(P)),
  odds = P/(1-P),
  odds_frac = as.character(fractions(odds)),
  logit = log(odds)
)
kbl(probs) %>% kable_paper(font_size = 22) %>% 
  row_spec(6, bold = TRUE)
with_x <- tibble(
  x = 1:30,
  logit = -3.5 + 0.3*x,
  odds = exp(logit),
  P = odds/(1+odds))

## Plot =======================================================================
lnplot(with_x, x, logit, "logit ~ X")
lnplot(with_x, x, P, "P ~ X")
