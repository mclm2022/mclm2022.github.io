# Code for the "logistic regression analysis" slides

# Libraries ####################################################################
library(tidyverse)
library(kableExtra)
library(ggeffects)
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
gonna <- tibble(variant = rep("gonna", 50), comp_length = rnorm(50, 6, 0.8),
                register = sample(c("formal", "informal"), 50, replace = TRUE, prob = c(0.1, 0.9)))
going_to <- tibble(variant = rep("going_to", 50), comp_length = rnorm(50, 3, 1.2),
                   register = sample(c("formal", "informal"), 50, replace = TRUE, prob = c(0.7, 0.3)))
gt <- bind_rows(gonna, going_to) %>%
  mutate(
    comp_length = if_else(comp_length > 1, comp_length, 1),
    variant = fct_relevel(variant, "gonna"),
    variant_num = as.numeric(variant)-1,
    register = fct_relevel(register, "informal"),
    source = factor(sample(letters, n(), replace = TRUE))
  )
m1 <- glm(variant ~ comp_length, data = gt, family = binomial(logit))
gt$fit1 <- m1$fitted.values
m2 <- glm(variant ~ comp_length + register, data = gt, family = binomial(logit))
gt$fit2 <- m2$fitted.values
## Plot ========================================================================
gonna_plot <- ggplot(gt, aes(x = comp_length, y = variant_num)) +
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
  geom_line(aes(y = fit1), color = "goldenrod", size = 2)

## Model =======================================================================
summary(m2)

### extract intercept ----
intercept <- m2$coefficients[[1]] # log odds
intercept
odds <- exp(intercept)
odds # odds
odds/(odds + 1) # probabilities

### extract a predictor coefficient
cl_lor <- m2$coefficients[["comp_length"]] # log odds ratio
exp(cl_lor) # odds ratio

cl_reg <- m2$coefficients[["registerformal"]]
exp(cl_reg)

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

## Plot ========================================================================
lnplot(with_x, x, logit, "logit ~ X")
lnplot(with_x, x, P, "P ~ X")

# Evaluation ###################################################################
set.seed(0)
pred_power <- tibble(
  gt0 = sample(filter(gt, variant == "gonna")$fit2, 15, replace = FALSE),
  gt1 = sample(filter(gt, variant == "going_to")$fit2, 15, replace = FALSE)) %>% 
  mutate(C = gt1 > gt0, D = gt0 > gt1, T = gt1 == gt0)
pred_power
# Interactions #################################################################
m3 <- glm(variant ~ comp_length*register, data = gt, family = binomial(logit))
summary(m3)
ggpredict(m3, c("comp_length", "register"))

# Random effects ###############################################################
m4 <- lme4::glmer(variant ~ comp_length*register + (1 | source), data = gt,
                  family = binomial)
summary(m4)

## Comparison ==================================================================
performance::compare_performance(m1, m2, m3, m4, metrics = "AIC")
