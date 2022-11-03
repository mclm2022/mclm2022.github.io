# Code for the "linear regression analysis" slides

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

# Simple linear regression #####################################################
## Set up dummy data ===========================================================
set.seed(2022)

utt_lengths <- tibble(age = 3:17) %>% 
  mutate(utterance_length = 3 + 0.5*age + rnorm(n(), sd = 0.3))

## Model data ==================================================================
m1 <- lm(utterance_length ~ age, data = utt_lengths)
utt_lengths$fit <- m1$fitted.values

utt_lengths %>% mutate(intercept = mean(utterance_length)) %>%
  slice_sample(n = 7) %>% kbl() %>%
  kable_paper(full_width = FALSE, font_size = 22)
summary(m1)

## Plot data ===================================================================
g <- ggplot(utt_lengths, aes(x = age, y = utterance_length)) +
  geom_point(size = 3) +
  labs(x = "Age", y = "Utterance length") +
  xlim(0,18) + ylim(0,15) +
  theme_minimal(base_size = 20) + theme(aspect.ratio = 1)

### Model with just intercept (=mean) ----
g_int <- g + geom_hline(aes(yintercept = mean(utterance_length)),
                        linetype = 2, color = "coral", size = 1)
g_int

### Sum of squares total ----
g_int +
  geom_segment(aes(xend = age, yend = mean(utterance_length)),
               color = "coral3", size = 2)

### Fitted model ----
g_fit <- g + geom_abline(intercept = m1$coefficients[[1]],
                         slope = m1$coefficients[[2]],
                         linetype = 2, color = "darkcyan", size = 1)
g_fit

### Sum of squares error ----
g_fit +
  geom_segment(aes(xend = age, yend = fit), color = "cyan4", size = 2)

# Multiple linear regression ###################################################
## Add predictor ===============================================================
set.seed(7)
utt_lengths <- utt_lengths %>% 
  mutate(pages_read = n()/2 + rnorm(n(), mean = 6, sd = 3))

## Model data ==================================================================
m2 <- lm(utterance_length ~ age + pages_read, data = utt_lengths)
utt_lengths$fit2 <- m2$fitted.values

utt_lengths %>% slice_tail(n = 7) %>% kbl() %>%
  kable_paper(full_width = FALSE, font_size = 22)
summary(m2)

## Add binary categorical predictor ============================================
utt_lengths_cat <- utt_lengths %>%
  mutate(mono = factor(c(rep("no", 5), "yes", "no", "no", rep("yes", 7))))

### Plot ----
ggplot(utt_lengths_cat, aes(x = as.numeric(mono)-1, y = utterance_length)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkolivegreen4") +
  labs(x = "Is the family monolingual?", y = "Utterance length") +
  ylim(c(0,15)) +
  scale_x_continuous(breaks = c(0, 1), labels = c("no", "yes")) +
  theme_minimal(base_size = 20) + theme(aspect.ratio = 1)

### Model ----
summary(lm(utterance_length ~ mono, data = utt_lengths_cat))

## Categorical predictor with more than two levels =============================
### Set up new dummy data ----
mean_utts <- c(EN = 11, ES = 7.4, ZH = 7.8) # mean per language
set.seed(2022)
by_lang <- tibble(L1 = factor(rep(c("EN", "ES", "ZH"), 15))) %>% 
  mutate(utterance_length = rnorm(n(), mean = mean_utts[L1], sd = 0.8))

### Model with English as reference level ----
m_lang1 <- lm(utterance_length ~ L1, data = by_lang)
summary(m_lang1)

### Model with Spanish as reference level ----
es_first <- mutate(by_lang, L1 = fct_relevel(L1, "ES"))
m_lang2 <- lm(utterance_length ~ L1, data = es_first)
summary(m_lang2)

