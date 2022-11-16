# Code for the "Factor analysis" slides

# Libraries ####################################################################
library(tidyverse)
library(GGally) # to plot correlogram
library(xml2) # to read examples from files
path <- here::here("studies", "register-analysis.tsv")
# path <- "https://github.com/mclm2022/mclm2022.github.io/studies/register-analysis.tsv)"

# Dataset and analysis #########################################################

# load data
dataset <- read_tsv(path, show_col_types = FALSE)

# Factor Analysis
fa <- dataset %>% data.frame(row.names = "filename") %>% 
  as.matrix() %>% factanal(factors = 4, scores = "regression")
fa

# Interpretation ###############################################################
## Correlogram with GGally =====================================================
library(GGally)
dataset %>%
  select(word_len, p_nomin, p_ppss, p_pobi, ttr) %>% ggpairs()

## Factors 1 and 2 =============================================================
### Get loadings ----
fa_loads <- unclass(loadings(fa)) %>% 
  as_tibble(rownames = "Variable") %>% 
  mutate(across(where(is.numeric), ~ if_else(abs(.x) < 0.3, NA_real_, .x))) %>% 
  filter(!is.na(Factor1) | !is.na(Factor2))
kbl(fa_loads[,1:3]) %>% kable_paper(font_size = 22)

### Get scores data ----
scores <- as_tibble(fa$scores, rownames = "File")

### Prepare centroids of registers ----
brown_mapping_file <- here::here("resources", "brown_files.tsv")
# brown_mapping_file <- "https://github.com/mclm2022/mclm2022.github.io/resources/brown_files.tsv"
brown_mapping <- read_tsv(brown_mapping_file, show_col_types = FALSE) %>%
  select(Component, Register) %>%
  deframe()

register_centroids <- scores %>%
  mutate(Component = str_extract(File, "[a-z]"), Register = brown_mapping[Component]) %>%
  group_by(Register) %>%
  summarize(Factor1 = mean(Factor1), Factor2 = mean(Factor2))

### Get most extreme values ----
top_1 <- register_centroids %>% arrange(Factor1) %>%
  tail(2) %>% summarize(across(where(is.numeric), mean))
top_2 <- register_centroids %>% arrange(Factor2) %>%
  tail(2) %>% summarize(across(where(is.numeric), mean))

### Plot scores and centroids with annotation ----
ggplot(scores, aes(x = Factor1, y = Factor2)) +
  geom_text(aes(label = File), color = "gray", size = 6, alpha = 0.8) +
  geom_point(data = register_centroids, size = 6) +
  annotate("segment", x = 1.5, y = 1, xend = top_1$Factor1, yend = top_1$Factor2) +
  annotate("label", x = 1.5, y = 1, label = "Miscellaneous & Learned", size = 8) +
  annotate("segment", x = -0.8, y = 3, xend = top_2$Factor1, yend = top_2$Factor2) +
  annotate("label", x = -0.8, y = 3, label = "Romance and Mystery", size = 8) +
  theme_minimal(base_size = 23) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_vline(xintercept = 0, color = "gray", linetype = 3)

## Representative examples =====================================================
get_random_s <- function(filepath) {
  read_xml(filepath) %>% 
    xml_find_all("//d1:s") %>% 
    sample(1) %>% 
    xml_children() %>% 
    xml_text() %>% 
    paste(collapse = " ")
}
filepath <- function(file) here::here("studies", "_corpora", "brown_tei", paste0(file, ".xml"))
set.seed(5)
# Replace Factor1 with Factor2 below to get extremes of Factor 2
right_1 <- filter(scores, Factor1 == max(Factor1))$File
left_1 <- filter(scores, Factor1 == min(Factor1))$File

# get example with code below; replace right_1 with left_1 to get leftmost file
get_random_s(filepath(right_1))

# Data extraction (summary) ####################################################
## Loadings ====================================================================
options(knitr.kable.NA = "") # so NA values are not printed if using kable(Extra)

fa_loads <- loadings(fa) %>% 
  unclass() %>% # remove "loadings" class
  as_tibble(rownames = "Variable") %>% 
  mutate(across(
    where(is.numeric),
    ~ if_else(abs(.x) < 0.3, NA_real_, .x))
  ) # change the 0.3 to another cutoff value if you want

## Uniquenesses ====================================================
# enframe turns a named vector into a tibble
uniqueness <- fa$uniquenesses %>% 
  enframe(name = "Variable", value = "Uniqueness") %>% 
  arrange(desc(Uniqueness))

## Variance ====================================================================
### custom function ----
factanal_variances <- function(loadings) {
  # obtain variances from loadings
  vars <- colSums(loadings^2)/nrow(loadings)
  as_tibble_row(vars) %>%
    pivot_longer(
      everything(),
      names_to = "Factor",
      values_to = "Variance",
      names_transform = ~str_remove(.x, "Factor"))
}

### new model with 7 factors ----
fa7 <- dataset %>%
  data.frame(row.names = "filename") %>%
  as.matrix() %>% factanal(7)

### Scree plot of variances ----
### This helps figure out a good number of factors
loadings(fa7) %>% factanal_variances() %>% 
  ggplot(aes(x = Factor, y = Variance)) +
  geom_point(size = 7) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal(base_size = 35)

## Scores ======================================================================
scores <- as_tibble(fa$scores, rownames = "File")

### compute centroid as means of groups ----
centroids <- scores %>% 
  mutate(Register = str_extract(File, "[a-z]")) %>% 
  group_by(Register) %>% 
  summarize(across(where(is.numeric), mean))
