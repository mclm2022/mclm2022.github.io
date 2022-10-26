# Code for the "Reading and exploring a corpus" slides

# Load packages ################################################################
library(here)
library(tidyverse)
library(mclm)

# Load data ####################################################################
path_to_corpus <- here("studies", "_corpora", "brown") # adapt

## List filenames ==============================================================
brown_fnames <- get_fnames(path_to_corpus)

# Create a frequency list ######################################################
brown_fnames <- brown_fnames %>% 
  keep_re("/c[a-z]")
flist <- freqlist(brown_fnames, re_token_splitter = re("\\s+"))

## Print and check features ====================================================
print(flist, n = 3)
n_tokens(flist)
n_types(flist)

## Plot frequencies ============================================================
as_tibble(flist) %>% 
  ggplot(aes(x = rank, y = abs_freq)) +
  geom_point(alpha = 0.3) +
  theme_minimal()

as_tibble(flist) %>% 
  ggplot(aes(x = rank, y = abs_freq)) +
  geom_point(alpha = 0.3) +
  ggrepel::geom_text_repel(data = as_tibble(keep_bool(flist, flist > 10000)),
                           aes(label = type), xlim = c(0, NA)) +
  theme_minimal()

as_tibble(flist) %>%
  mutate(freq_range = case_when(
    abs_freq == 1 ~ "1",
    abs_freq <= 5 ~ "2-5",
    abs_freq <= 100 ~ "6-100",
    abs_freq <= 1000 ~ "101-1000",
    TRUE ~ "> 1000"
  ) %>% fct_reorder(abs_freq)) %>% 
  ggplot(aes(x = freq_range)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = ..count..))
