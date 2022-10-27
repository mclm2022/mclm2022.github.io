# Code for the "Contingency tables" slides

# Load packages ################################################################
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

## Print =======================================================================
print(flist, n = 5)

# Explore hot ##################################################################
flist %>% keep_re("^hot/")

## Get concordance =============================================================
hot <- conc(brown_fnames, "\\bhot/jj")
hot

hot %>% 
  arrange(right) %>% 
  print_kwic(from = 25, n = 15)

## Get surface cooccurrences ===================================================
hot_cooc <- surf_cooc(brown_fnames, "^hot/jj", re_token_splitter = "\\s+")

## Explore surface cooccurrences ===============================================
head(hot_cooc$target_freqlist, 7)
head(hot_cooc$ref_freqlist, 7)
map(hot_cooc, keep_re, "^coffee/")

## Obtain association scores ===================================================
assoc_scores(hot_cooc)
assoc_scores(hot_cooc)["coffee/nn",]
