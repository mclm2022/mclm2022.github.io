# Code for the "Association measures" slides

# ##############################################################################
# ==============================================================================
# Load packages ################################################################
library(tidyverse)
library(mclm)

# Load data ####################################################################
path_to_corpus <- here("studies", "_corpora", "brown") # adapt

## List filenames ==============================================================
brown_fnames <- get_fnames(path_to_corpus) %>% keep_re("/c[a-z]")

# Create association scores object #############################################
hot_assoc <- surf_cooc(brown_fnames, "^hot/jj", re_token_splitter = "\\s+") %>% 
  assoc_scores() %>% as_tibble()

# Effect size measures #########################################################
## Sort based on Delta P =======================================================
hot_assoc %>% arrange(desc(DP_rows)) %>% 
  select(type, a, b, c, d, DP_rows) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on Relative Risk==================================================
hot_assoc %>% arrange(desc(RR_rows)) %>% 
  select(type, a, b, c, d, RR_rows) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on Odds Ratio ====================================================
hot_assoc %>% arrange(desc(OR)) %>% 
  select(type, a, b, c, d, OR) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on Log Odds Ratio ================================================
hot_assoc %>% 
  mutate(log_OR = log(OR)) %>% # compute log odds ratio
  arrange(desc(log_OR)) %>% 
  select(type, a, b, c, d, OR, log_OR) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on PMI ===========================================================
hot_assoc %>% arrange(desc(PMI)) %>% 
  select(type, a, exp_a, PMI) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on DICE coefficient ==============================================
hot_assoc %>% arrange(desc(dICE)) %>% 
  select(type, a, b, c, d, dICE) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

# Strength of evidence measures ################################################
## Sort based on chi-squared ===================================================
hot_assoc %>% arrange(desc(chi2_signed)) %>% 
  select(type, a, exp_a, b, c, d, chi2_signed) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on log-likelihood ratio ==========================================
hot_assoc %>% arrange(desc(G_signed)) %>% 
  select(type, a, exp_a, b, c, d, G_signed) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)

## Sort based on Fisher exact test =============================================
hot_assoc %>% arrange(p_fisher_1) %>% 
  select(type, a, b, c, d, p_fisher_1) %>% head(10) %>% 
  kbl() %>% kable_paper(font_size = 22)
