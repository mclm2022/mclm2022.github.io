# Code for the "correspondence analysis" slides

# Libraries ####################################################################
library(tidyverse)
library(mclm)
library(kableExtra)
options(digits = 3) # print up to 3 digits

# Custom functions #############################################################
# for printing matrices with kableExtra
print_matrix <- function(mtx) {
  k <- kbl(mtx, booktabs = TRUE) %>% 
    kable_classic(full_width = FALSE, font_size = 20)
  if ("Sum" %in% colnames(mtx)) {
    k <- column_spec(k, ncol(mtx)+1, bold = TRUE)
  }
  if ("Sum" %in% rownames(mtx)) {
    k <- row_spec(k, nrow(mtx), bold = TRUE)
  }
  k
}

# Smoke dataset ################################################################
## Set up data =================================================================
data(smoke)
print_matrix(smoke)

## Correspondence analysis =====================================================
smoke_ca <- ca(smoke)
plot(smoke_ca)

## Matrices examples ===========================================================
# Original matrix
smoke_mtx <- as.matrix(smoke)
print_matrix(smoke_mtx)

# Correspondence matrix
prop.table(smoke_mtx) %>% 
  addmargins() %>% 
  print_matrix()

# Row profiles
addmargins(smoke_mtx, 1) %>% 
  prop.table(1) %>% 
  print_matrix()

# Row cloud
row_cloud <- addmargins(smoke_mtx, 1) %>% prop.table(1) %>% 
  as_tibble(rownames = "Employee") %>%
  mutate(Employee = if_else(Employee == "Sum", "Centroid", Employee))
row_cloud %>% ggplot(aes(x = none, y = light, label = Employee)) +
  geom_text(size = 10) +
  theme_minimal(base_size = 20) +
  annotate("point",
           x = row_cloud$none[[nrow(row_cloud)]],
           y = row_cloud$light[[nrow(row_cloud)]],
           shape = 1, size = 25)

# Column profiles
addmargins(smoke_mtx, 2) %>% 
  prop.table(2) %>% 
  print_matrix()

# Column cloud
col_cloud <- addmargins(smoke_mtx, 2) %>% prop.table(2) %>% 
  t() %>% 
  as_tibble(rownames = "Smoker") %>%
  mutate(Smoker = if_else(Smoker == "Sum", "Centroid", Smoker))
col_cloud %>% ggplot(aes(x = SM, y = SC, label = Smoker)) +
  geom_text(size = 8) +
  theme_minimal(base_size = 20) +
  annotate("point",
           x = col_cloud$SM[[nrow(col_cloud)]],
           y = col_cloud$SC[[nrow(col_cloud)]],
           shape = 1, size = 25)

## Interpretation ==============================================================
smoke_ca
smoke_ca$rowmass
smoke_ca$rowdist
smoke_ca$rowinertia

### rowmass ----
addmargins(smoke_mtx, 1) %>% 
  prop.table(1) %>% 
  print_matrix()
smoke_ca$colmass

addmargins(smoke_mtx, 2) %>% 
  prop.table(2) %>% 
  print_matrix()

smoke_ca$rowmass

### euclidean distance
sqrt(sum((prop.table(smoke_mtx,1)["SM",]-smoke_ca$colmass)^2))

### chi-squared distance
sqrt(sum((prop.table(smoke_mtx,1)["SM",]-smoke_ca$colmass)^2/smoke_ca$colmass))

### rowdist ----
row_dists <- map_dbl(smoke_ca$rownames, function(row) {
  sqrt(sum((prop.table(smoke_mtx,1)[row,]-smoke_ca$colmass)^2/smoke_ca$colmass))
})
row_dists

smoke_ca$rowdist

### rowinertia ----
smoke_ca$rowdist ^ 2 * smoke_ca$rowmass

smoke_ca$rowinertia
smoke_ca
sum(smoke_ca$rowinertia)
sum(smoke_ca$colinertia)

### summary() ----
smoke_sum <- summary(smoke_ca)
smoke_sum
round(smoke_ca$rowmass*1000)
round(smoke_ca$rowinertia/sum(smoke_ca$rowinertia)*1000)
round(smoke_ca$colinertia/sum(smoke_ca$colinertia)*1000)
rows <- summary(smoke_ca)$rows
colnames(rows) <- c("row", "mass", "quality", "inertia",
                    paste0(rep(c("k", "cor", "ctr"), 2), rep(c(1, 2), each = 3)))
rows <- as_tibble(rows)

rows %>% print_matrix()

rows %>% select(row, quality, cor1, cor2) %>% 
  mutate(cors = cor1+cor2, qdiff = quality-cors) %>% 
  print_matrix()
plot(smoke_ca)
## Chi-squared test ============================================================
smoke_chisq <- chisq.test(smoke)
smoke_chisq

print_matrix(smoke_chisq$expected)
print_matrix(smoke_chisq$residuals)

# Trump-Clinton dataset ########################################################
# (See also corresponding study)

## Setup =======================================================================
## get corpus documents
corpus_folder <- here::here("studies", "_corpora", "clinton_trump")
fnames <- get_fnames(corpus_folder)
short_fnames <- short_names(fnames)

## get possible features
stop_list <- read_types(here::here("studies", "assets", "ca-trump-clinton", "stop_list.txt"))
features <- freqlist(fnames) %>% drop_types(stop_list) %>%
  keep_pos(1:150) %>% as_types()

## Contingency table / matrix ==================================================
d <- map(setNames(fnames, short_fnames), ~ freqlist(.x)[features]) %>%
  bind_cols() %>% data.frame(row.names = features) %>% 
  as.matrix() %>% t() %>% drop_empty_rc()
kbl(d[1:10, 1:10]) %>% kable_paper(font_size = 20)

## Correspondence analysis =====================================================
d_ca <- ca(d)
summary(d_ca)
plot(d_ca)

## Custom plot =================================================================
texts_df <- row_pcoord(d_ca)[,c(1, 2)] %>% 
  as_tibble(rownames = "text") %>% 
  mutate(Subcorpus = re_retrieve_first(fnames, "/clinton_trump/([^/]+)", requested_group = 1))

words_df <- col_pcoord(d_ca)[,c(1, 2)] %>% 
  as_tibble(rownames = "word")

dim_1 <- sprintf("Dimension 1 (%.2f %%)", summary(d_ca)$scree[1,3])
dim_2 <- sprintf("Dimension 2 (%.2f %%)", summary(d_ca)$scree[2,3])

ggplot(words_df, aes(x = V1, y = V2)) +
  geom_text(aes(label = word), color = "gray60") +
  geom_point(data = texts_df, aes(color = Subcorpus)) +
  scale_color_manual(values = c("#0000CD","#DC143C")) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_vline(xintercept = 0, color = "darkgray") +
  theme_bw(base_size = 12) +
  labs(x = dim_1, y = dim_2) +
  coord_fixed()