library(tidyverse)
library(mclm)
library(xml2)

corpus_directory <- here::here("studies", "_corpora", "brown_tei")

tei_fnames <- get_fnames(corpus_directory) %>% 
  keep_re(r"--[(?xi) / [a-z] \d\d [.]xml $ ]--")

short_names <- short_names(tei_fnames)

small_proportions <- function(pos_codes, n_tok) {
  pos_mapping <- c(p_ppss = "PPSS", p_adj = "JJ", p_neg = "[*]",
                   p_adv = "RB", p_qual = "QL")
  map_dbl(pos_mapping, ~n_tokens(pos_codes[re(.x)])/n_tok * 10000) %>% 
    as_tibble_row()
}

d <- as_tibble(tei_fnames) %>% 
  mutate(
    xml = map(filename, read_xml),
    ns = map(xml, xml_ns),
    token_tags = map2(xml, ns, ~ find_xpath(.x, "//d1:w | //d1:mw | //d1:c", namespaces = .y)),
    word_tags = map2(xml, ns, ~ find_xpath(.x, "//d1:w", namespaces = .y)),
    mw_tags = map2(xml, ns, ~ find_xpath(.x, "//d1:mw", namespaces = .y)),
    punctuation_tags = map2(xml, ns, ~ find_xpath(.x, "//d1:c", namespaces = .y)),
    tokens = map(token_tags, ~ xml_text(.x) %>% tolower() %>% cleanup_spaces() %>% as_tokens()),
    words = map(word_tags, ~ xml_text(.x) %>% tolower() %>% cleanup_spaces() %>% as_tokens()),
    pos_codes = map(token_tags, function(toks) {
      most_pos <- xml_attr(toks, "type")
      mw_pos <- xml_attr(toks, "pos")
      most_pos[is.na(most_pos)] <- mw_pos[!is.na(mw_pos)]
      as_tokens(most_pos)
    }),
    bigrams = map(tokens, ~ paste(.x, c(.x[-1], "EOF"), sep = "|") %>% as_tokens()),
    pos_bigrams = map(pos_codes, ~ paste(.x, c(.x[-1], "EOF"), sep = "|") %>% as_tokens()),
    n_tok = map_dbl(tokens, n_tokens),
    ttr = map_dbl(tokens, n_types)/n_tok,
    p_bigr = map_dbl(bigrams, n_types) / n_tok,
    p_pobi = map_dbl(pos_bigrams, n_types) / n_tok,
    p_mw = map_dbl(mw_tags, length) / n_tok * 10000,
    p_c = map_dbl(punctuation_tags, length) / n_tok,
    word_len = map_dbl(words, ~ sum(nchar(.x))/n_tokens(.x)),
    p_nomin = map_dbl(tokens, ~n_tokens(.x[re("..+(tion|ment|ness|ity)$")])) / n_tok * 10000,
    p_noun = map_dbl(pos_codes, ~ n_tokens(.x[re("NN")])) / n_tok,
    sp = map2(pos_codes, n_tok, small_proportions)) %>% 
  unnest(sp) %>% 
  select(ttr, word_len, starts_with("p_")) %>% 
  as.matrix()
rownames(d) <- short_names

d_fa <- factanal(d, 4, scores = "regression")
d_fa
factanal(d, 4, scores = "regression",
         rotation = "promax")
loadings(d_fa) %>% 
  unclass() %>% 
  as_tibble(rownames = "variable")
scores <- as_tibble(d_fa$scores, rownames = "file") %>% 
  mutate(component = re_retrieve_first(file, "[a-z]"))
plots <- map(unique(scores$component), function(comp) {
  this_comp <- filter(scores, component == comp)
  other_comps <- filter(scores, component != comp)
  ggplot(other_comps, aes(x = Factor1, y = Factor2)) +
    geom_point(shape = 3, color = "gray") +
    geom_text(data = this_comp, label = comp, color = "darkblue") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed")
})

# show plot_grid?