abcd_o <- function(target, reference,
                   target_item = "Target item",
                   others = "Other items") {
  tibble::tibble(
    rows = c(target, reference, "Total"),
    colloc = c("O<sub>11</sub>", "O<sub>21</sub>", "C<sub>1</sub>"),
    not_colloc = c("O<sub>12</sub>", "O<sub>22</sub>", "C<sub>2</sub>"),
    total = c("R<sub>1</sub>", "R<sub>2</sub>", "N")
  ) %>% 
    kableExtra::kbl(col.names = c(" ", target_item, others, "Total"),
        escape = FALSE) %>%
    kableExtra::kable_styling()
}
