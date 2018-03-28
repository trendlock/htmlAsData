
#' @export

build_html_as_data <- function(h, file.name) {

  message(file.name)

  h_r <- h

  processed <- h_r %>%
    as.character() %>%
    str_split("\n") %>%
    unlist() %>%
    str_split("\t") %>%
    unlist() %>%
    str_trim() %>%
    .[. != ""]

  df <- tibble(title = file.name, row.num = seq_len(length(processed)), raw_html = processed)

  df <- mutate_html_tags(df, raw_html)

  # mutate text of found html anchors/links to help filter on
  urls_text_df <- match_href_anchor(h_r) %>%
    filter(raw != "")

  left_join(df, urls_text_df, by = "raw") %>%
    distinct(title, row.num, .keep_all = T)


}

