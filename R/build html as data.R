#' @export

build_html_as_data <- function(base_path, join_urls = T) {
  nms <- list.files(base_path)

  list.files(base_path, full.names = T) %>%
    map2(nms,  ~ extract_each(.x, .y, join_urls = join_urls)) %>%
    bind_rows()
}



extract_each <- function(h, file.name, join_urls) {

  message(file.name)

  h_r <- h %>%
    read_html()

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


  if (join_urls) {
    # could remove dual use of read html function
    if (join_urls) urls_text_df <- match_href_anchor(h_r) %>%
        filter(raw != "")
    df2 <- left_join(df, urls_text_df, by = "raw") %>%
      distinct(title, row.num, .keep_all = T)
    df2
  } else {
    df
  }

}

