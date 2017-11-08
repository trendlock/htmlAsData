


#' @export

mutate_html_tags <- function(df, target_var) {
  vbl <- enquo(target_var)
  html_tag_regex <- "(?=\\<).*?(?<=\\>)"

  found_tags_df <- df %>%
    pull(!!vbl) %>%
    extract_html_tags_each(html_tag_regex)

  df <- df %>%
    mutate(raw = rm_and_clean_strings(!!vbl, html_tag_regex))

  bind_cols(df, found_tags_df)
}


rm_and_clean_strings <- function(x, regex_in) {

  x %>%
    str_replace_all("\\>\\<", "> <") %>%
    str_replace_all('\\"', "\\'") %>%
    str_replace_all(regex_in, "") %>%
    str_trim()

}

extract_html_tags_each <- function(x, regex_in) {

  x %>%
    map( ~ extract_html_tags(.x, regex_in)) %>%
    bind_rows()

}

extract_html_tags <- function(x, regex_in) {

  x <- str_replace_all(x, "\\>\\<", "> <")
  x <- str_replace_all(x, '\\"', "\\'")

  tags <- find_any_tags(x, regex_in)

  # returns empty tbl for string with no tags
  if (is_empty(tags)) return(tibble(tags_cl = NA, tags_open = NA))
  if (all(is.na(tags))) return(tibble(tags_cl = NA, tags_open = NA))

  # separates tags into open and closed
  # find closed tags if any and extract or give NA
  tag_cls_regex <- "\\</"
  split_tags <- sep_tags(tags, tag_cls_regex)

  tibble(tags_open = split_tags[["tag_open"]], tags_cl = split_tags[["tag_cl"]])

}

sep_tags <- function(x, tag_cls_regex) {

  has_closed <- str_detect(x, tag_cls_regex) %>% any()
  if (has_closed) {
    tag_cl <- x[str_detect(x, tag_cls_regex)] %>%
      str_c(collapse = " - ")
  } else {
    tag_cl <- NA
  }


  tag_open <- x[!str_detect(x, tag_cls_regex)] %>%
    str_c(collapse = " - ")

  if(is_empty(tag_open)) tag_open <- NA

  list(tag_open = tag_open, tag_cl = tag_cl)
}

find_any_tags <- function(x, regex_in) {

  test_res <- str_detect(x, "\\<|\\>")

  if (test_res) {
    str_extract_all(x, regex_in, "") %>%
      as.vector() %>%
      .[. != ""]
  } else {
    NA
  }
}
