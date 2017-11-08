



df <- build_text_as_data()


#  run on all html pages

#raw_html_df <- speechFetch::build_text_as_data(find::this("/internal data storage/speech data/html"), raw_html = T)


html_tag_regex <- "(?=\\<).*?(?<=\\>)"

y <- "<p>"
y
str_extract_all(y, html_tag_regex)

y <- df$raw[6]
y
str_extract_all(y, html_tag_regex)

y <- df$raw[44]
y
str_extract_all(y, html_tag_regex)

y <- df$raw[123]
y
str_extract_all(y, html_tag_regex)

y <- df$raw[212]
y
tags <- str_extract_all(y, html_tag_regex) %>% unlist()

tags_cl <- tags[str_detect(tags, "/")]
tags_cl
tags_open <- tags[!str_detect(tags, "/")] %>%
  str_c(collapse = " - ")
tags_open


html_tag_regex <- "(?=\\<).*?(?<=\\>)"







mutate_html_tags <- function(df, target_var) {
  vbl <- enquo(target_var)
  html_tag_regex <- "(?=\\<).*?(?<=\\>)"

  found_tags_df <- df %>%
    pull(!!vbl) %>%
    extract_html_tags_each(html_tag_regex)

  bind_cols(df, found_tags_df)
}


rm_and_clean_strings <- function(x, regex_in) {

  x %>%
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

  tibble(raw = x, tags_open = split_tags[["tag_open"]], tags_cl = split_tags[["tag_cl"]])

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




# test runs debuggin

testx <- df %>%
  sample_n(100)
test <- testx %>%
  pull(raw)

found_tags <- test %>%
  extract_html_tags_each(html_tag_regex)


testx %>% find_any_tags(html_tag_regex)


str_replace_all("</script><script type='text/javascript'>", "\\>\\<", "> <")
"</form>"

