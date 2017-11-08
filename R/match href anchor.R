
#' @export
match_href_anchor <- function(h) {

  h <- h %>%
    html_nodes("a")

  h_chr <- h %>%
    html_text()

  urls <- h %>%
    html_attr("href")

  h_df <- map2(h_chr, urls,
               ~ c(.x, .y) %>%
                 enframe() %>%
                 spread(name, value)) %>%
    bind_rows() %>%
    `colnames<-`(c("raw", "url"))

  h_df

}
