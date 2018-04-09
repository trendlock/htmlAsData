

h <- "/Users/rosseji/Dropbox (Trendlock)/Trendlock Team Folder/internal data storage/speech data/html test/doorstop-hong-kong_17483.html"

file.name <- "doorstop-hong-kong_17483"

message(h)

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

df <- tibble(
  title = file.name,
  row.num = seq_len(length(processed)),
  raw_html = processed
)

df <- mutate_html_tags(df, raw_html)


# mutate text of found html anchors/links to help filter on
urls_text_df <- match_href_anchor(h_r) %>%
  filter(raw != "")

df <- left_join(df, urls_text_df, by = "raw") %>%
  distinct(title, row.num, .keep_all = T)
