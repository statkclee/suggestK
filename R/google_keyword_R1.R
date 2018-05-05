# Google Suggest Search Terms - Search Level 1
#
# The function aims to crawl the first level related search terms from the Google search engine.
# The only argument for this function is "Search Term".
# It refer to the Google Suggest API.
#
# GOOGLE url: https://www.google.com/

google_keyword_R1 <- function(keyword) {

    gsuggest_url <- "http://suggestqueries.google.com/complete/search?output=toolbar&q="
    search_keyword <- glue(gsuggest_url, keyword)

    gsuggest_resp <- GET(search_keyword) %>%
      content()

    gsuggest_R1_df <- gsuggest_resp %>%
      xml_nodes('CompleteSuggestion') %>%
      xml_node('suggestion') %>%
      map(xml_attrs) %>%
      map_df(~as.list(.))

    return(gsuggest_R1_df)
}
