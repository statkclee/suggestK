#' Google Suggest Search Terms - Search Level 2
#'
#' The function aims to crawl the second level related search terms from the Google search engine.
#' The only argument for this function is "Search Terms".
#' This function retrieves the google_keyword_R1 function in order to get the related search terms from the intial search term.
#' It iterates crawling the individual search terms until it consumes all the level 1 search terms.
#' It refer to the Google Suggest API.
#'
#'# GOOGLE url: https://www.google.com/
#'
#' @param keyword search terms, keywords which supports Korean as well.
#' @keywords google, search terms
#' @export
#' @examples
#' google_keyword("druking")

google_keyword <- function(keyword) {

  ## API call URL and the 1st Related Search Terms --------------
  gsuggest_url <- "http://suggestqueries.google.com/complete/search?output=toolbar&q="
  search_keyword <- glue(gsuggest_url, keyword)

  R1_df <- google_keyword_R1(keyword)

  ## Google the 2nd Related Search Terms --------------
  R2_list <- vector("list", length = nrow(R1_df))

  for(i in 1:nrow(R1_df)) {
    tryCatch({
      search_2nd_keyword <- glue(gsuggest_url, url_encode(R1_df$data[i]))

      gsuggest_2nd_resp <- GET(search_2nd_keyword) %>%
        content()

      gsuggest_2nd_df <- gsuggest_2nd_resp %>%
        xml_nodes('CompleteSuggestion') %>%
        xml_node('suggestion') %>%
        map(xml_attrs) %>%
        map_df(~as.list(.))

      R2_list[[i]] <- gsuggest_2nd_df %>%
        flatten()
    }, error = function(e) {})
  }

  names(R2_list) <- R1_df$data

  return(R2_list)
}
