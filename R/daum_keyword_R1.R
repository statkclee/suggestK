#' Daum Suggest Search Terms - Search Level 1
#'
#' The function aims to crawl the first level related search terms from the DAUM search engine.
#' The only argument for this function is "Search Term".
#'
#' DAUM url: https://www.daum.net/
#'
#' @param keyword search terms, keywords which supports Korean as well.
#' @keywords daum, search terms
#' @export
#' @examples
#' daum_keyword_R1("data+scientist")

daum_keyword_R1 <- function(keyword) {

  searchURL <- "https://search.daum.net/search?w=tot&DA=YZR&t__nil_searchbox=btn&sug=&sugo=&q="

  R1 <- vector(mode="character", length = 0)

  R1_idx_v <- 1L

  while(TRUE) {
    keyword_res <- glue(searchURL, keyword) %>%
      read_html %>%
      html_nodes(xpath = glue('//*[@id="netizen_lists_top"]/span[', R1_idx_v, ']')) %>%
      html_text
    if(identical(keyword_res, character(0))) {
      break
    } else {
      R1[R1_idx_v] <- keyword_res
      R1_idx_v <- R1_idx_v + 1
    }
  }

  return(R1)
}
