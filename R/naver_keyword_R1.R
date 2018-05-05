#' Naver Suggest Search Terms - Search Level 1
#'
#' The function aims to crawl the first level related search terms from the NAVER search engine.
#' The only argument for this function is "Search Term".
#'
#' NAVER url: https://www.naver.com/
#'
#' @param keyword search terms, keywords which supports Korean as well.
#' @keywords naver, search terms
#' @export
#' @examples
#' naver_keyword_R1("data+scientist")


naver_keyword_R1 <- function(keyword) {

  searchURL <- "https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=1&ie=utf8&query="

  R1 <- glue(searchURL, keyword) %>%
    read_html %>%
    html_nodes(xpath = '//*[@id="nx_related_keywords"]/dl/dd[1]') %>%
    html_text %>%
    str_split("   ") %>%
    unlist %>%
    str_trim()

  return(R1)
}


