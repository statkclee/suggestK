#' Naver Suggest Search Terms - Search Level 2
#'
#' The function aims to crawl the second level related search terms from the NAVER search engine.
#' The only argument for this function is "Search Terms".
#' This function retrieves the naver_keyword_R1 function in order to get the related search terms from the intial search term.
#' It iterates crawling the individual search terms until it consumes all the level 1 search terms.
#'
# NAVER url: https://www.naver.com/

#' @param keyword search terms, keywords which supports Korean as well.
#' @keywords naver, search terms
#' @export
#' @examples
#' naver_keyword("korea")

naver_keyword <- function(keyword) {

    R1 <- naver_keyword_R1(keyword)

    R2 <- vector(mode="list", length = length(R1))

    for(i in 1: length(R1)) {
      R2[[i]] <- naver_keyword_R1(url_encode(R1[i]))
    }

    names(R2) <- R1

    return(R2)
}
