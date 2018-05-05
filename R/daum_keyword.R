# Daum Suggest Search Terms - Search Level 2
#
# The function aims to crawl the second level related search terms from the DAUM search engine.
# The only argument for this function is "Search Terms".
# This function retrieves the daum_keyword_R1 function in order to get the related search terms from the intial search term.
# It iterates crawling the individual search terms until it consumes all the level 1 search terms.
#
# DAUM url: https://www.daum.net/

daum_keyword <- function(keyword) {

    R1 <- daum_keyword_R1(keyword)

    R2 <- vector(mode="list", length = length(R1))

    for(i in 1: length(R1)) {
      R2[[i]] <- daum_keyword_R1(url_encode(R1[i]))
    }

    names(R2) <- R1

    return(R2)
}
