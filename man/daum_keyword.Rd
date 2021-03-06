% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daum_keyword.R
\name{daum_keyword}
\alias{daum_keyword}
\title{Daum Suggest Search Terms - Search Level 2}
\usage{
daum_keyword(keyword)
}
\arguments{
\item{keyword}{search terms, keywords which supports Korean as well.}
}
\description{
The function aims to crawl the second level related search terms from the DAUM search engine.
The only argument for this function is "Search Terms".
This function retrieves the daum_keyword_R1 function in order to get the related search terms from the intial search term.
It iterates crawling the individual search terms until it consumes all the level 1 search terms.
}
\details{
DAUM url: https://www.daum.net/
}
\examples{
daum_keyword("data+scientist")
}
\keyword{daum,}
\keyword{search}
\keyword{terms}
