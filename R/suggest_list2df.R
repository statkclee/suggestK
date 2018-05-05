#' List to Dataframe from NAVER and DAUM search terms
#'
#' The function convert list to dataframe.
#' It receives the input from the Daum and Naver related search terms
#' in the form of list data structure.
#' It convert the list datatype into the dataframe.
#'
#' The return ouput will be used for the subsequent data analysis and visualization
#'
#' @param input_list Naver and Daum search terms result which must be list datatype.
#' @param keyword search terms, keywords which supports Korean as well.
#' @keywords daum, naver, search terms, list to dataframe, conversion
#' @export
#' @examples
#' korea_list <- daum_keyword("korea")
#' suggest_list2df(korea_list, "korea")


suggest_list2df <- function(input_list, keyword) {

  if(length(list(input_list, keyword)) != 2) {
    stop("At least two arguments needed!!")
  }

  R1R2_df <- list2df(input_list) %>%
    rename(from=X2, to=X1) %>%
    select(from, to)

  R0R1_df <- tibble(to = names(input_list)) %>%
    mutate(from = keyword)

  suggest_df <- bind_rows(R0R1_df, R1R2_df)
  suggest_df_list <- list(R0R1_df, suggest_df)

  return(suggest_df_list)
}
