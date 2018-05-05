# List to Dataframe from NAVER and DAUM search terms
#
# The function convert list to dataframe.
# It receives the input from the Daum related search terms
# in the form of list data structure.
# It convert the list datatype into the dataframe.
#
# The return ouput will be used for the subsequent data analysis and visualization

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
