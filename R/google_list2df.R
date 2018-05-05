# Google List to Dataframe
#
# The function convert list to dataframe.
# It receives the input from the Daum related search terms
# in the form of list data structure.
# It convert the list datatype into the dataframe.
#
# The return ouput will be used for the subsequent data analysis and visualization

google_list2df <- function(input_list, keyword) {

  if(length(list(input_list, keyword)) != 2) {
    stop("At least two arguments needed!!")
  }

  suggest_df <- list2df(input_list) %>%
    rename(from=X2, to=X1) %>%
    select(from, to)

  R0R1_df <- suggest_df %>%
    filter(from == keyword)

  suggest_df_list <- list(R0R1_df, suggest_df)

  return(suggest_df_list)
}
