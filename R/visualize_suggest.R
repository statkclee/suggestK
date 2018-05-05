#' Visualize Keyword, 1st Search Terms and 2nd Search Terms
#'
#' The function visualizes the relationships among keyword,
#' 1st search terms and 2nd search terms
#' The only search term is needed to get visualized artefacts.
#'
#' @param keyword search terms, keywords which supports Korean as well.
#' @param search_engine search engine; "daum", "naver", "google"
#' @keywords network, visualize
#' @export
#' @examples
#' visualize_suggest("korea", "google")

visualize_suggest <- function(keyword, search_engine) {

  if(search_engine == "daum") {
    tmp_list <- daum_keyword(keyword)
  } else if(search_engine == "naver") {
    tmp_list <- naver_keyword(keyword)
  } else {
    tmp_list <- google_keyword(keyword)
  }

  if(search_engine == "google") {
    suggest_df_list <- google_list2df(tmp_list, keyword)
  } else {
    suggest_df_list <- suggest_list2df(tmp_list, keyword)
  }

  suggest_nw <- suggest_df_list[[2]] %>%
    graph_from_data_frame %>%
    igraph_to_networkD3

  suggest_nw$nodes$group <- ifelse(suggest_nw$nodes$name %in% suggest_df_list[[1]]$from, "Keyword",
                                   ifelse(suggest_nw$nodes$name %in% suggest_df_list[[1]]$to, "1st Relation", "2nd Relation"))

  networkD3::forceNetwork(Links = suggest_nw$links, Nodes = suggest_nw$nodes,
                          colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                          Source = "source", Target = "target",
                          Group = "group", NodeID = "name",
                          opacity = 0.7, zoom = T,
                          fontSize = 13, fontFamily = "NanumGothic", legend = T,
                          opacityNoHover = 0.9)

}
