#' count number of authors from search
#' @param df A dataframe derived from a search. Must have package_data as one
#'  of the columns
#' @export
#' @author Kathryn Doering
count_authors <- function(df) {
   list_with_authors <- apply(df, 1, function(df_row) {
      author_string <- df_row[["package_data"]][["Authors@R"]]
      author_count <- stringr::str_count(author_string, "person")
      if(length(author_count) == 0) {
        if(isTRUE(length(grep("Author", names(df_row[["package_data"]]),
                              ignore.case = TRUE)) > 0)) {
          author_string <- df_row[["package_data"]][[
            grep("Author", names(df_row[["package_data"]]), ignore.case = TRUE)[1]]]
                # try just counting commas? maybe close enough?
          author_count <- stringr::str_count(author_string, ",")
        } else {
          author_count <- NA
        }
      }
      author_count
      })
  list_with_authors <- unlist(list_with_authors)
  df$author_count <- list_with_authors
  df
}

