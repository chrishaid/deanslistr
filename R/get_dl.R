#' Wrapper around \code{\link{deanslist_api}} to pull suspesion data for multiple schools
#'
#' @param key_list either a names list of or character vector of DeansLists API keys for each school
#' to accdess suspesions data for or you can set .Renviron variables
#' @param ... parameteres like domain passed on onto \code{\link{deanslist_api}}
#'
#' @return a data.frame with DeansList suspension data
#' @export
#'
get_suspensions <- function(key_list,
                   ...) {

  if (missing(key_list)) {
    key_names <- grep("DL_KEY_", names(Sys.getenv(names = TRUE)), value = TRUE)
    key_list <- Sys.getenv(key_names)

  }

  resp_list <- key_list %>%
    purrr::map( ~ deanslist_api(key = ., endpoint = "suspensions", ...))


  out <- purrr::map2(.x = resp_list,
                        .y = names(resp_list),
                        .f = ~.x$content$data %>%
                                mutate(school_name = .y))

  out

}

#' @importFrom dplyr "%>%"