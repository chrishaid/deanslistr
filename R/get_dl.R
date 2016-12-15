#' Wrapper around \code{\link{deanslist_api}} to pull suspesion data for multiple schools
#'
#' @param key_list either a named list or named character vector of DeansLists API keys for each school
#' to accdess suspesions data for your school(s).  Alternatively you can add keys to .Renviron variables, which must start with \code{DL_KEY_}
#' in order to be recognized.
#' @param ... parameteres like domain passed on onto \code{\link{deanslist_api}}
#'
#' @description This function wraps \code{\link{deanslist_api}} to pull the suspenions endpoint
#' (i.e., \code{deanlist_api(endpoint = 'suspensions', ...)}.  If you don't explicitly pass the
#' \code{key_list} paramater a named list, the function will attempt use \code{\link[base]{Sys.getenv}}
#' to gather keys from environment variables.  These variables can be added to a \code{.Renviron} file
#' that is source on startup or you can you set them using \code{\link[base]{Sys.setenv}}.  In either case,
#' the function looks for variables that begin with \code{DLAKEY_}.  So for Langley High School you might
#' set a key with the variable name \code{DL_KEY_LHS = 'super_secret_key'}.
#'
#' If multiple keys are found (or passed as a named list to the \code{key_list} argument), then
#' a list of data frames will be returned.  The slot in the list for each data frame is named
#' after the key used to retrieve it.
#'
#' @return a data.frame or list of data.frames (if multiple keys passed) with DeansList suspension data
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
                                dplyr::as_data_frame() %>%
                                dplyr::mutate(school_name = .y))

  out

}

