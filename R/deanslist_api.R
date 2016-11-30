#' R API for DeansList's RESTful API
#'
#' @param endpoint the endpoint from which to requrest data.
#' @param domain your DearnsList domain.
#' @param key  your school specific API key.
#' @param endpoint_version the endpoint verrsion.  Default is "v1".  Anything else implements "beta"
#' endpoints, for which you should supply the table name (i.e., supply "behavavior" to hit
#' the \code{get-behavior-data.php} endpoint).
#' @param ... optional DeansList paramaters added to query string in the GET request (e.g. sdt and
#' edt for start and end dates).
#'
#' @details This is a workhourse function that wraps the HTTP RESTful API that DeansList has implemented.
#' You will need your school's/district's/network's domain (i.e., DOMAIN in \code{https://DOMAIN.deanslistsoftware.com})
#' and each schools API key, which you'll need to request from DeansList.
#'
#' For requests in the v1 API you give the end of the endpoint name to he \code{deanslist_api()}. For example,
#' the \code{/api/v1/suspensions} endpoint is accessed with
#' \code{deanslist_api('suspensions', domain = 'dlacademy', key = 'SUPER_SECRET_KEY')}.  Requests for the beta API
#' simply require the component of the end point that unique identifies (i.e., the table name).  So the \code{get-behavior-data.php} endpoint
#' is accessed with \code{deanslist_api('behaviors', domain = 'dlacademy', key = 'SUPER_SECRET_KEY', endpoint_version = 'beta').}
#'
#' @return a deanslistr object, which is a list containing the response, endpoint path,
#' and returned content
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dla_key <- Sys.getenv("KEY_DLA") # Get this key from Deanslist
#'
#' x <- deanslist_api(endpoint = 'suspensions',
#'                    domain = 'dlacademy',
#'                    key = dla_key)
#'}
#'
deanslist_api <- function(endpoint,
                          domain,
                          key,
                          endpoint_version = c("v1", "beta"),
                          ...){

  if (missing(endpoint_version)) endpoint_version <- "v1"

  if (endpoint_version == "v1") {
    end_point <- sprintf('api/v1/%s', endpoint)
  } else {
    end_point <- sprintf('api/beta/export/get-%s-data.php', endpoint)
  }

  # There's a couple of enpoints that don't fit the two rules above, so we fix em here
  if (endpoint == "pointbank") {
    end_point <- 'api/beta/bank/get-bank-book.php'
  }

  if (endpoint == "users") {
    end_point <- 'api/beta/export/get-users.php'
  }

  if (endpoint == "students" & endpoint_version == "beta") {
    end_point <- 'api/beta/export/get-students.php'
  }

  if (endpoint == "roster-assignments") {
    end_point <- 'api/beta/export/get-roster-assignments.php'
  }


  url <- httr::modify_url(url = sprintf('https://%s.deanslistsoftware.com/', domain),
                          path = end_point
                          )


  ua <- httr::user_agent("http:://github.com/chrishaid/deanslistr")

  query_list = list(
    apikey = key,
    ...
  )


  resp <- httr::GET(url = url,
                    query = query_list,
                    ua
                    )

  # response error checking
  httr::stop_for_status(resp)

  if(endpoint != "pointbank") {
    if (httr::http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               flatten = TRUE)


  structure(
    list(
      content = parsed,
      path = end_point,
      response = resp
    ),
    class = "deanslist_api"
  )
}

#' @export
print.deanslist_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  str(x$content, max.level = 2)
  invisible(x)
}