#' Get the API key from environment variable
#' @param KEY A character string to get the relevant api.
#' @return A character string with the API key, or error if not found
#' @export
#' @keywords internal
apikey_from_sysenv <- function(KEY) {
  key <- Sys.getenv(KEY)
  if (!nzchar(key)) {
    warning("No API key found.")
    return(NULL)
  }
  key
}