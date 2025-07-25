#' Structured Content Coding with OpenAI GPT Models
#'
#' This function is a wrapper around the `chat_openai()` function from the 
#' `ellmer` package, designed to facilitate structured text coding for data 
#' stored in a `data.frame`.
#' It uses structured prompts with OpenAI GPT models to extract or generate structured 
#' information based on a user-defined schema (`type_object_setting`).
#'
#' The function supports token usage checking, user confirmation for long input, 
#' and optionally merges the results back into the original data frame.
#'
#' @param df A `data.frame` or vector containing the text to be processed.
#' @param type_object_setting A `TypeObject` created via `ellmer::type_object()` 
#' that defines the expected structure of the output.
#' @param which_col A column name (character) or index (integer) indicating 
#' which column of `df` contains the text input. Defaults to `1`.
#' @param model Character string specifying the OpenAI model to use. Defaults to 
#' `"gpt-4.1"`. See https://platform.openai.com/docs/models for other models.
#' @param include_df Logical. If `TRUE`, the result is merged with the input `df`. 
#' If `FALSE`, only the response is returned. Defaults to `TRUE`.
#' @param overall_instruction Optional character string to be passed as the 
#' `system_prompt` to the chat model.
#' @param base_url The base URL for the OpenAI API. Defaults to `"https://api.openai.com/v1"`.
#' @param api_key Character string containing your OpenAI API key. If \code{NULL}, 
#' the key will be retrieved from the environment variable \code{"OPENAI_API_KEY"}.
#' @param params Optional list of additional parameters to pass to the model 
#' (e.g., temperature, max_tokens). See also \link[ellmer:params]{params} for detail.
#' @param seed 	Optional integer seed that ChatGPT uses to try and make output
#'  more reproducible.
#' @param api_args Additional API-specific arguments passed as a list. 
#' Defaults to an empty list.
#' @param echo Character string indicating whether to print messages during execution. 
#' Must be one of `"none"`, `"output"`, or `"all"`.
#' @param nchar_warning_limit Integer. If the total number of characters in the 
#' input exceeds this threshold, a confirmation prompt will be displayed. 
#' Defaults to `100000`.
#'
#' @return If `include_df = TRUE`, a `data.frame` with the original `df` and the 
#' model's structured response. If `include_df = FALSE`, a list of structured outputs.
#'
#' @details
#' \strong{How to define a TypeObject:}
#'
#' Below are examples of how to define a `TypeObject` using the `type_object()` 
#' constructor:
#'
#' Example 1: Simple type object
#' 
#' \preformatted{
#'     type_object(
#'       name = type_string(),
#'       age = type_number()
#'     )
#' }
#' 
#' Example 2: With custom instructions
#' 
#' \preformatted{
#'     type_object(
#'       name = type_string("Extracted entity name."),
#'       type = type_enum("Entity type: one of 'person', 'location', 'organization'"),
#'       age = type_number("Age in years.")
#'     )
#' }
#'
#' Supported base types include: 
#' - `type_boolean()`: logical
#' - `type_integer()`: integer
#' - `type_number()` : double
#' - `type_string()` : string, and
#' - `type_enum()`   : categorical.
#'
#' For more information, see the ellmer vignette: 
#' \url{https://ellmer.tidyverse.org/articles/structured-data.html}
#'
#' \strong{How to set the OpenAI API key:}
#'
#' You can provide the API key directly via the `api_key` argument, or set it through an environment variable.
#'
#' \emph{Option 1: Temporary environment variable (valid for current session)}
#' \preformatted{
#'     Sys.setenv(OPENAI_API_KEY = "your_key_here")
#' }
#'
#' \emph{Option 2: Permanent setting via .Renviron}
#' \enumerate{
#'   \item Open your `.Renviron` file: \code{file.edit("~/.Renviron")}
#'   \item Add a line: \code{OPENAI_API_KEY=sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
#'   \item Save and restart your R session.
#' }
#'
#' @import ellmer
#'
#' @examples
#' \dontrun{
#' df <- data.frame(ID = 1:2, text = c("東京都渋谷区", "大阪府大阪市北区"))
#' schema <- type_object(city = type_string(), district = type_string())
#' result <- coding_gpt(df = df,
#'                      which_col = "text",
#'                      type_object_setting = schema, 
#'                      api_key = "[API_KEY]")               # set your OpenAI API KEY
#' }
#' @export

coding_gpt <- function(
    df,
    type_object_setting,
    which_col = 1,
    model = "gpt-4.1",
    include_df = TRUE,
    overall_instruction = NULL,
    base_url = "https://api.openai.com/v1",
    api_key = NULL,
    params = list(),
    seed = NULL,
    api_args = list(),
    echo = c("none", "output", "all"),
    nchar_warning_limit = 100000
) {
  # process the model and settings
  echo <- match.arg(echo)
  # check API key
  if(is.null(api_key)){
    api_key <- apikey_from_sysenv("OPENAI_API_KEY")
    # stop if still null
    if (is.null(api_key)) stop("API key not found in environment variable.")
  }
  
  # seed
  if(!is.null(seed)){
    stopifnot(is.numeric(seed))
    params$seed <- seed
  }
  
  # validate the type_object_setting
  stopifnot(inherits(type_object_setting, "ellmer::TypeObject"))
  
  # validate df
  if(is.vector(df)){
    df <- data.frame(df)
    which_col <- 1
  }
  stopifnot(is.data.frame(df))
  stopifnot(length(which_col) == 1)
  
  # extract the input text
  col_vec <- df[[which_col]]
  input <- as.list(col_vec)
  
  # check the number of characters
  n_char <- sum(nchar(unlist(input)))
  if (n_char > nchar_warning_limit) {
    cat(sprintf("This input text has %.0f characters in total.\n", n_char))
    ans <- readline("Are you sure to continue? (y/n): ")
    if (tolower(ans) != "y") stop("Aborted by user.")
  }
  
  # instance the chat object
  chat <- chat_openai(
    system_prompt = overall_instruction,
    base_url = base_url,
    api_key = api_key,
    model = model,
    params = params,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
  
  
  # get response
  response <- tryCatch(
    parallel_chat_structured(chat, input, type = type_object_setting),
    error = function(e) stop("parallel_chat_structured failed: ", e$message)
  )
  
  # create output
  out <- if (include_df) data.frame(df, response) else response
  
  # print cost
  cat("Estimated cost:")
  print(chat$get_cost("all"))
  
  return(out)
}

