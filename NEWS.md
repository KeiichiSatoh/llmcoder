# llmcoder 0.0.2 (2025-07-25)

## Modified
- Re-exported `ellmer::type_object` and related functions, 
so they can now be used without explicitly loading the `ellmer` package. 
The README has been updated accordingly.
- `coding_gpt()`: Improved the function documentation.

# llmcoder 0.0.1 (2025-07-23)

## Initial release
- Released basic documentation including DESCRIPTION, NEWS, and LICENSE files.

## Added
- `coding_gpt()`: A wrapper function around `ellmer::openai_chat()` for 
processing `data.frame` objects.
