test_that("coding_gpt works with minimal input", {
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    skip("No OpenAI API key set. Skipping coding_gpt test.")
  }
  
  library(ellmer)
  
  df <- data.frame(
    ID = 1:2,
    text = c("I like bananas", "I prefer apples"),
    stringsAsFactors = FALSE
  )
  
  type_object_setting <- ellmer::type_object(
    favorite_fruits = ellmer::type_string("Extract the speaker's favorite fruits")
  )
  
  out <- coding_gpt(
    df = df,
    which_col = "text",
    type_object_setting = type_object_setting,
    overall_instruction = "Answer in one word only."
  )
  
  expect_s3_class(out, "data.frame")
  expect_true("favorite_fruits" %in% colnames(out))
  expect_equal(nrow(out), 2)
})
