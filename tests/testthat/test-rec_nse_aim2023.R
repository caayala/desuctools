test_that("rec_nse_aim2023 usa educación, ocupación e ingreso output labelled", {
  df <- data.frame(
    edu = c(1, 3, 5, 7, 9, 4, 4, 4),
    ocup = c(2, 4, 6, 8, 9, 4, 4, 4),
    res = c(3, 3, 4, 4, 5, 5, 10, NA_integer_),
    ing = c(1, 2, 3, 4, 5, 6, 7, NA_integer_)
  )

  res <- rec_nse_aim2023(
    .edu_jh = df$edu,
    .ocu_jh = df$ocup,
    .tramo_ingreso = df$ing,
    factor = FALSE
  )

  expect_s3_class(res, "haven_labelled")
  expect_equal(
    labelled::to_character(res),
    c("E", "D", "C3", "C2", "C1a", "C2", "C1b", NA_character_)
  )
})


test_that("rec_nse_aim2023 usa educación, ocupación e ingreso output labelled", {
  df <- data.frame(
    edu = c(1, 3, 5, 7, 9, 4, 4, 4),
    ocup = c(2, 4, 6, 8, 9, 4, 4, 4),
    res = c(3, 3, 4, 4, 5, 5, 10, NA_integer_),
    ing = c(1, 2, 3, 4, 5, 6, 7, NA_integer_)
  )

  res <- rec_nse_aim2023(
    .edu_jh = df$edu,
    .ocu_jh = df$ocup,
    .tramo_ingreso = df$ing,
    factor = TRUE
  )

  expect_s3_class(res, "factor")
  expect_equal(
    as.character(res),
    c("E", "D", "C3", "C2", "C1a", "C2", "C1b", NA_character_)
  )
})
