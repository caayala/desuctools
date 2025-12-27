test_that("alch_get_survey_contacts valida credenciales faltantes", {
  expect_error(
    alch_get_survey_contacts(
      api_token = "",
      api_token_secret = "secret",
      survey_id = 8529571,
      campaign_id = 24631558
    ),
    "credenciales"
  )

  expect_error(
    alch_get_survey_contacts(
      api_token = "token",
      api_token_secret = "",
      survey_id = 8529571,
      campaign_id = 24631558
    ),
    "credenciales"
  )
})

test_that("alch_get_survey_contacts valida parámetros numéricos", {
  expect_error(
    alch_get_survey_contacts(
      survey_id = "no_numerico",
      campaign_id = 24631558
    ),
    "survey_id.*numérico"
  )

  expect_error(
    alch_get_survey_contacts(
      survey_id = 8529571,
      campaign_id = "no_numerico"
    ),
    "campaign_id.*numérico"
  )

  expect_error(
    alch_get_survey_contacts(
      survey_id = 8529571,
      campaign_id = 24631558,
      results_per_page = 1000
    ),
    "results_per_page.*500"
  )
})

test_that("alch_get_survey_contacts valida parámetro page", {
  expect_error(
    alch_get_survey_contacts(
      survey_id = 8529571,
      campaign_id = 24631558,
      page = 0
    ),
    "page.*all.*positivo"
  )

  expect_error(
    alch_get_survey_contacts(
      survey_id = 8529571,
      campaign_id = 24631558,
      page = "invalido"
    ),
    "page.*all.*positivo"
  )
})
