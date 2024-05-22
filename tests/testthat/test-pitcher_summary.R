poly_utah_game <- read.csv(here::here("5-10-24_CalPoly_UtahTech.csv"))

test_that("pitcher_summary dimensions work", {
  correct_result <- c(6, 3)

  my_result <- pitcher_summary(data = poly_utah_game, pitcherid = 1000114562) %>%
    dim()

  expect_equal(my_result, correct_result)
})


test_that("pitcher_summary changing type works", {
  correct_result <- c(6, 5)

  my_result <- pitcher_summary(data = poly_utah_game,
                               pitcherid = 1000114562,
                               type = "auto") %>%
    dim()

  expect_equal(my_result, correct_result)
})

test_that("get_pitching_summary works", {
  correct_result <- poly_utah_game %>%
    filter(PitcherId == 1000114562) %>%
    pull(RelSpeed) %>%
    mean() %>%
    round(digits = 4)

  my_result <- pitcher_summary(data = poly_utah_game,
                               pitcherid = 1000114562,
                               type = "tagged")


  expect_equal(round(my_result[2,3], digits = 4), correct_result)
})
