context("%->%: assign temporal result to a variable")

test_that("Memorize assignment operator works", {

  x <- 1:10
  x %>% sum %->% y %>% sin
  expect_that(sum(x), is_identical_to(y))

  x <- 1:10
  x %>% multiply_by(2) %->% y
  expect_that(2*x, is_identical_to(y))

  x <- 1:10
  x %>% sin %->% y %>% plot
  expect_that(sin(x), is_identical_to(y))

})
