context("%->%: assign temporal result to a variable")

test_that("Memorize assignment operator works", {

  x <- 1:10
  x %>% sum %->% y %>% sin
  expect_equal( sum(x), y)

  x <- 1:10
  x %>% multiply_by(7) %->% y %->% z
  expect_equal(7*x, y)
  expect_equal(z, y)

  x <- 1:10
  (x %>% sin %->% y %>% plot)
  expect_equal(sin(x), y)

})
