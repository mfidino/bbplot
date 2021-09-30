context("Test axis_text")

test_that("axis_text", {
  
  f <- function(side, text, at){
    blank(xlim = c(0,50), ylim = c(0,100))
    axis_blank(side=side)
    axis_text(text = text, side = side, at = at)
  }
  expect_error(
    f(5, NA, NA)
  )
  expect_silent(
    f(1, NA, NA)
  )
  expect_silent(
    f(2, NA, NA)
  )
  expect_silent(
    f(1, seq(0,50,10), at= seq(0,50,10))
  )
  expect_error(
    f("x", NA, NA)
  )
})