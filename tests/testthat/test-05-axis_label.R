context("Test axis_label")

test_that("axis_label", {
  
  f <- function(side, text, at){
    blank(xlim = c(0,50), ylim = c(0,100))
    axis_blank(side = side)
    axis_text(side = side)
    axis_label(text = text, side = side)
  }
  expect_error(
    f(1, data.frame(), NA)
  )
  expect_silent(
    f(1, "x axis", NA)
  )
  expect_silent(
    f(2, "y axis", NA)
  )
})