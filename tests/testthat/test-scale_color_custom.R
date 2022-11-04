test_that("Requires string data names", {
  expect_error(scale_color_custom(dat.names = 1:10))
})
