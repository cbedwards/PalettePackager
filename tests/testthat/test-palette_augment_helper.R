test_that("basic augmentation works", {
  aug = data.frame(name = letters[1:10],
                   color = rep("blue",10))
  expect_true(palette_augment_helper(dat.names = letters[1:3],
                         augment = aug))
})
