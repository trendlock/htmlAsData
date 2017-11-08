# tests for date funs

context("Working with HTML tags as strings")

test_that("int_find_good_date returns dates correctly", {
  expect_equal(find_good_date("MARCH 13, 2014"), "2014-03-13")

})

