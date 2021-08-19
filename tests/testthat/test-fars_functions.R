# test make_filename function
library(myfars)
test_that("test make_filename function", {
expect_identical(make_filename('2013'),"accident_2013.csv.bz2")})

# test fars_read function
test_that('test fars_read', {
dat <- fars_read('accident_2013.csv.bz2')
expect_equal(dim(dat), c(30202, 50))})

# test fars_read_years
test_that('test fars_read_years', {
yrs <- 2013:2015
dat <- fars_read_years(yrs)
expect_identical(class(dat), 'list')
expect_identical(length(dat), length(yrs))})

# fars_map_state
test_that('test fars_map_state', {
  # error when invalid state number
  expect_error(fars_map_state(200, 2015),
               'invalid STATE number: 1000')
})



