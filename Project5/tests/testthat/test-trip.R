library(lubridate)

test_that("a trip has the documented columns and types", {
  trip <- bart_trip("MLBR")
  expect_equal(nrow(trip), 1)
  expect_equal(names(trip), c("orig", "dest", "depart", "arrive",
                            "minutes", "fare", "transfers"))
  expect_type(trip$orig, "character")
  expect_type(trip$dest, "character")
  expect_true(inherits(trip$depart, "POSIXct"))
  expect_true(inherits(trip$arrive, "POSIXct"))
  expect_equal(attr(trip$depart, "tzone"), "America/Los_Angeles")
  expect_equal(attr(trip$arrive, "tzone"), "America/Los_Angeles")
  expect_type(trip$minutes, "double")
  expect_type(trip$fare, "double")
  expect_true(is.numeric(trip$transfers))
})

test_that("a trip arrives by the deadline and obeys duration and fare rules", {
  trip <- bart_trip("MLBR")
  deadline <- mdy_hm("11/18/2026 8:45 AM", tz = "America/Los_Angeles")
  expect_true(all(trip$arrive <= deadline))
  expect_equal(trip$minutes,
               as.numeric(difftime(trip$arrive, trip$depart, units = "mins")))
  expect_true(all(trip$minutes > 0))
  expect_true(all(trip$fare > 0))
  expect_true(all(trip$transfers >= 0))
})

test_that("Millbrae needs no transfer and Dublin needs a transfer", {
  expect_equal(bart_trip("MLBR")$transfers, 0)
  expect_true(bart_trip("DUBL")$transfers >= 1)
})

test_that("bad origins produce useful errors", {
  expect_error(bart_trip(c("MLBR", "DUBL")), "single nonmissing station string")
  expect_error(bart_trip(123), "single nonmissing station string")
  expect_error(bart_trip(NA_character_), "single nonmissing station string")
  expect_error(bart_trip(character(0)), "single nonmissing station string")
  expect_error(bart_trip(""), "single nonmissing station string")
  expect_error(bart_trip("XXXX"), "400|no matching trip")
})

test_that("bad destinations produce useful errors", {
  expect_error(bart_trip("MLBR", dest = c("DBRK", "DUBL")), "dest must be")
  expect_error(bart_trip("MLBR", dest = NA_character_), "dest must be")
})

test_that("origin equal to destination means no train journey", {
  trip <- bart_trip("DBRK")
  deadline <- mdy_hm("11/18/2026 8:45 AM", tz = "America/Los_Angeles")
  expect_equal(nrow(trip), 1)
  expect_equal(trip$orig, "DBRK")
  expect_equal(trip$dest, "DBRK")
  expect_equal(trip$depart, deadline)
  expect_equal(trip$arrive, deadline)
  expect_equal(trip$minutes, 0)
  expect_equal(trip$fare, 0)
  expect_equal(trip$transfers, 0)
})

test_that("an unknown same-station code still gives an error", {
  expect_error(bart_trip("XXXX", dest = "XXXX"), "Unknown station abbreviation")
})
