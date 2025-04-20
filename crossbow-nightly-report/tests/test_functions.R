source("../R/functions.R")

test_that("arrow_commit_links", {
  expect_equal(
    arrow_commit_links("befa11ed"),
    "<a href='https://github.com/apache/arrow/commit/befa11ed' target='_blank'>befa11e</a>"
  )
})

test_that("arrow_compare_links", {
  expect_equal(
    arrow_compare_links("befa11ed", "ca55e77e"),
    "<a href='https://github.com/apache/arrow/compare/befa11ed...ca55e77e' target='_blank'>befa11e</a>"
  )
  expect_equal(
    arrow_compare_links("befa11ed", "ca55e77e"),
    "<a href='https://github.com/apache/arrow/compare/befa11ed...ca55e77e' target='_blank'>befa11e</a>"
  )
  expect_equal(arrow_compare_links("befa11ed", NULL), "Build has not yet been successful")
  expect_equal(arrow_compare_links(NULL, "ca55e77e"), "Build has not yet been successful")
  expect_equal(arrow_compare_links(NULL, NULL), "Build has not yet been successful")
})

test_that("arrow_build_table handles successful builds", {
  # Create fixed date for testing
  fixed_date <- as.Date("2025-04-19")

  # Create sample data with all successful builds
  success_data <- tibble::tibble(
    task_name = "task1",
    build_type = "Linux",
    nightly_date = as.Date(c("2025-04-18", "2025-04-17", "2025-04-16", "2025-04-15", "2025-04-14")),
    task_status = "success",
    arrow_commit = "abcdef1234567890",
    build_links = "https://ci-builds/build/123"
  )

  success_result <- arrow_build_table(success_data, "Linux", "task1", to_day = fixed_date)

  expect_equal(nrow(success_result), 1)
  expect_equal(success_result$most_recent_status, "passing")
  expect_equal(success_result$since_last_successful_build, 1)
  expect_true(grepl("<a href='https://github.com/apache/arrow/commit/", success_result$last_successful_commit))
  expect_true(grepl("<a href='https://ci-builds/build/123", success_result$last_successful_build))
})

test_that("arrow_build_table handles recent failures", {
  # Create fixed date for testing
  fixed_date <- as.Date("2025-04-19")

  # Create sample data with recent failures
  failure_data <- tibble::tibble(
    task_name = "task1",
    build_type = "Linux",
    nightly_date = as.Date(c(
      "2025-04-18",
      "2025-04-17",
      "2025-04-16",
      "2025-04-15",
      "2025-04-14",
      "2025-04-13",
      "2025-04-12"
    )),
    task_status = c("failure", "failure", "failure", "success", "success", "success", "success"),
    arrow_commit = c("def123", "cde123", "bcd123", "abc123", "zyx987", "wvu876", "tsr765"),
    build_links = c(
      "https://ci-builds/build/def123",
      "https://ci-builds/build/cde123",
      "https://ci-builds/build/bcd123",
      "https://ci-builds/build/abc123",
      "https://ci-builds/build/zyx987",
      "https://ci-builds/build/wvu876",
      "https://ci-builds/build/tsr765"
    )
  )

  failure_result <- arrow_build_table(failure_data, "Linux", "task1", to_day = fixed_date)

  expect_equal(nrow(failure_result), 1)
  expect_equal(failure_result$most_recent_status, "failing")
  expect_equal(failure_result$since_last_successful_build, 4)
  expect_match(failure_result$last_successful_build, "https://ci-builds/build/abc123")
  expect_match(failure_result$first_failure, "https://ci-builds/build/bcd123")
  expect_match(failure_result$most_recent_failure, "https://ci-builds/build/def123")
})

test_that("arrow_build_table handles all failures case", {
  # Create fixed date for testing
  fixed_date <- as.Date("2025-04-19")

  # Create sample data where all builds have failed
  all_failure_data <- tibble::tibble(
    task_name = "task1",
    build_type = "Linux",
    nightly_date = as.Date(c("2025-04-18", "2025-04-17", "2025-04-16", "2025-04-15", "2025-04-14")),
    task_status = "failure",
    arrow_commit = c("def123", "cde123", "bcd123", "abc123", "zyx987"),
    build_links = c(
      "https://ci-builds/build/def123",
      "https://ci-builds/build/cde123",
      "https://ci-builds/build/bcd123",
      "https://ci-builds/build/abc123",
      "https://ci-builds/build/zyx987"
    )
  )

  all_failure_result <- arrow_build_table(all_failure_data, "Linux", "task1", to_day = fixed_date)

  expect_equal(nrow(all_failure_result), 1)
  expect_equal(all_failure_result$most_recent_status, "failing")
  expect_true(is.na(all_failure_result$since_last_successful_build))
  expect_match(all_failure_result$last_successful_commit, "Build has not yet been successful")
  expect_match(all_failure_result$first_failure, "https://ci-builds/build/zyx987")
  expect_match(all_failure_result$most_recent_failure, "https://ci-builds/build/def123")
})

test_that("arrow_build_table recent success, some failures", {
  # Create fixed date for testing
  fixed_date <- as.Date("2025-04-19")

  # Create sample data with yesterday's success but previous failures
  mixed_data <- tibble::tibble(
    task_name = "task1",
    build_type = "Linux",
    nightly_date = as.Date(c(
      "2025-04-18",
      "2025-04-17",
      "2025-04-16",
      "2025-04-15",
      "2025-04-14",
      "2025-04-13",
      "2025-04-12"
    )),
    task_status = c("success", "failure", "failure", "failure", "success", "success", "success"),
    arrow_commit = c("ghi456", "def123", "cde123", "bcd123", "abc123", "zyx987", "wvu876"),
    # No tests need to differentiate the build link, but if they did, this needs to be changed:
    build_links = "https://ci-builds/build/123"
  )

  # Should be treated as success since the most recent day (day_window) is successful
  result <- arrow_build_table(mixed_data, "Linux", "task1", to_day = fixed_date)

  expect_equal(result$most_recent_status, "passing")
  expect_equal(result$since_last_successful_build, 1)
})


test_that("arrow_build_table ignores failures and then removal", {
  # Create fixed date for testing
  fixed_date <- as.Date("2025-04-19")

  # Create sample data where all builds have failed
  all_failure_data <- tibble::tibble(
    task_name = "task1",
    build_type = "Linux",
    nightly_date = as.Date(c("2025-04-16", "2025-04-15", "2025-04-14", "2025-04-13", "2025-04-12")),
    task_status = "failure",
    arrow_commit = "abcdef1234567890",
    # No tests need to differentiate the build link, but if they did, this needs to be changed:
    build_links = "https://ci-builds/build/123"
  )

  all_failure_result <- arrow_build_table(all_failure_data, "Linux", "task1", to_day = fixed_date)

  expect_equal(nrow(all_failure_result), 0)
})
