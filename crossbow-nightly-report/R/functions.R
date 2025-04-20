library(tibble)
library(dplyr)
library(lubridate)
library(glue)
library(tidyr)

is_dev <- function() {
  Sys.getenv("GITHUB_ACTIONS") != "true"
}

dropdown_helper <- function(values, name, element_id) {
  htmltools::tags$select(
    # Set to undefined to clear the filter
    onchange = glue(
      "Reactable.setFilter('{element_id}', '{name}', event.target.value || undefined)"
    ),
    # "All" has an empty value to clear the filter, and is the default option
    htmltools::tags$option(value = "", "All"),
    lapply(unique(values), htmltools::tags$option),
    "aria-label" = sprintf("Filter %s", name)
  )
}

arrow_commit_links <- function(sha) {
  glue(
    "<a href='https://github.com/apache/arrow/commit/{sha}' target='_blank'>{substring(sha, 1, 7)}</a>"
  )
}

arrow_compare_links <- function(sha1, sha2) {
  comp_link <- glue(
    "<a href='https://github.com/apache/arrow/compare/{sha1}...{sha2}' target='_blank'>{substring(sha1, 1, 7)}</a>"
  )

  if (rlang::is_empty(comp_link)) {
    return(glue("Build has not yet been successful"))
  }
  return(comp_link)
}

format_links <- function(link) {
  glue("<a href='{link}' target='_blank'>{basename(link)}</a>")
}

make_nice_names <- function(x) {
  toTitleCase(gsub("_", " ", names(x)))
}

get_commit <- function(df, label) {
  df$arrow_commit[df$fail_label == label]
}

arrow_build_table <- function(nightly_data, type, task, to_day = today()) {
  # Filter data for a specific build type and task
  type_task_data <- nightly_data %>%
    filter(build_type == type) %>%
    filter(task_name == task)

  # Look at yesterday's date to determine recent failures
  # This is used as a window for identifying tasks that failed recently
  day_window <- to_day - 1

  # Get records where the task failed recently, order by date (newest first)
  # and standardize task status values to "pass" and "fail"
  ordered_only_recent_fails <- type_task_data %>%
    # Only keep records where the task name appears in yesterday's failures
    filter(
      task_name %in%
        task_name[nightly_date == day_window & task_status != "success"]
    ) %>%
    arrange(desc(nightly_date)) %>%
    mutate(
      task_status = case_when(
        task_status == "success" ~ "pass",
        task_status == "failure" ~ "fail",
        TRUE ~ task_status
      )
    )

  # If there are no recent failures, return a success summary or a null summary if the task is not active
  if (nrow(ordered_only_recent_fails) == 0) {
    # Calculate days since the last run (regardless of status)
    days <- as.numeric(
      difftime(
        ymd(to_day, tz = "UTC"),
        max(type_task_data$nightly_date)
      )
    )
    # Create a summary with success information
    success_df <- type_task_data %>%
      # Remove stale data by filtering out everything but the last ~2 days of runs
      # this makes it so that jobs that have been deleted (but are still in the 120 day look back)
      # don't continue to show up.
      filter(nightly_date >= to_day - 2) %>%
      # Then, take the most recent run since that's all we care about if there are no failures.
      slice_max(order_by = nightly_date) %>%
      mutate(
        since_last_successful_build = days,
        last_successful_commit = arrow_commit_links(arrow_commit),
        last_successful_build = glue(
          "<a href='{build_links}' target='_blank'>{task_status}</a>"
        ),
        most_recent_status = "passing"
      ) %>%
      select(
        task_name,
        most_recent_status,
        since_last_successful_build,
        last_successful_commit,
        last_successful_build,
        build_type
      )

    return(success_df)
  }

  # Find the length of the most recent consecutive failure streak
  # This uses run length encoding to identify the first sequence of failures
  idx_recent_fail <- rle(ordered_only_recent_fails$task_status)$lengths[1]

  # Create labels for the failure streak timeline
  # This builds a dataframe with positions and labels for the recent failure sequence
  failure_df <- tibble(fails_plus_one = seq(1, idx_recent_fail + 1)) %>%
    mutate(
      fail_label = case_when(
        fails_plus_one == idx_recent_fail ~ "first_failure", # Where the failures began
        fails_plus_one == 1 ~ "most_recent_failure", # The most recent failure
        fails_plus_one == idx_recent_fail + 1 ~ "last_successful_build", # Last successful build before failures
        TRUE ~ paste0(fails_plus_one, " days ago") # General failure timeline
      )
    ) %>%
    # Only keep the most recent 9 days of failures or specific labeled events
    filter(fails_plus_one <= 9 | grepl("failure|build", fail_label))

  # Join the failure timeline labels with the actual build data
  df <- ordered_only_recent_fails %>%
    rowid_to_column() %>%
    inner_join(failure_df, by = c("rowid" = "fails_plus_one"))

  # Calculate days since last successful build
  if (all(type_task_data$task_status %in% "failure")) {
    days <- NA_real_
  } else {
    # Calculate days between most recent failure and last successful build
    # Adding 1 to include the day of the failure
    days <- sum(
      as.numeric(
        difftime(
          df$nightly_date[df$fail_label == "most_recent_failure"],
          df$nightly_date[df$fail_label == "last_successful_build"]
        )
      ),
      1
    )
  }

  # Format the final result as a table with build status information (one row per task)
  df %>%
    arrange(desc(fail_label)) %>%
    mutate(
      build_links = glue(
        "<a href='{build_links}' target='_blank'>{task_status}</a>"
      )
    ) %>%
    select(task_name, build_type, build_links, fail_label) %>%
    # Reshape data to have one column for each failure stage
    pivot_wider(names_from = fail_label, values_from = build_links) %>%
    # Add additional context columns
    mutate(
      since_last_successful_build = days,
      last_successful_commit = arrow_compare_links(
        get_commit(df, "last_successful_build"),
        get_commit(df, "first_failure")
      ),
      most_recent_status = "failing",
      .after = build_type
    )
}

crossbow_theme <- function(data, ...) {
  data %>%
    tab_options(
      table.font.size = 14,
      ...
    ) %>%
    opt_all_caps()
}
