is_dev <- function() {
  Sys.getenv("GITHUB_ACTIONS") != "true"
}

dropdown_helper <- function(values, name, element_id) {
  htmltools::tags$select(
    # Set to undefined to clear the filter
    onchange = glue("Reactable.setFilter('{element_id}', '{name}', event.target.value || undefined)"),
    # "All" has an empty value to clear the filter, and is the default option
    htmltools::tags$option(value = "", "All"),
    lapply(unique(values), htmltools::tags$option),
    "aria-label" = sprintf("Filter %s", name)
  )
}

arrow_commit_links <- function(sha) {
  glue("<a href='https://github.com/apache/arrow/commit/{sha}' target='_blank'>{substring(sha, 1, 7)}</a>")
}

arrow_compare_links <- function(sha1, sha2) {
  comp_link <- glue("<a href='https://github.com/apache/arrow/compare/{sha1}...{sha2}' target='_blank'>{substring(sha1, 1, 7)}</a>")

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

arrow_build_table <- function(nightly_data, type, task) {
  type_task_data <- nightly_data %>%
    filter(build_type == type) %>%
    filter(task_name == task)

  ## filter for when the most recent run is a failure 
  day_window <- today() - 2
  ordered_only_recent_fails <- type_task_data %>%
    filter(task_name %in% task_name[nightly_date == day_window & task_status != "success"]) %>%
    arrange(desc(nightly_date)) %>%
    mutate(task_status = case_when(
      task_status == "success" ~ "pass",
      task_status == "failure" ~ "fail",
      TRUE ~ task_status
    ))

  if (nrow(ordered_only_recent_fails) == 0) {
    ## if there are no failures, return a version of the table that reflects that
    days <- as.numeric(
      difftime(
        ymd(Sys.Date(), tz = "UTC"),
        max(type_task_data$nightly_date)
      )
    )
    success_df <- type_task_data %>%
      slice_max(order_by = nightly_date) %>%
      mutate(
        since_last_successful_build = days,
        last_successful_commit = arrow_commit_links(arrow_commit),
        last_successful_build = glue("<a href='{build_links}' target='_blank'>{task_status}</a>"),
        most_recent_status = "passing"
      ) %>%
      select(task_name, most_recent_status, since_last_successful_build, last_successful_commit, last_successful_build, build_type)
    return(success_df)
  }

  ## find first failure index
  idx_recent_fail <- rle(ordered_only_recent_fails$task_status)$lengths[1]

  ## expand failure index and give it some names
  failure_df <- tibble(fails_plus_one = seq(1, idx_recent_fail + 1)) %>%
    mutate(fail_label = case_when(
      fails_plus_one == idx_recent_fail ~ "first_failure",
      fails_plus_one == 1 ~ "most_recent_failure",
      fails_plus_one == idx_recent_fail + 1 ~ "last_successful_build",
      TRUE ~ paste0(fails_plus_one, " days ago")
    )) %>%
    filter(fails_plus_one <= 9 | grepl("failure|build", fail_label))

  ## inner_join to ordered data
  df <- ordered_only_recent_fails %>%
    rowid_to_column() %>%
    inner_join(failure_df, by = c("rowid" = "fails_plus_one"))


  if (all(type_task_data$task_status %in% "failure")) {
    days <- NA_real_
  } else {
    ## days since last successful build (need to add one)
    days <- sum(as.numeric(
      difftime(
        df$nightly_date[df$fail_label == "most_recent_failure"],
        df$nightly_date[df$fail_label == "last_successful_build"]
      )
    ), 1)
  }


  get_commit <- function(label) {
    df$arrow_commit[df$fail_label == label]
  }

  df %>%
    arrange(desc(fail_label)) %>%
    mutate(build_links = glue("<a href='{build_links}' target='_blank'>{task_status}</a>")) %>%
    select(task_name, build_type, build_links, fail_label) %>%
    pivot_wider(names_from = fail_label, values_from = build_links) %>%
    mutate(
      since_last_successful_build = days,
      last_successful_commit = arrow_compare_links(get_commit("last_successful_build"), get_commit("first_failure")),
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
