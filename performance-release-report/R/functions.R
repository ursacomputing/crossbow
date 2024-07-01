#' Find runs for a given hardware name and two git commits
#' A wrapper around conbenchcoms::runs()
#'
#' @return A tibble of runs (or an empty tibble if no runs were found)

find_runs <- function(baseline_git_commit, contender_git_commit, hardware_name) {
  runs_raw <- conbenchcoms::runs(c(baseline_git_commit, contender_git_commit))
  if (length(runs_raw) == 0) {
    message("This combination of commits did not return any runs.")
    return(invisible(tibble::tibble()))
  }

  runs_raw %>%
    dplyr::filter(hardware.name %in% hardware_name) %>%
    dplyr::mutate(run_type = case_when(
      commit.sha == contender_git_commit ~ "contender",
      commit.sha == baseline_git_commit ~ "baseline"
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("links"), conbenchcoms::url_cleaner))
}

#' A wrapper around conbenchcoms::compare() which looks for
#' contender and baseline runs in the given data frame
compare_baseline_to_contender <- function(.data) {
  compare_runs(
    .data$id[.data$run_type == "baseline"],
    .data$id[.data$run_type == "contender"]
  ) %>%
    as_tibble()
}


#' helper function to get the language summary from a comparison
get_language_summary_from_comparison <- function(.data) {
  .data %>%
    filter(!is.na(language)) %>%
    mutate(
      benchmark_type = case_when(
        language %in% c("R", "Python") ~ "[macrobenchmarks](#macro-bm)",
        language %in% c("C++", "Java", "JavaScript") ~ "[microbenchmarks](#micro-bm)",
      )
    ) %>%
    summarise(
      languages = paste(unique(language), collapse = ", "),
      n_benchmarks = length(benchmark_name),
      .by = c(run_id, benchmark_type)
    )
}




#' Detect if we are on GitHub Actions
is_gha <- function() {
  Sys.getenv("GITHUB_ACTIONS") == "true"
}


plot_comparison <- function(plot_df, alpha = 0.7) {

  ## Can be a uri as well
  plot_df <- plot_df %>%
    mutate(dataset = case_when(
      is.na(dataset) ~ dataset_uri,
      TRUE ~ dataset
    ))

  tag_names <- unlist(plot_df$tags_used)
  x_facets <- c("compression", "format", "file_type", "use_legacy_dataset")
  # TODO: add others?
  x_facets <- x_facets[x_facets %in% tag_names]

  y_facets <- c("output_type", "input_type", "streaming", "pre_buffer", "async", "selectivity", "scale_factor")
  y_facets <- y_facets[y_facets %in% tag_names]

  maybe_vars <- function(variables) {
    if (length(variables) == 0) {
      return(NULL)
    }

    vars(!!!syms(variables))
  }

  title <- glue::glue("{unique(plot_df$language)}: {unique(plot_df$benchmark_name)}")
  p <- plot_df %>%
    mutate(change_lab = change * 1.05) %>%
    ggplot() +
    aes(y = forcats::fct_reorder(dataset, pn_lab, .desc = TRUE), x = change, fill = pn_lab) +
    geom_col_interactive(
      aes(
        tooltip = glue(
          "Baseline result: {baseline.single_value_summary}{unit}\n",
          "Contender result: {contender.single_value_summary}{unit}\n",
          "Difference: {difference}\n",
          "Percent change: {round(change, 2)}%\n",
          "Dataset: {dataset}"
        )
      ),
      position = "dodge", alpha = 0.75, width = 0.75, colour = "grey 40"
    ) +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_text(aes(x = change_lab, hjust = ifelse(change >= 0, 0, 1), label = difference, colour = pn_lab), size = 6) +
    facet_grid(rows = maybe_vars(x_facets), cols = maybe_vars(y_facets), labeller = label_both) +
    scale_fill_manual(values = change_cols) +
    scale_colour_manual(values = change_cols) +
    scale_x_continuous(expand = expansion(mult = 0.6)) +
    guides(colour = "none") +
    ylab("Dataset") +
    xlab("% change")

  ## Some height adjustments
  g_heights <- 6
  g_widths <- 16

  if (length(x_facets) > 0) {
    g_heights <- g_heights*(n_distinct(plot_df[, x_facets, drop = TRUE])/1.25)
  }
  girafe(
    ggobj = p,
    options = list(
      opts_tooltip(use_fill = TRUE),
      opts_sizing(width = .7)
    ),
    width_svg = g_widths,
    height_svg = g_heights
  )
}

#' Take a language and benchmark name combo and return a processed tibble
#' with some relevant columns brought to the top level
#' @param .x language
#' @param .y benchmark name
#' @return tibble
tidy_compare <- function(.x, .y) {
  tags <- .x %>%
    select(starts_with("baseline.tags")) %>%
    select(where(~!all(is.na(.))))

  if (length(unique(tags$baseline.tags.name)) > 1) {
    browser()
  }

  tag_names <- colnames(tags)
  tag_names <- tag_names[!tag_names %in% c("baseline.tags.name", "baseline.tags.query_id", "baseline.tags.language", "baseline.tags.engine", "baseline.tags.memory_map", "baseline.tags.query", "baseline.tags.async")]

  plot_df <- .x %>%
    select(-any_of(c("baseline.tags.dataset", "baseline.tags.language", "baseline.tags.unit"))) %>%
    rename_with(~ gsub("baseline.tags.", "", .)) %>%
    mutate(change = analysis.pairwise.percent_change) %>%
    mutate(difference = paste0(round((baseline.single_value_summary - contender.single_value_summary), 4), "", unit)) %>%
    mutate(cb_url = glue('https://conbench.ursa.dev/benchmark-results/{baseline.benchmark_result_id}/')) %>%
    mutate(
      pn_lab = case_when(
        analysis.pairwise.percent_change == 0 ~ "no change",
        analysis.pairwise.percent_change > 0 ~ paste0(contender_git_commit_short, " (contender) faster"),
        analysis.pairwise.percent_change < 0 ~ paste0(baseline_git_commit_short, " (baseline) faster")
      )
    ) %>%
    mutate(
      tags_used = list(gsub("baseline.tags.", "", tag_names)), ## just need the bare tag names
      language = .y$baseline.language,
      benchmark_name = .y$baseline.benchmark_name
    )
}


generate_compare_url <- function(x) {
  baseline_id <- unique(x$baseline$run_id)
  baseline_id <- baseline_id[!is.na(baseline_id)]
  contender_id <- unique(x$contender$run_id)
  contender_id <- contender_id[!is.na(contender_id)]

  glue("https://conbench.ursa.dev/compare/runs/{baseline_id}...{contender_id}")
}


top_zscore_table <- function(.data, top_n = 20, direction = c("improvement", "regression")) {

  direction <- match.arg(direction)

  if (direction == "improvement") {
    .data <- .data %>%
      arrange(desc(analysis_lookback_z_score_z_score))
  } else {
    .data <- .data %>%
      arrange(analysis_lookback_z_score_z_score)
  }

  ## let's convert things to megabytes
  .data <- .data %>%
    mutate(across(ends_with("single_value_summary"), ~ case_when(
      unit == "B/s" ~ .x/1000000, ## B/s -> MB/s
      TRUE ~ .x
    ))) %>%
    mutate(unit = case_when(
      unit == "B/s" ~ "MB/s",
      TRUE ~ unit
    ))

  .data %>%
    head(top_n) %>%
    mutate(name = glue("[{name}]({cb_url})")) %>%
    select(
      language, suite, name, params, analysis_lookback_z_score_z_score, analysis_pairwise_percent_change, baseline_single_value_summary, contender_single_value_summary, unit) %>%
    arrange(language, suite, name, params) %>%
    gt(rowname_col = "language", groupname_col = "suite") %>%
    fmt_markdown(columns = "name") %>%
    fmt_percent(columns = "analysis_pairwise_percent_change", scale_values = FALSE, decimals = 2) %>%
    fmt_number(columns = ends_with("single_value_summary"), decimals = 0) %>%
    fmt_number(columns = "analysis_lookback_z_score_z_score", decimals = 2) %>%
    cols_label(
      language = "Language",
      name = "Benchmark",
      suite = "Suite",
      params = "Params",
      baseline_single_value_summary = "Baseline result",
      contender_single_value_summary = "Contender result",
      analysis_pairwise_percent_change = "Percent Change",
      analysis_lookback_z_score_z_score = "z-score"
    ) %>%
    tab_spanner(columns = c("baseline_single_value_summary", "contender_single_value_summary", "unit"), label= "Results") %>%
    tab_spanner(columns = starts_with("analysis_"), label= "Analysis") %>%
    opt_table_font(font = google_font("Roboto Mono")) %>%
    tab_options(table.font.size = "10px") %>%
    tab_footnote(
      footnote = "MB/s = megabytes per second; ns = nanoseconds; i/s = iterations per second",
      locations = cells_body(columns = "unit")
    )
}

top_perf_table <- function(.data, top_n = 20, direction = c("improvement", "regression")) {

  direction <- match.arg(direction)

  if (direction == "improvement") {
    .data <- .data %>%
      arrange(desc(analysis_pairwise_percent_change))
  } else {
    .data <- .data %>%
      arrange(analysis_pairwise_percent_change)
  }

  ## let's convert things to megabytes
  .data <- .data %>%
    mutate(across(ends_with("single_value_summary"), ~ case_when(
      unit == "B/s" ~ .x/1000000, ## B/s -> MB/s
      TRUE ~ .x
    ))) %>%
    mutate(unit = case_when(
      unit == "B/s" ~ "MB/s",
      TRUE ~ unit
    ))

  .data %>%
    head(top_n) %>%
    mutate(name = glue("[{name}]({cb_url})")) %>%
    select(
      language, suite, name, params, analysis_pairwise_percent_change, baseline_single_value_summary, contender_single_value_summary, unit) %>%
    arrange(language, suite, name, params) %>%
    gt(rowname_col = "language", groupname_col = "suite") %>%
    fmt_markdown(columns = "name") %>%
    fmt_percent(columns = "analysis_pairwise_percent_change", scale_values = FALSE, decimals = 2) %>%
    fmt_number(columns = ends_with("single_value_summary"), decimals = 0) %>%
    cols_label(
      language = "Language",
      name = "Benchmark",
      suite = "Suite",
      params = "Params",
      baseline_single_value_summary = "Baseline result",
      contender_single_value_summary = "Contender result",
      analysis_pairwise_percent_change = "Percent Change",
    ) %>%
    tab_spanner(columns = c("baseline_single_value_summary", "contender_single_value_summary", "unit"), label= "Results") %>%
    tab_spanner(columns = starts_with("analysis_"), label= "Analysis") %>%
    opt_table_font(font = google_font("Roboto Mono")) %>%
    tab_options(table.font.size = "10px") %>%
    tab_footnote(
      footnote = "MB/s = megabytes per second; ns = nanoseconds; i/s = iterations per second",
      locations = cells_body(columns = "unit")
    )
}