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
  compare(
    type = "runs",
    .data$id[.data$run_type == "baseline"],
    .data$id[.data$run_type == "contender"],
    simplifyVector = TRUE
  ) %>%
    as_tibble()
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
          "Percent change: {round(change, 2)}%\n",
          "Difference: {difference}\n",
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
  tags <- .x$baseline_tags[colSums(!is.na(.x$baseline_tags)) > 0]

  if (length(unique(tags$name)) > 1) {
    browser()
  }

  tag_names <- colnames(tags)
  tag_names <- tag_names[!tag_names %in% c("name", "query_id", "language", "engine", "memory_map", "query", "async")]

  plot_df <- .x %>%
    jsonlite::flatten() %>%
    as_tibble() %>%
    select(-any_of(c("baseline_tags.dataset", "baseline_tags.language"))) %>%
    rename_with(~ gsub("baseline_tags.", "", .)) %>%
    mutate(change = analysis.pairwise.percent_change) %>%
    mutate(difference = paste0(round((baseline.single_value_summary - contender.single_value_summary) * 1000, 4), "", unit)) %>%
    mutate(
      pn_lab = case_when(
        analysis.pairwise.percent_change == 0 ~ "no change",
        analysis.pairwise.percent_change > 0 ~ paste0(contender_git_commit_short, " (contender) faster"),
        analysis.pairwise.percent_change < 0 ~ paste0(baseline_git_commit_short, " (baseline) faster")
      )
    ) %>%
    mutate(
      tags_used = list(tag_names),
      language = .y$`baseline$language`,
      benchmark_name = .y$`baseline$benchmark_name`
    )
}


generate_compare_url <- function(x) {
  baseline_id <- unique(x$baseline$run_id)
  baseline_id <- baseline_id[!is.na(baseline_id)]
  contender_id <- unique(x$contender$run_id)
  contender_id <- contender_id[!is.na(contender_id)]

  glue("https://conbench.ursa.dev/compare/runs/{contender_id}...{baseline_id}")
}


top_zscore_table <- function(.data, top_n = 20, direction = c("positive", "negative")) {
  direction <- match.arg(direction)

  if (direction == "positive") {
    .data <- .data %>%
      arrange(desc(analysis_lookback_z_score_z_score))
  } else {
    .data <- .data %>%
      arrange(analysis_lookback_z_score_z_score)
  }

  .data %>%
    head(top_n) %>%
    select(language, name, suite, params, analysis_lookback_z_score_z_score) %>%
    gt() %>%
    cols_label(
      language = "Language",
      name = "Benchmark",
      suite = "Suite",
      params = "Params",
      analysis_lookback_z_score_z_score = "z-score"
    ) %>%
    opt_table_font(font = google_font("Roboto Mono")) %>%
    tab_options(table.font.size = "10px")
}