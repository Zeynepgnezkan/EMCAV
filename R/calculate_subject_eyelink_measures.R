#' Calculate Eyelink Measures for a Subject
#'
#' This function calculates fixation time measures for a given subject based on their Eyelink data. It processes the trial database and fixations data to extract relevant metrics, including fixation time per word, and organizes the results for further analysis.
#'
#' @param subject A list containing the subject's data, which includes:
#'   - `trial_db`: A data frame with trial information (e.g., language, trial type, target word number, etc.).
#'   - `trial_fixations`: A list of data frames containing fixation data for each trial.
#'   - `trial_blinks`: A list of data frames containing blink data for each trial.
#'   - `trial_words`: A list of data frames containing word boundary information for each trial.
#'   - `filename`: A character string representing the subject's file name.
#' @return A list containing:
#'   - `fixation_time_measures_by_word`: A data frame with calculated fixation time measures for each word in the trials.
#'   - `fixations_for_measures`: A data frame with processed fixations data.
#'   - `fixations_not_clean`: A data frame with raw fixations that were not cleaned.
#'
#' @import dplyr
#' @import stringr
#' @importFrom purrr map_dfr
#'
#' @export
calculate_subject_eyelink_measures <- function(subject) {
  results <- list()
  cat("Subject: ", subject$filename)
  trial_db <- subject$trial_db
  filename <- subject$filename
  if (subject$trial_db$language[1] == "ENG") {
    subnumber <- as.numeric(str_extract(filename, "(?<=THE)\\d+"))
  } else {
    if (subject$trial_db$language[1] == "TR") {
      subnumber <- as.numeric(str_extract(filename, "(?<=sub_)\\d+"))
    } else {
      subnumber <- as.numeric(str_extract(filename, "(?<=sub_)\\d+"))
      # modify for es
    }
  }

  # preallocate list entries for each trial (based on trial_db)
  # vector("list", n) creates a list of n NULL entries
  fixation_time_measures_by_word <- vector("list", nrow(trial_db))
  fixations_not_clean <- vector("list", nrow(trial_db))
  fixations_for_measures <- vector("list", nrow(trial_db))

  for (i in 1:nrow(trial_db)) {
    cat(i, " ")
    if (subject$trial_db$language[1] == "ENG") {
      trial_db[i, ]$trial_type <-
        ifelse(trial_db[i, ]$trial_type == "experiment", "E", "P")
    } else {
      if (subject$trial_db$language[1] == "TR" |
          subject$trial_db$language[1] == "ES") {
        trial_db[i, ]$trial_type <-
          ifelse(trial_db[i, ]$trial_type == "exp", "E", "P")
      }
    }
    if (trial_db[i, ]$trial_type == "E" &
        trial_db[i, ]$has_display_off == TRUE) {
      fixations_not_clean[[i]] <-
        fix_in_dc(
          eyelink_fixations = subject$trial_fixations[[i]],
          eyelink_blinks = subject$trial_blinks[[i]],
          trial_words = subject$trial_words[[i]],
          trial_db_for_trial = trial_db[i, ]
        )

      fixations_for_measures[[i]] <-
        process_eyelink_trial(
          eyelink_fixations = subject$trial_fixations[[i]],
          eyelink_blinks = subject$trial_blinks[[i]],
          trial_words = subject$trial_words[[i]],
          trial_db_for_trial = trial_db[i, ]
        )

      if (!is_null(fixations_for_measures[[i]])) {
        if (subject$trial_db$language[1] == "ES") {
          fixation_time_measures <-
            calculate_fixation_time_measures(fixations = fixations_for_measures[[i]],
                                             words = subject$trial_words[[i]])
        } else {
          fixation_time_measures <-
            calculate_fixation_time_measures(fixations = fixations_for_measures[[i]],
                                                 words = subject$trial_words[[i]])
        }

        fixation_time_measures$subject <- subnumber

        fixation_time_measures$target_word_nr <- as.numeric(trial_db[i, ]$target_word_nr) + 1 # python counts from 0
        fixation_time_measures$acceptability <- trial_db[i, ]$response
        fixation_time_measures$cond <- trial_db[i, ]$cond
        fixation_time_measures$language <- subject$language
        fixation_time_measures_by_word[[i]] <- fixation_time_measures
      }
    }
  }

  results$fixation_time_measures_by_word <- bind_rows(fixation_time_measures_by_word)
  results$fixations_for_measures <- bind_rows(fixations_for_measures)
  results$fixations_not_clean <- bind_rows(fixations_not_clean)

  return(results)
}

