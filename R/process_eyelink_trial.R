#' Processing EyeLink data for a single trial
#'
#' This function processes EyeLink data for a single trial, including fixations, saccades, and blinks.
#'
#' @param eyelink_fixations A data frame containing fixation data from the ASC file.
#' @param eyelink_blinks A data frame containing blink data from the ASC file.
#' @param trial_words A data frame containing word information for the current trial.
#' @param trial_db_for_trial A data frame containing trial information for the current trial.
#' @param use_eye A character string specifying which eye to use for the analysis. Must be either "L" or "R". Default is "R".
#' @param min_duration_for_elimination An integer specifying the minimum duration (in milliseconds) for a fixation to be considered valid. Fixations shorter than this duration will be eliminated. Default is 800 ms.
#' @param max_duration_for_elimination_or_merge An integer specifying the maximum duration (in milliseconds) for a fixation to be considered valid. Fixations longer than this duration will be eliminated, and fixations shorter than this duration may be merged with neighboring fixations. Default is 80 ms.
#' @param max_distance_for_merge An integer specifying the maximum distance (in pixels) between fixations to be considered for merging. Fixations that are closer than this distance may be merged. Default is 12 px.
#' @param sentence_start_x An integer specifying the x-coordinate of the start of the sentence. Default is 51 px.
#'
#' @return A list containing various data frames and lists including trial information, messages, samples, words, fixations, saccades, and blinks.
#'
#' @import readr
#' @import dplyr
#' @seealso merge_fixations
#' @seealso assign_word_info
#' @seealso assign_word_info_eng
#' @seealso mark_fixations_close_to_blinks
#'
#' @examples
#' eyelink_fixations <- read_csv("eyelink_fixations.csv")
#' eyelink_blinks <- read_csv("eyelink_blinks.csv")
#' trial_words <- read_csv("trial_words.csv")
#' trial_db_for_trial <- read_csv("trial_db_for_trial.csv")
#' processed_data <- process_eyelink_trial(eyelink_fixations, eyelink_blinks, trial_words, trial_db_for_trial)
#'
#' @export

process_eyelink_trial <- function(eyelink_fixations, eyelink_blinks, trial_words, trial_db_for_trial, use_eye = "R", min_duration_for_elimination = 800, max_duration_for_elimination_or_merge = 80, max_distance_for_merge = 12, sentence_start_x = 51){

    # Validate input for 'use_eye'
    use_eye <- toupper(use_eye)  # Normalize to uppercase
    if (use_eye != "L" & use_eye != "R") {
      stop("Invalid use_eye parameter. Must be either 'L' or 'R'")
    }

    # Check if data contains fixations from the opposite eye
    opposite_eye <- ifelse(use_eye == "R", "L", "R")
    if (all(eyelink_fixations$eye == opposite_eye)) {
      # All fixations are from the opposite eye - change
      eyelink_fixations$eye <- use_eye
      warning("No fixations found for ", use_eye, " but there are fixations for ", opposite_eye, " which will be used instead.")
    } else if (!any(eyelink_fixations$eye == use_eye)) {
      # No fixations from the desired eye. Issue a warning.
      warning(paste0("No fixations found for either eye."))
    }

  # filter fixations for the current trial
  fixations <- eyelink_fixations %>%  filter(eye == use_eye & t_start > trial_db_for_trial$timestamp_display_on & t_end < trial_db_for_trial$timestamp_display_off)

  # if there are blinks, filter them too
  fixations$near_blink <- FALSE

  # if there are no blinks, the blinks list just contains NA, which is not a tibble
  if(is_tibble(eyelink_blinks) & length(eyelink_blinks) > 0){
    blinks <- eyelink_blinks %>% filter(eye == use_eye & t_start > trial_db_for_trial$timestamp_display_on & t_end < trial_db_for_trial$timestamp_display_off)
    if(length(blinks) > 0){
      fixations <- mark_fixations_close_to_blinks(fixations, blinks, t_diff_threshold = 50)
    }
  }

  # short fixations < 80 ms and < 100 ms near blinks should be eliminated. Ones < 80 ms not near blinks may be merged with preceding or subsequent fixations if they are less than one character (12 px) away
  fixations <- fixations %>% mutate(too_short = duration < max_duration_for_elimination_or_merge, too_long = duration > min_duration_for_elimination)

  merged_fixations <- merge_fixations(fixations, max_duration = max_duration_for_elimination_or_merge, max_distance = 12, rate = as.numeric(trial_db_for_trial$recording_sampling_rate))

  # for diagnostics -- is tidyverse function the same as loop?
  #merged_fixations_loop <- merge_fixations_loop(fixations, max_duration = max_duration_for_elimination_or_merge, max_distance = 12, rate = as.numeric(trial_db_for_trial$recording_sampling_rate))

  #if(!identical(merged_fixations, merge_fixations_loop(fixations, max_duration = max_duration_for_elimination_or_merge, max_distance = 12))){
  #  warning("Merged fixations not identical!")
  #}

  # assign word information to fixations
  if(nrow(merged_fixations) > 0){
    if(trial_db_for_trial$language == "ES"){
      merged_fixations_with_word_info <- assign_word_info(fixations= merged_fixations, word_boundaries=trial_words, sentence_left_x_boundary= 51)
    }else{
      merged_fixations_with_word_info <- assign_word_info_eng(fixations= merged_fixations, word_boundaries=trial_words, sentence_left_x_boundary= 51)
    }
    return(merged_fixations_with_word_info)
  }
  else{
    warning(paste("No fixations found for", use_eye, "eye, Subject", trial_db_for_trial$subject_nr, "Trial", rownames(trial_db_for_trial), "Sentence", trial_db_for_trial$sentence_number, ". Returning NULL"))
    return(NULL)
  }
}
