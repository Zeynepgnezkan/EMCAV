#' Function to extract fixations from a trial
#'
#' This function extracts fixations from a trial, given the trial's word boundaries and the fixations data. It also assigns the word information to each fixation.
#'
#' @param eyelink_fixations Fixations data from the Eyelink
#' @param eyelink_blinks Blinks data from the Eyelink
#' @param trial_words Word boundaries for the trial
#' @param trial_db_for_trial Data for the trial
#' @param use_eye Eye to use for the fixations
#'
#' @return fixations_with_word_info Fixations data with word information
#'
#' @import dplyr
#'
#' @examples
#' # Example data
#' eyelink_fixations <- data.frame(eye = c("R", "R", "L"),
#'                                 t_start = c(100, 200, 300),
#'                                 t_end = c(150, 250, 350))
#' eyelink_blinks <- data.frame(t_start = c(120, 230),
#'                              t_end = c(130, 240))
#' trial_words <- data.frame(word_start = c(50, 160, 270),
#'                           word_end = c(150, 250, 350))
#' trial_db_for_trial <- data.frame(timestamp_display_on = 100,
#'                                  timestamp_display_off = 350,
#'                                  language = "EN",
#'                                  subject_nr = 1,
#'                                  sentence_number = 42)
#'
#' # Run function with example data
#' fix_in_dc(eyelink_fixations, eyelink_blinks, trial_words, trial_db_for_trial, use_eye = "R")
#'
#' @export
fix_in_dc <- function(eyelink_fixations, eyelink_blinks, trial_words, trial_db_for_trial, use_eye = "R"){
  if(eyelink_fixations$eye[1] == "L"){
    eyelink_fixations$eye <- "R"
  }
  fixations <- eyelink_fixations %>%  filter(eye == use_eye & t_start > trial_db_for_trial$timestamp_display_on & t_end < trial_db_for_trial$timestamp_display_off)
  if(nrow(fixations) > 0){
    if(trial_db_for_trial$language == "ES"){
      fixations_with_word_info <- assign_word_info(fixations= fixations, word_boundaries=trial_words, sentence_left_x_boundary= 51)
    }else{
      fixations_with_word_info <- assign_word_info_eng(fixations= fixations, word_boundaries=trial_words, sentence_left_x_boundary= 51)
    }
    return(fixations_with_word_info)
  }
  else{
    warning(paste("No fixations found for", use_eye, "eye, Subject", trial_db_for_trial$subject_nr, "Trial", rownames(trial_db_for_trial), "Sentence", trial_db_for_trial$sentence_number, ". Returning NULL"))
    return(NULL)
  }
}
