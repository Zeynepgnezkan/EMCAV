#' Read Eyelink ASC File and Extract Data
#'
#' This function reads an Eyelink ASC file from an experiment conducted with Opensesame, and extracts various data including raw samples, fixations, blinks, and word information that was written into the ASC file by Opensesame.
#'
#' @param data_file The path to the ASC data file to be read.
#' @param rate The sampling rate of the data.
#' @param sentence_start_x The x-coordinate of the sentence start. Default is 125.
#' @param language The language of the experiment. Default is "ENG".
#'
#' @return A list containing various data frames and lists including trial information, messages, samples, words, fixations, saccades, and blinks.
#'
#' @import readr
#' @import dplyr
#'
#' @examples
#' # Provide the path to the ASC file
#' data_file <- "path/to/your/data.asc"
#'
#' # Read the ASC file and extract data
#' subject_data <- read_asc_file(data_file)
#'
#' @export
read_asc_file <- function(data_file, rate = 1000,  sentence_start_x = 125,language){
  subject <- list()
  subject$filename <- data_file
  subject$rate = rate
  filename <- subject$filename
  if (language == "ENG") {
    subnumber <- as.numeric(str_extract(filename, "(?<=THE)\\d+"))
  } else {
    if (language == "TR") {
      subnumber <- as.numeric(str_extract(filename, "(?<=sub_)\\d+"))
    } else {
      subnumber <- as.numeric(str_extract(filename, "(?<=sub_)\\d+"))
      # modify for es
    }
  }
  cat("\n"); cat(sprintf("Loading data file: %s", data_file), "\n")
  dataF <- readLines(data_file)

  ### get start and end times ###

  S_W <- which(grepl('TRIAL \\d+ ITEM \\d+ WORD 1\\s', dataF))
  S_W_text <- dataF[S_W]
  end_stop<- which(grepl('stop_trial', dataF))
  trial_start_t<- which(grepl('start_trial', dataF))




  ### get trial infos:
  ID<- which(grepl('TRIALID', dataF));
  trial_text<- dataF[ID]

  result_df <- data.frame(do.call(rbind, strsplit(trial_text, " ")))



  trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
  trials<- gsub(" ", "", trials)
  itemN <- as.numeric(stringr::str_match(trials, pattern = '\\d{1,4}'))
  cond <- stringr::str_match(trials, pattern = '_(\\w{3,9})')[,2]
  subject_nr <- rep(subnumber, times = length(cond))


  trial_db <- data.frame(itemN, cond, trial_start_t, end_stop, S_W,subject_nr)

  ntrials<- nrow(trial_db)

  cat("Found", ntrials, "trials.\n")

  # add more columns to trial_db
  #trial_db$sentence_start_x <- sentence_start_x
  trial_db <- trial_db %>% dplyr::mutate(language = language,
                                         has_display_off = NA,
                                         sentence_start_x = NA,
                                         sentence_number = NA,
                                         #subject_nr = NA,
                                         target_word_nr = NA,
                                         target = NA,
                                         trial_type = NA,
                                         # trial timing
                                         timestamp_display_on = NA,
                                         timestamp_display_off = NA,
                                         question = NA,
                                         response = NA,
                                         answer = NA,
                                         correct = NA,
                                         recording_sampling_rate = NA,
                                         # boundary measures
                                         dc_start_timestamp = NA,
                                         dc_end_timestamp = NA,
                                         dc_duration = NA,
                                         boundary_x = NA,
                                         timestamp_boundary_l = NA,
                                         timestamp_boundary_r = NA,
                                         prev_fix_l_start = NA,
                                         prev_fix_l_end = NA,
                                         prev_fix_r_start = NA,
                                         prev_fix_r_end = NA,
                                         next_fix_l_start = NA,
                                         next_fix_l_end = NA,
                                         next_fix_r_start = NA,
                                         next_fix_r_end = NA,
                                         prev_fix_l_x = NA,
                                         prev_fix_l_y = NA,
                                         prev_fix_r_x = NA,
                                         prev_fix_r_y = NA,
                                         next_fix_l_x = NA,
                                         next_fix_l_y = NA,
                                         next_fix_r_x = NA,
                                         next_fix_r_y = NA
  )



  trial_msgs <- list()
  trial_samples <- list()
  trial_words <- list()

  # eyelink fixations/saccades/blinks
  trial_fixations <- list()
  trial_saccades <- list()
  trial_blinks <- list()


  for (i in 1:nrow(trial_db)) {
    start_line <- trial_db$S_W[i]
    start_line_samples <- trial_db$trial_start_t[i]
    end_line_samples <- trial_db$end_stop[i]
    if (i < nrow(trial_db)){
      end_line <- trial_db$S_W[i+1]-1
    } else {
      end_line <- end_line_samples
    }

    # get trial lines from file (use all the lines for messages so we don't miss the trial info, but only lines after trial start flag for samples)
    trial_lines_msgs <- dataF[start_line:end_line]

    trial_lines_samples <- dataF[start_line_samples:end_line_samples]

    # get messages from the built-in Eyelink Saccade detection algorithm
    trial_lines_efix <- grep(x = trial_lines_samples, pattern = "EFIX", value = TRUE)
    fixations <- read_table(trial_lines_efix, col_names = c("EFIX", "eye", "t_start", "t_end", "duration", "x", "y", "pupil")) %>% dplyr::select(-EFIX)

    trial_lines_esacc <- grep(x = trial_lines_samples, pattern = "ESACC", value = TRUE)
    saccades <- read_table(trial_lines_esacc, col_names = c("ESACC", "eye", "t_start", "t_end", "duration", "x_start", "y_start", "x_end", "y_end", "amplitude", "peak_velocity")) %>% dplyr::select(-ESACC)

    trial_lines_eblink <- grep(x = trial_lines_samples, pattern = "EBLINK", value = TRUE)
    if(length(trial_lines_eblink) > 1){
      blinks <- read_table(trial_lines_eblink, col_names = c("EBLINK", "eye", "t_start", "t_end", "duration")) %>% dplyr::select(-EBLINK)
    } else if(length(trial_lines_eblink) == 1) {
      # we have a problem if there is only one single blink since read_table interprets the string as a file name
      # fix: duplicate the line and then only save the first row of the resulting tibble
      trial_lines_eblink <- c(trial_lines_eblink, trial_lines_eblink)
      blinks <- read_table(trial_lines_eblink, col_names = c("EBLINK", "eye", "t_start", "t_end", "duration")) %>% dplyr::select(-EBLINK) %>% .[1,]
    } else {
      blinks <- NA
    }


    # filter lines into messages and samples
    if(language == "ES"){
      pattern_samples <- "^\\d{5,10}(?:\\s+\\-{0,1}\\d{0,5}\\.{1}\\d{0,1}){6}\\s[\\.ICR]{5}$"
    }else{
      pattern_samples <- "^[0-9].+$"
    }
    pattern_msg <- "^MSG\\s+\\d{5,10}"
    trial_msg <- trial_lines_msgs[grepl(pattern_msg, trial_lines_msgs, perl = TRUE)]
    trial_sample <- trial_lines_samples[grepl(pattern_samples, trial_lines_samples, perl = TRUE)]

    if(language == "ES"){
      col_name <- c("timestamp", "x_l", "y_l", "pupil_l", "x_r", "y_r", "pupil_r", "flags")
      coltype = cols(
        timestamp = col_integer(),
        x_l = col_number(),
        y_l = col_number(),
        pupil_l = col_number(),
        x_r = col_number(),
        y_r = col_number(),
        pupil_r = col_number(),
        flags = col_character())
    }else{
      if(language == "TR" | language == "ENG"){
        col_name <- c("timestamp", "x", "y", "pupil", "flags")
        coltype = cols(
          timestamp = col_integer(),
          x = col_number(),
          y = col_number(),
          pupil = col_number(),
          flags = col_number())
      }
    }

    # read_table is by far the fastest way to read the samples into a data frame/tibble
    suppressWarnings({
      sample <- read_table(trial_sample,
                           col_names = col_name,
                           col_types = coltype,
                           na = "."
      )
    })
    trial_samples[[i]] <- sample
    cat(i, " ")

    # get more information about the trial from the messages

    # get_trial_info_from_msg finds the matching line, then extracts the info from the single capture group
    if(language != "ENG" & trial_db[i,]$cond != "identical"){
      trial_db[i,]$cond <- get_trial_info_from_msg(trial_msg, ("MSG\\t\\d+ var changed ([a-z]+)"))
    }
    trial_db[i,]$sentence_start_x <- get_trial_info_from_msg(trial_msg, ("MSG\\t\\d+ var sentence_start_x (\\d+)"))
    trial_db[i,]$sentence_number <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var sentence_number (\\d+)")
    #trial_db[i,]$subject_nr <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var subject_nr (\\d+)")
    trial_db[i,]$target_word_nr <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var target_word_nr (\\d+)")
    trial_db[i,]$target <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var target ([\\?a-z]+)")
    trial_db[i,]$trial_type <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var trial_type ([a-z]+)")
    trial_db[i,]$timestamp_display_on <- get_trial_info_from_msg(trial_msg, "MSG\\t(\\d+) DISPLAY ON") %>% as.numeric
    trial_db[i,]$timestamp_display_off <-  get_trial_info_from_msg(trial_msg, "MSG\\t(\\d+) DISPLAY OFF") %>% as.numeric
    # some trials were never properly terminated. In this case, this flag helps us remove them from the analysis.
    trial_db[i,]$has_display_off <- !is.na(trial_db[i,]$timestamp_display_off)

    # Get information about comprehension questions
    trial_db[i,]$question <-  get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var question ([\\?A-Za-z\\s]*)")
    trial_db[i,]$response <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var response ([0-5rightlefspac]+)")

    if(language == "ES"){
      trial_db[i,]$answer <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var answer ([SINOA]+)")
    }else if(language == "TR"){
      trial_db[i,]$correct <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var correct (.+)")
      trial_db[i,]$answer <- "not_defined"
    }else{
      trial_db[i,]$answer <- tolower(get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var correct_button ([LEFTRIGH]+)"))
    }
    # score questions in Spanish and English
    if(!is.na(trial_db[i,]$question)){
      if(language == "ES"){
        trial_db[i,]$correct <- ifelse((trial_db[i,]$response == "right" & trial_db[i,]$answer == "NO") | ((trial_db[i,]$response == "left" & trial_db[i,]$answer == "SI")),1,0)
      }else if(language == "ENG"){
        trial_db[i,]$correct <- ifelse((trial_db[i,]$response == trial_db[i,]$answer) ,1,0)
      }
    }

    # Get display change information
    trial_db[i,]$dc_start_timestamp <- get_trial_info_from_msg(trial_msg, "MSG\\t(\\d+) DC started")
    trial_db[i,]$dc_end_timestamp <- get_trial_info_from_msg(trial_msg, "MSG\\t(\\d+) DC completed\\. Time\\: [0-9\\.]+ ms.")
    trial_db[i,]$dc_duration <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ DC completed\\. Time\\: ([0-9\\.]+) ms.")
    trial_db[i,]$boundary_x <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var boundary ([0-9\\.]+)") %>% as.numeric
    trial_db[i,]$recording_sampling_rate <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ !MODE RECORD [CR]+ (\\d+) \\d \\d [LR]+", multiple_allowed = TRUE)

    # when was the boundary crossed? detect late display changes and j-hooks
    if(language == "ES"){
      # when was the boundary crossed in each eye according to samples?
      trial_db[i,]$timestamp_boundary_l <- sample %>% filter(timestamp > trial_db[i,]$timestamp_display_on & x_l > trial_db[i,]$boundary_x) %>% slice(which.min(timestamp)) %>% .$timestamp
      trial_db[i,]$timestamp_boundary_r <- sample %>% filter(timestamp > trial_db[i,]$timestamp_display_on & x_r > trial_db[i,]$boundary_x) %>% slice(which.min(timestamp)) %>% .$timestamp
      ## previous fixation before boundary
      previous_fixation_l <- fixations %>% filter(eye == "L" & t_start < trial_db[i,]$timestamp_boundary_l) %>% slice_max(t_start)
      previous_fixation_r <- fixations %>% filter(eye == "R" & t_start < trial_db[i,]$timestamp_boundary_r) %>% slice_max(t_start)
      trial_db[i,]$prev_fix_l_start <- previous_fixation_l$t_start
      trial_db[i,]$prev_fix_l_end <- previous_fixation_l$t_end
      trial_db[i,]$prev_fix_r_start <- previous_fixation_r$t_start
      trial_db[i,]$prev_fix_r_end <- previous_fixation_r$t_end
      trial_db[i,]$prev_fix_l_x <- previous_fixation_l$x
      trial_db[i,]$prev_fix_l_y <- previous_fixation_l$y
      trial_db[i,]$prev_fix_r_x <- previous_fixation_r$x
      trial_db[i,]$prev_fix_r_y <- previous_fixation_r$y
      ## subsequent fixation after boundary
      subsequent_fixation_l <- fixations %>% filter(eye == "L" & t_end > trial_db[i,]$timestamp_boundary_l) %>% slice_min(t_end)
      subsequent_fixation_r <- fixations %>% filter(eye == "R" & t_end > trial_db[i,]$timestamp_boundary_r) %>% slice_min(t_end)
      trial_db[i,]$next_fix_l_start <- subsequent_fixation_l$t_start %>% as.numeric
      trial_db[i,]$next_fix_l_end <- subsequent_fixation_l$t_end
      trial_db[i,]$next_fix_r_start <- subsequent_fixation_r$t_start
      trial_db[i,]$next_fix_r_end <- subsequent_fixation_r$t_end
      trial_db[i,]$next_fix_l_x <- subsequent_fixation_l$x
      trial_db[i,]$next_fix_l_y <- subsequent_fixation_l$y
      trial_db[i,]$next_fix_r_x <- subsequent_fixation_r$x
      trial_db[i,]$next_fix_r_y <- subsequent_fixation_r$y
    }else{
      if(language == "TR" | language == "ENG"){
        #trial_db[i,]$timestamp_boundary_l <- NA
        trial_db[i,]$timestamp_boundary_r <- sample %>% filter(timestamp > trial_db[i,]$timestamp_display_on & x > trial_db[i,]$boundary_x) %>% slice(which.min(timestamp)) %>% .$timestamp
        ## previous fixations
        #previous_fixation_l <- NA
        previous_fixation_r <- fixations %>% filter(eye == "R" & t_start < trial_db[i,]$timestamp_boundary_r) %>% slice_max(t_start)
        #trial_db[i,]$prev_fix_l_start <- NA
        #trial_db[i,]$prev_fix_l_end <- NA
        if(nrow(previous_fixation_r) != 0){
          trial_db[i,]$prev_fix_r_start <- previous_fixation_r$t_start
          trial_db[i,]$prev_fix_r_end <- previous_fixation_r$t_end
          trial_db[i,]$prev_fix_r_x <- previous_fixation_r$x
          trial_db[i,]$prev_fix_r_y <- previous_fixation_r$y

        }
        #trial_db[i,]$prev_fix_l_x <- NA
        #trial_db[i,]$prev_fix_l_y <- NA
        ## subsequent fixations
        #subsequent_fixation_l <- NA
        subsequent_fixation_r <- fixations %>% filter(eye == "R" & t_end > trial_db[i,]$timestamp_boundary_r) %>% slice_min(t_end)
        #trial_db[i,]$next_fix_l_start <- NA
        #trial_db[i,]$next_fix_l_end <- NA
        if(nrow(subsequent_fixation_r) != 0){
          trial_db[i,]$next_fix_r_start <- subsequent_fixation_r$t_start
          trial_db[i,]$next_fix_r_end <- subsequent_fixation_r$t_end
          #trial_db[i,]$next_fix_l_x <- NA
          #trial_db[i,]$next_fix_l_y <- NA
          trial_db[i,]$next_fix_r_x <- subsequent_fixation_r$x
          trial_db[i,]$next_fix_r_y <- subsequent_fixation_r$y
        }
      }
    }

    # save the msgs just in case
    trial_msgs[[i]] <- trial_msg

    # get_word_info_from_msg makes a data frame with all the words in the sentence from the messages written by Opensesame into the ASC at each trial start
    trial_words[[i]] <- get_word_info_from_msg(trial_msg)

    # save Eyelink saccade detection algorithm information

    trial_fixations[[i]] <- fixations
    trial_saccades[[i]] <- saccades
    trial_blinks[[i]] <- blinks

  }

  # make sure variables are in the correct format in trial_db before we save it
  # trial_db <- trial_db %>% mutate(
  #   response = case_match(.x = trial_db$response,
  #                         "1" ~ "si",
  #                         "2" ~ "no",
  #                         "left" ~ "si",
  #                         "right" ~ "no"
  #                         ),
  #   ) %>% mutate(across(contains("timestamp"), as.numeric),
  #                across(contains("start"), as.numeric),
  #                across(contains("end"), as.numeric),
  #                #across(contains("_t"), as.numeric),
  #                across(contains("duration"), as.numeric),
  #                across(contains("_x"), as.numeric),
  #                across(contains("_y"), as.numeric)) %>%
  #   mutate(boundary_diff_prev = )
  #
  subject$trial_db <- trial_db
  subject$trial_msgs <- trial_msgs
  subject$trial_samples <- trial_samples
  subject$trial_words <- trial_words

  subject$trial_fixations <- trial_fixations
  subject$trial_saccades <- trial_saccades
  subject$trial_blinks <- trial_blinks
  return(subject)
}

