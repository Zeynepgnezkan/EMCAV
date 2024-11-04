#' Merge Adjacent Fixations Based on Duration and Distance
#'
#' This function merges adjacent fixations in an eye-tracking dataset based on specified
#' maximum duration and spatial distance criteria. Using a loop, it checks each fixation
#' and determines if it should be merged with the previous or next fixation.
#'
#' @param df A data frame containing fixation data, including columns `x`, `y`, `duration`,
#'        `t_start`, and `t_end`.
#' @param max_duration Numeric. The maximum duration (in milliseconds) under which a fixation
#'        can be considered for merging. Defaults to 80 ms.
#' @param max_distance Numeric. The maximum distance (in pixels) under which a fixation
#'        can be merged with an adjacent fixation. Defaults to 12 pixels.
#' @param rate Numeric. The sampling rate of the eye-tracker in Hz. Used to adjust
#'        the merged fixation duration. Defaults to 1000 Hz.
#'
#' @return A data frame with the same columns as `df`, with updated `t_start`, `t_end`,
#'         and `duration` values for merged fixations. A new column `merged` is added,
#'         indicating whether a fixation was merged (`TRUE` or `FALSE`).
#'
#' @details This function iterates through each fixation and checks if the duration is below
#'          `max_duration` and if the spatial distance to the previous or next fixation is
#'          below `max_distance`. If these conditions are met, the fixation is merged with
#'          the adjacent fixation, and `t_start`, `t_end`, and `duration` values are updated.
#'          The function issues a warning if there are fewer than 3 fixations, as merging
#'          cannot occur in this case.
#'
#' @examples
#' # Example usage:
#' merged_fixations <- merge_fixations_loop(df, max_duration = 80, max_distance = 12, rate = 1000)
#'
#' @export
merge_fixations_loop <-
  function(df,
           max_duration = 80,
           max_distance = 12,
           rate = 1000) {
    merged_df <- df
    merged_df$merged <- FALSE

    n_fixations <- nrow(df)

    if (n_fixations >= 3) {
      for (i in 2:(nrow(df) - 1)) {
        current_fixation <- df[i, ]
        prev_fixation <- df[i - 1, ]
        next_fixation <- df[i + 1, ]

        if (current_fixation$duration < max_duration) {
          distance_to_prev <-
            sqrt((current_fixation$x - prev_fixation$x) ^ 2 + (current_fixation$y - prev_fixation$y) ^2)
          distance_to_next <-
            sqrt((current_fixation$x - next_fixation$x) ^ 2 + (current_fixation$y - next_fixation$y) ^2)

          if (distance_to_prev <= max_distance) {
            merged_df$t_end[i - 1] <- current_fixation$t_end
            merged_df$duration[i - 1] <-
              merged_df$t_end[i - 1] - prev_fixation$t_start + 1000/rate
            merged_df$merged[i] <- TRUE
          } else if (distance_to_next <= max_distance) {
            merged_df$t_start[i + 1] <- current_fixation$t_start
            merged_df$duration[i + 1] <-
              next_fixation$t_end - merged_df$t_start[i + 1] + 1000/rate
            merged_df$merged[i] <- TRUE
          }
        }
      }


    } else{
      warning("Trial has fewer than 3 fixations. Not merging fixations.")
    }
    return(merged_df)
  }
