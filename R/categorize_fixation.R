#' Categorize Fixations in a Sequence
#'
#' This function categorizes fixations in a given sequence of words into three categories:
#' "first pass", "first pass refixation", and "other fixation". The function uses the
#' unique occurrences of words to determine which fixations can be classified as first
#' pass fixations. A word can receive first pass fixations only once, and any subsequent
#' fixations on the same word are classified as refixations if they follow a first pass fixation.
#'
#' @param sequence A numeric vector representing a sequence of word numbers where fixations occurred.
#' Each number corresponds to a specific word in the text being read.
#'
#' @return A character vector of the same length as the input `sequence`, with each element
#' indicating the category of the corresponding fixation: "first pass", "first pass refixation",
#' or "other fixation".
#'
#' @examples
#' # Example sequence of fixations on words
#' fixation_sequence <- c(1, 2, 2, 3, 4, 5, 5, 5, 6)
#'
#' # Categorize the fixations
#' categorized_fixations <- categorize_fixation(fixation_sequence)
#'
#' # Print the results
#' print(categorized_fixations)
#'
categorize_fixation <- function(sequence) {
  categories <- character(length(sequence))
  # these words can receive first pass fixations
  first_occurrences <- unique(sequence)

  for (i in seq_along(sequence)) {
    element <- sequence[i]

    if (element %in% first_occurrences) {
      categories[i] <- "first pass"
      # remove words that no longer can receive first pass fixations
      # this includes all word numbers < current element, even if they were not fixated before
      first_occurrences <-
        setdiff(first_occurrences, min(sequence):element)
    } else if (i > 1 &&
               sequence[i] == sequence[i - 1] &
               (categories[i - 1] == "first pass" |
                categories[i - 1] == "first pass refixation")) {
      categories[i] <- "first pass refixation"
    } else {
      categories[i] <- "other fixation"
    }
  }

  return(categories)
}
