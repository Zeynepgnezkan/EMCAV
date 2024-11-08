% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_trial_info_from_msg.R
\name{get_trial_info_from_msg}
\alias{get_trial_info_from_msg}
\title{Get Trial Information from Messages}
\usage{
get_trial_info_from_msg(text_vector, regex_pattern, multiple_allowed = FALSE)
}
\arguments{
\item{text_vector}{A character vector with multiple lines of text.}

\item{regex_pattern}{A regex pattern with exactly one capture group for matching lines in the text.}

\item{multiple_allowed}{Logical indicating whether multiple matches are allowed. If TRUE, the function returns the first match and displays a warning; if FALSE, an error is thrown for multiple matches.}
}
\value{
The matched content of the capture group or NA if no match is found.
}
\description{
This function extracts information from a character vector containing multiple lines of text based on a provided regex pattern with exactly one capture group. It is designed to retrieve relevant information from messages in an ASC file.
}
\examples{
# Create a character vector of messages
messages <- c("!MODE RECORD", "Trial: 42", "Subject: John Doe")

# Extract trial information using regex
trial_number <- get_trial_info_from_msg(messages, "Trial: (\\\\d+)")

}
