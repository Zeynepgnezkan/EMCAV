% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_word_info_from_msg.R
\name{get_word_info_from_msg}
\alias{get_word_info_from_msg}
\title{Get Word Information from Trial Message}
\usage{
get_word_info_from_msg(trial_msg)
}
\arguments{
\item{trial_msg}{A character vector containing lines of trial messages.}
}
\value{
A data frame with word information including trial number, sentence number, word number, word text, and word right boundary.
}
\description{
This function extracts word information from a trial message containing lines related to words. It searches for lines containing the "WORD" pattern and parses relevant information from them to create a data frame with word-related details.
}
