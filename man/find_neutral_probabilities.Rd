% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fair_model.R
\name{find_neutral_probabilities}
\alias{find_neutral_probabilities}
\title{find_neutral_probabilities}
\usage{
find_neutral_probabilities(gsat1, p_threshold = 0.67)
}
\arguments{
\item{gsat1}{a data frame of warming temperature for global, national ffi and afolu emissions}

\item{p_threshold}{the probability threshold of interest used to classify neutrality combinations}
}
\value{
a dataframe giving probabilities of neutrality before neutral_year, neutral_year-5 and neutral_year -10.
}
\description{
The probability of temperature neutrality occurring before a given year neutral year, currently fixed at 2050. A specified probability
threshold is use to classify neutrality in 5-yearly increment e.g. 67% probability of neutrality before 2046 etc.
}
