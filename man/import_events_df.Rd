% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_events_df.R
\name{import_events_df}
\alias{import_events_df}
\title{import_events_df}
\usage{
import_events_df(
  input,
  stat_unit = "stat_unit",
  date = "date",
  tag = "tag",
  optional_data,
  date_format_func = (function(x) lubridate::parse_date_time(x, date_format_reg)),
  date_format_reg = "ymd-HMS",
  force_date_format = FALSE,
  model = analysr_env
)
}
\arguments{
\item{input}{A data frame.}

\item{stat_unit}{A string containing the stat_unit label.}

\item{date}{A string containing the date label.}

\item{tag}{A string containing the tag label.}

\item{optional_data}{A vector containing label to import in descriptions
table.}

\item{date_format_func}{A function to format date with (not required).
Default: \code{lubridate::parse_date_time(x, date_format_reg)}
If you want to use milliseconds \href{https://bit.ly/33JGr6s}{look at this}.}

\item{date_format_reg}{A expression to format date with (not required).
Default: \code{"ymd-HMS"}
For more details see \href{https://bit.ly/3bp3FD0}{this documentation}.}

\item{force_date_format}{Boolean to force date format func (not required).
Default: \code{FALSE}}

\item{model}{An AnalysR env.
Default: \code{analysr_env}}
}
\value{
A boolean (\code{TRUE} if no errors)
}
\description{
Import events from a data frame.
}
