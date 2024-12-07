% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayasNumericInput.R
\name{bayasNumericInput}
\alias{bayasNumericInput}
\title{BAYAS Numeric Input}
\usage{
\code{bayasNumericInput(
  inputId,
  label,
  value,
  min = NA,
  max = NA,
  step = NA,
  integer = F,
  placeholder = 100,
  emptyWarning = T,
  invalidMessage = NULL,
  invalidTooltip = NULL,
  showMessages = !is.null(invalidMessage),
  numericStyle = NULL
)}
}
\arguments{
\item{inputId}{The input slot that will be used to access the value.}

\item{label}{Display label for the control, or NULL for no label.}

\item{value}{Initial value.}

\item{min}{Minimum allowed value.}

\item{max}{Maximum allowed value.}

\item{step}{Interval to use when stepping between min and max.}

\item{integer}{TRUE if only integer values are allowed.}

\item{invalidMessage}{Showed message for invalid values.}

\item{placeholder}{A character string or number giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.}

\item{emptyWarning}{If true (default) empty inputs are invalid.}

\item{invalidTooltip}{Tooltip when hover over the message.}

\item{numericStyle}{Style arguments for the input itself}

\item{showMessages}{If invalidMessage should be shown.}

\item{...}{Further arguments passed to outer div.}
}
\description{
Creates a numeric input similar to shinys numericInput but with a few additional options.
If a value exceeds the given min and max or if the value is not numeric
(or not an integer) a message is shown and the numeric input is bordered red.
Also the return value is slightly different, see the section below 'Server return'.
}
\section{Server return}{

Using the common shiny access via 'input'.
Returns a vector of two values. 0/1 for an (in)valid value and
the value itself.
}

