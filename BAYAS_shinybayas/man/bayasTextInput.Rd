% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayasTextInput.R
\name{bayasTextInput}
\alias{bayasTextInput}
\title{Create a text input control (similar to shinys textInput)}
\usage{
\code{bayasTextInput(
  inputId="textInputName",
  label="Name",
  value="personal name",
  width="200px",
  placeholder="name"
)}
}
\arguments{
\item{inputId}{The input slot that will be used to access the value.}

\item{label}{Display label for the control, or NULL for no label}

\item{value}{Initial value.}

\item{width}{The width of the input, e.g. '400px', or '100%'}

\item{placeholder}{A character string giving the user a hint as to what can be entered into the control. Internet Explorer 8 and 9 do not support this option.}
}
\description{
Create an input control for entry of unstructured text values, that is triggered
when the focus is lost or the 'Enter' key is pressed.
}
