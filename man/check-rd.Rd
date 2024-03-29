% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_Rd.R
\name{check-rd}
\alias{check-rd}
\alias{check_Rd_examples}
\alias{check_Rd_value}
\title{Check Rd file}
\usage{
check_Rd_examples(path = "man/", ignore_doc_type = c("data", "package"))

check_Rd_value(path = "man/", ignore_doc_type = c("data", "package"))
}
\arguments{
\item{path}{Path to Rd files, typically \code{man/}}

\item{ignore_doc_type}{Ignore specific documentation type.
Default is to ignore documentation for datasets and package. Use \code{NULL} to cjeck all files.}
}
\value{
Character vector of Rd files path.
}
\description{
Check documentation files to be sure each function have an example and a return value.
}
\examples{
\dontrun{

# Check if Rd files have examples
check_Rd_examples()

# Check if Rd files have value (function return)
check_Rd_value()

}
}
