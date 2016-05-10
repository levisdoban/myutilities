\name{EnsurePackage}
\alias{EnsurePackage}
\title{Ensure library is loaded}
\usage{
EnsurePackage(x)
}
\description{
This function checks whether a library has been installed. If the library has been installed, it loads it. Otherwise, it installes, then loads the library. It works with prepare RR.
}
\examples{
EnsurePackage("data.table")
}
