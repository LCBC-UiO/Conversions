% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bids-functions.R
\name{bidsify}
\alias{bidsify}
\title{Create BIDS type structure}
\usage{
bidsify(ID, session, site = NULL, type = "file")
}
\arguments{
\item{ID}{CrossProject_ID}

\item{session}{Subject_Timepoint}

\item{site}{Site_Name}

\item{type}{"file" or "folder"}
}
\value{
character in BIDS compliant format
}
\description{
Function to return the equivalent
BIDS-type reference for a specific 
set of data.
}
\examples{
bidsify(1100300, 2)
bidsify(1100300, 2, "ousAvanto")
bidsify(1100300, 2, "ousAvanto", type = "folder")
}
