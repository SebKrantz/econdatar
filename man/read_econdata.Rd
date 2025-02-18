\name{read_econdata}
\alias{read_econdata}
\alias{econdata_tidy}
\title{
read_econdata
}
\description{
Returns the data for the given data set - ECONDATA:id(version), as a list, or as tidy \emph{data.table}'s. Available data sets can be looked up from the web platform (http://www.econdata.co.za). Tidying can be done directly within \code{read_econdata()}, or ex-post using \code{econdata_tidy()}.
}
\usage{
read_econdata(id, \dots, tidy = FALSE)

econdata_tidy(x, \dots)
}
\arguments{
  \item{id}{Data identifier.}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_econdata()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or 'all' to return all available versions. \cr
    \code{series_key} \tab\tab character. A character vector specifying a subset of time series (see the web platform (export function) for details). \cr
    \code{release} \tab\tab character or time object with format \%Y-\%m-\%dT\%H:\%M:\%S. The release description, or a date/time which will return the data as it was at that moment, or 'latest', or 'unreleased'. \cr
    \code{file} \tab\tab character. File name for retrieving JSON data from disk. \cr
    \code{username} \tab\tab character. Web username. \cr
    \code{password} \tab\tab character. Web password. \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{econdata_tidy}. If \code{TRUE}, \code{read_econdata()/econdata_tidy()} admit the following additional arguments:
  \tabular{llll}{
    \code{wide} \tab\tab logical, default: \code{TRUE}. Returns data in a column-based format, with \code{"label"} and \code{"source_identifier"} attributes to columns (when available) and an overall "metadata" attribute to the table, otherwise a long-format is returned. See Value. \cr

    \code{codelabel} \tab\tab logical, default: \code{FALSE}. If \code{wide = TRUE}, setting \code{codelabel = TRUE} the data key will be used to generate the \code{"label"}, when available. \cr

    \code{combine} \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{combine = TRUE} will combine all data and metadata into a single long table, whereas the default \code{FALSE} will return data and metadata in separate tables, for more efficient storage. \cr

    \code{origmeta}  \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{origmeta = TRUE} will combine all metadata fields attached to the series in the dataset as they are. The default is to construct a standardized set of metadata variables, and then drop those not observed. See also \code{allmeta}. \cr

    \code{allmeta} \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{allmeta = TRUE} always returns the full set of metadata fields, regardless of whether they are recorded for the given dataset. It is also possible that there are series with zero observations in a dataset. Such series are dropped in tidy output, but if \code{combine = FALSE}, \code{allmeta = TRUE} retains their metadata in the metadata table. \cr

    \code{prettymeta} \tab\tab logical, default: \code{TRUE}. Attempts to make the returned metadata more human readable replacing each code category and enumeration with its name. It is advisable to leave this set to \code{TRUE}, in some cases, where speed is paramount you may want to set this flag to \code{FALSE}. If multiple datasets are being querioed this option is automatically set to \code{FALSE}. \cr

   \code{release} \tab\tab logical, default: \code{FALSE}. \code{TRUE} allows you to apply \code{econdata_tidy()} to objects returned by \code{read_release()}. All other flags to \code{econdata_tidy()} are ignored.
}
}
}
\details{
An EconData account (http://www.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="username;password")}. If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.
}
\value{
%%  ~Describe the value returned
If \code{tidy = FALSE}, a list of data frames is returned, where the names of the list are the EconData series codes, and each data frame has a single column named 'OBS_VALUE' containing the data, with corresponding dates attached as rownames. Each data frame further has a \code{"metadata"} attribute providing information about the series. The entire list of data frames also has a \code{"metadata"} attribute, providing information about the dataset. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, a list of such lists is returned.

If \code{tidy = TRUE} and \code{wide = TRUE} (the default), a single \emph{data.table} is returned where the first column is the date, and the remaining columns are series named by their EconData codes. Each series has two attributes: \code{"label"} provides a variable label combining important metadata from the \code{"metadata"} attribute in the non-tidy format, and \code{"source.code"} gives the series code assigned by the original data provider. The table has the same dataset-level \code{"metadata"} attribute as the list of data frames if \code{tidy = FALSE}. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, a list of such \emph{data.table}'s is returned.

If \code{tidy = TRUE} and \code{wide = FALSE} and \code{compact = FALSE} (the default), a named list of two \emph{data.table}'s is returned. The first, \code{"data"}, has columns 'code', 'date' and 'value' providing the data in a long format. The second, \code{"metadata"}, provides dataset and series-level matadata, with one row for each series. If \code{compact = TRUE}, these two datasets are combined, where all repetitive content is converted to factors for more efficient storage. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, \code{compact = FALSE} gives a nested list, whereas \code{compact = TRUE} binds everything together to a single long frame. In general, if \code{wide = FALSE}, no attributes are attached to the tables or columns in the tables.

%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{write_econdata}}
}
\examples{
\dontrun{
# library(econdatar)
# Sys.setenv(ECONDATA_CREDENTIALS="username;password")
# Ids see: https://www.econdata.co.za/FusionRegistry/data/datastructure.html

# Electricity Generated
ELECTRICITY <- read_econdata(id = "ELECTRICITY")
ELECTRICITY_WIDE <- econdata_tidy(ELECTRICITY) # Or: read_econdata("ELECTRICITY", tidy = TRUE)
ELECTRICITY_LONG <- econdata_tidy(ELECTRICITY, wide = FALSE)
# Same as econdata_tidy(ELECTRICITY, wide = FALSE, combine = TRUE):
with(ELECTRICITY_LONG, metadata[data, on = "data_key"])

# CPI Analytical Series: Different Revisions
CPI_ANL <- read_econdata(id = "CPI_ANL_SERIES", version = "all")
CPI_ANL_WIDE <- econdata_tidy(CPI_ANL)
CPI_ANL_LONG <- econdata_tidy(CPI_ANL, wide = FALSE, combine = TRUE)
CPI_ANL_ALLMETA <- econdata_tidy(CPI_ANL, wide = FALSE, allmeta = TRUE) # v2.0 has some 0-obs series

# Can query a specific version by adding e.g. version = "2.0.0" to the call

# Returns 5-10 years (daily average bond yields) not yet contained in the latest release
# (particularly useful for daily data that is released monthly)
MARKET_RATES <- read_econdata(id = "MARKET_RATES", series_key = "CMJD003.B.A", release = "unreleased")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ download }% __ONLY ONE__ keyword per line
