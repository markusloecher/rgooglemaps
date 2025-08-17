\name{incidents}
\alias{incidents}

\docType{data}
\title{San Francisco crime data}
\usage{data(incidents)}
\description{
The \code{incidents} data frame has 5000 rows and 16 columns.
These are 5000 random rows from the 2012 crime data recorded in San Francisco.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{IncidntNum}{incident number assigned by the police}
    \item{Category}{Category of crime}
    \item{Descript}{longer description}
    \item{DayOfWeek}{day of week}
    \item{Date}{date}
    \item{Time}{time of day formatted as hh:mm }
    \item{PdDistrict}{police district}
    \item{Resolution}{was the crime resolved?}
    \item{Location}{location as address}
    \item{lon}{longitude}
    \item{lat}{latitude}
    \item{violent}{violent flag}
    \item{HrOfDay}{hour of day as 2-digit integer}
    \item{TimeOfDay}{hour of day as decimal number}
    \item{HourOfWeek}{hour of week as decimal number between 0-168}
    \item{censusBlock}{ID of census block}
  }
}
\details{
crime data recorded in San Francisco
}
\source{
URL https://data.sfgov.org/
}


\examples{
data(incidents)
table(incidents$Category)

}
\keyword{datasets}
