% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{login}
\alias{login}
\title{Login to 4CastHub}
\usage{
login(sleep_time = 90, ...)
}
\arguments{
\item{sleep_time}{Maximum waiting for URI authentication}

\item{...}{PARAM_DESCRIPTION}
}
\value{
A file with your authentication will be saved at your package location.
}
\description{
This function is used to validate the two factor authentication of your 4CastHub user. If you select the checkbox 'Remember this device por 30 days', the login only needs to be done once every 30 days.
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 faas4i::login()
 ## Once the url is printed, copy and paste it to your browser and follow with authentication
 }
}
}
\seealso{
\code{\link[jsonlite]{toJSON, fromJSON}}
}
