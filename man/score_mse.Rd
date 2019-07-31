% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_mse.R
\name{score_mse}
\alias{score_mse}
\alias{scoreMSE}
\title{Mean Square Error}
\usage{
score_mse(object)

scoreMSE(object)
}
\arguments{
\item{object}{An object of class 'model_audit' or 'model_residual'.}
}
\value{
an object of class score_audit
}
\description{
Mean Square Error.
}
\examples{
dragons <- DALEX::dragons[1:100, ]
lm_model <- lm(life_length ~ ., data = dragons)
lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
score_mse(lm_au)

}
\seealso{
\code{\link{score}}
}