% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_mse.R
\name{score_mse}
\alias{score_mse}
\alias{scoreMSE}
\title{Mean Square Error}
\usage{
score_mse(object, data = NULL, y = NULL, ...)

scoreMSE(object)
}
\arguments{
\item{object}{An object of class \code{explainer} created with function
\code{\link[DALEX]{explain}} from the DALEX package.}

\item{data}{New data that will be used to calculate the score.
Pass \code{NULL} if you want to use \code{data} from \code{object}.}

\item{y}{New y parameter will be used to calculate score.}

\item{...}{Other arguments dependent on the type of score.}
}
\value{
An object of class \code{auditor_score}.
}
\description{
Mean Square Error.
}
\examples{
dragons <- DALEX::dragons[1:100, ]

# fit a model
model_lm <- lm(life_length ~ ., data = dragons)

lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)

# calculate score
score_mse(lm_audit)

}
\seealso{
\code{\link{score}}
}
