\name{makedummies}
\alias{makedummies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{makedummies}
\description{
A package for creating dummy variables from factor.
}
\usage{
makedummies(dat, basal_level = FALSE, col = NULL)
}
\arguments{
  \item{dat}{
    data.frame
  }
  \item{basal_level}{
    \code{TRUE}: include a dummy variable for basal level

    \code{FALSE} (default): exclude a dummy variable for basal level
  }
  \item{col}{
    Columns vector (all columns are used if NULL is given)
  }
}
\value{
return as data.frame
}
\examples{
## factor
dat <- data.frame(x = factor(rep(c("a", "b", "c"), each = 3)))
dat$x
makedummies(dat)

## ordered
dat <- data.frame(x = factor(rep(c("a", "b", "c"), each = 3)))
dat$x <- ordered(dat$x, levels = c("a" ,"c" ,"b"))
dat$x
makedummies(dat)

## numeric
dat <- data.frame(x = rep(1:3, each = 3))
makedummies(dat)

## factor and numeric
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = rep(1:3, each = 3)
)
makedummies(dat)

## factors
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat)
}

## "col" option
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat, col = "x")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{function}
