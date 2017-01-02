\name{makedummies}
\alias{makedummies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{makedummies}
\description{
Create dummy variables from categorical data.
This package can convert categorical data (factor and ordered) into
dummy variables and handle multiple columns simultaneously.
This package enables to select whether a dummy variable for base group
is included (for principal component analysis/factor analysis) or
excluded (for regression analysis) by an option.
}
\usage{
makedummies(dat, basal_level = FALSE, col = NULL, numerical = NULL, as.is = NULL)
}
\arguments{
  \item{dat}{
    data.frame
  }
  \item{basal_level}{
    \code{TRUE}: include a dummy variable for base group

    \code{FALSE} (default): exclude a dummy variable for base group
  }
  \item{col}{
    Columns vector (all columns are used if NULL is given)
  }
  \item{numerical}{
    Columns vector converting from factor/ordered to numeric (ignore if column is numeric)
  }
  \item{as.is}{
    Columns vector not converting
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

## "col" option
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat, col = "x")

## "numerical" option
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
makedummies(dat, numeric = "x")

dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = rep(4:6, each = 3)
)
dat$x <- ordered(dat$x, levels = c("a" ,"c" ,"b"))
dat
dat$x
makedummies(dat, numeric = c("x", "y"))

## "as.is" option
dat <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 3)),
    y = factor(rep(1:3, each = 3))
)
dat
makedummies(dat, as.is = "x")
makedummies(dat, as.is = c("x", "y"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{function}
