\name{makedummies}
\alias{makedummies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{makedummies}
\description{
Create dummy variables from factor.
Distinct from using model.matrix(),
this package can handle multiple columns simultaneously,
and include or not a dummy variable of basal category by an option.
}
\usage{
makedummies(dat, basal_level = FALSE, col = NULL, numerical = NULL, as.is = NULL)
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
  \item{numerical}{
    Columns vector converting from factor/ordered to numeric (ignore if column is numeric)
  }
  \item{as.is}{
    Columns vector not converting (all columns are used if NULL is given)
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
