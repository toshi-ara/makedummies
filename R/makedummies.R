#' Create Dummy Variables from Categorical Data
#'
#' Create dummy variables from categorical data.
#' This package can convert categorical data (factor and ordered) into
#' dummy variables and handle multiple columns simultaneously.
#' This package enables to select whether a dummy variable for base group
#' is included (for principal component analysis/factor analysis) or
#' excluded (for regression analysis) by an option.

#' @param dat \code{data.frame}
#' @param basal_level
#'   \describe{
#'    \item{TRUE}{: include a dummy variable for base group}
#'    \item{FALSE}{(default) : exclude a dummy variable for base group}
#'   }
#' @param col Columns vector (all columns are used if \code{NULL} is given)
#' @param numerical Columns vector converting from \code{factor/ordered} to \code{numeric} (ignore if column is \code{numeric})
#' @param as.is Columns vector not converting
#' @return return as \code{data.frame}
#'
#' @note \code{tbl} class is also accepted from version 1.1 (returned as \code{data.frame}).
#' @note Pull Request #1 (add column name when when columns has binary value) (\url{https://github.com/toshi-ara/makedummies/pull/1}). Thanks to Kohki YAMAGIWA for the contribution.
#'
#' @export

#' @examples
#' ## factor
#' dat <- data.frame(x = factor(rep(c("a", "b", "c"), each = 3)))
#' dat$x
#' makedummies(dat)
#'
#' ## ordered
#' dat <- data.frame(x = factor(rep(c("a", "b", "c"), each = 3)))
#' dat$x <- ordered(dat$x, levels = c("a" ,"c" ,"b"))
#' dat$x
#' makedummies(dat)
#'
#' ## numeric
#' dat <- data.frame(x = rep(1:3, each = 3))
#' makedummies(dat)
#'
#' ## factor and numeric
#' dat <- data.frame(
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = rep(1:3, each = 3)
#' )
#' makedummies(dat)
#'
#' ## factors
#' dat <- data.frame(
#' x = factor(rep(c("a", "b", "c"), each = 3)),
#' y = factor(rep(1:3, each = 3))
#' )
#' makedummies(dat)
#'
#' ## "col" option
#' dat <- data.frame(
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = factor(rep(1:3, each = 3))
#' )
#' makedummies(dat, col = "x")
#'
#' ## "numerical" option
#' dat <- data.frame(
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = factor(rep(1:3, each = 3))
#' )
#' makedummies(dat, numeric = "x")
#'
#' dat <- data.frame(
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = rep(4:6, each = 3)
#' )
#' dat$x <- ordered(dat$x, levels = c("a" ,"c" ,"b"))
#' dat
#' dat$x
#' makedummies(dat, numeric = c("x", "y"))
#'
#' ## "as.is" option
#' dat <- data.frame(
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = factor(rep(1:3, each = 3))
#' )
#' dat
#' makedummies(dat, as.is = "x")
#' makedummies(dat, as.is = c("x", "y"))

makedummies <- function(dat, basal_level = FALSE, col = NULL, numerical = NULL, as.is = NULL) {
    ## convert tbl data to data.frame
    if (inherits(dat, "tbl")) dat <- data.frame(dat)

    ## names of column and row
    if (is.null(col)) {
        name_col <- colnames(dat)
    } else {
        name_col <- unique(col)
    }
    name_row <- rownames(dat)

    ## process each column
    for (i in seq(length(name_col))) {
        name <- name_col[i]
        tmp <- dat[,name]
        if (name %in% as.is) { ## as.is option
            res <- data.frame(tmp)
            colnames(res) <- name
        } else if (is.factor(tmp)) { ## factor or ordered
            if (name %in% numerical) { ## numerical option => convert numeric
                res <- as.matrix(as.numeric(tmp))
                colnames(res) <- name
            } else { ## convert dummy variables
                level <- levels(droplevels(tmp))
                m <- length(tmp)
                n <- length(level)
                res <- matrix(0, m, n)
                res[cbind(seq(m), tmp)] <- 1
                colnames(res) <- paste(name, level, sep = "_")
                ## basal_level option => delete basal level
                if (basal_level == FALSE) {
                    res <- res[,-1]
                }
                res <- data.frame(res)
                colnames(res) <- name
            }
        } else { ## non-factor and non-ordered => as-is
            res <- data.frame(tmp)
            colnames(res) <- name
        }
        if (i == 1) {
            result <- data.frame(res)
        } else {
            result <- data.frame(result, res)
        }
    }
    return(result)
}

