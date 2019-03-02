#' Create Dummy Variables from Categorical Data
#'
#' Create dummy variables from categorical data.
#' This package can convert categorical data (factor and ordered) into
#' dummy variables and handle multiple columns simultaneously.
#' This package enables to select whether a dummy variable for base group
#' is included (for principal component analysis/factor analysis) or
#' excluded (for regression analysis) by an option.
#' \code{makedummies} function accepts
#' \code{data.frame} class and
#' \code{tbl} (tibble) class (by \code{tibble} package).
#'
#' @param dat data of \code{data.frame} or \code{tbl} class
#' @param \dots arguments to makedummies.data.frame (\code{tbl} class)
#'
#' @return return as \code{data.frame} or \code{tbl} class
#'
#' @note Pull Request #1 (add column name when when columns has binary value) (\url{https://github.com/toshi-ara/makedummies/pull/1}). Thanks to Kohki YAMAGIWA for the contribution.
#'
#' @examples
#' #### 'data.frame' class
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
#'
#' @export
#'
makedummies <- function(dat, ...) UseMethod("makedummies")


#' @rdname makedummies
#' @method makedummies default
#'
#' @param basal_level logical
#'   \describe{
#'    \item{TRUE}{: include a dummy variable for base group}
#'    \item{FALSE}{(default) : exclude a dummy variable for base group}
#'   }
#' @param col Columns vector (all columns are used if \code{NULL} is given)
#' @param numerical Columns vector converting from \code{factor/ordered} to \code{numeric} (ignore if column is \code{numeric})
#' @param as.is Columns vector not converting
#'
#' @export
#'
makedummies.default <- function(dat, basal_level = FALSE,
                                col = NULL, numerical = NULL,
                                as.is = NULL, ...) {
    ## names of column and row
    if (is.null(col)) {
        name_col <- colnames(dat)
    } else {
        name_col <- unique(col)
    }
    name_row <- rownames(dat)

    ## process each column
    for (i in seq_along(name_col)) {
        name <- name_col[i]
        tmp <- dat[,name]

        if (!is.factor(tmp) || (name %in% as.is)) {
            ## (non-factor and non-ordered) or as.is option
            res <- data.frame(tmp)
            colnames(res) <- name
        } else if (!(name %in% numerical)) {
            ## factor or ordered
            ## convert dummy variables
            level <- levels(droplevels(tmp))
            m <- length(tmp)
            n <- length(level)
            res <- matrix(0L, m, n)
            res[cbind(seq.int(m), tmp)] <- 1L
            colnames(res) <- paste(name, level, sep = "_")

            ## basal_level option => delete basal level
            if (basal_level == FALSE && (n > 1)) {
                res <- res[, -1, drop = FALSE]
            }

            res <- data.frame(res)
            if (ncol(res) == 1) {
                colnames(res) <- name
            }
        } else {
            ## factor or ordered
            ## numerical option => convert numeric
            res <- as.matrix(as.numeric(tmp))
            colnames(res) <- name
        }

        if (i == 1) {
            result <- data.frame(res)
        } else {
            result <- data.frame(result, res)
        }
    }

    rownames(result) <- name_row
    return(result)
}


#' @rdname makedummies
#' @method makedummies tbl
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#' #### 'tibble' class
#' if (require(tibble)) {
#'   dat <- as_tibble(iris)
#'   makedummies(dat[46:55,], col = "Species", basal_level = TRUE)
#' }
#'
#' @export
#'
makedummies.tbl <- function(dat, ...) {
    dat <- data.frame(dat)
    tibble::as_tibble(makedummies.default(dat, ...))
}

