#' Create Dummy Variables from Categorical Data
#'
#' Create dummy variables from categorical data.
#' This package can convert categorical data (factor and ordered) into
#' dummy variables and handle multiple columns simultaneously.
#' This package enables to select whether a dummy variable for base group
#' is included (for principal component analysis/factor analysis) or
#' excluded (for regression analysis) by an option.
#' \code{makedummies} function accepts
#' \code{data.frame}, \code{matrix}, and
#' \code{tbl} (tibble) class (by \code{tibble} package).
#' \code{matrix} class data is automatically converted to
#' \code{data.frame} class.
#'
#' @param dat data of \code{data.frame}, \code{matrix},
#' or \code{tbl} class
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
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = factor(rep(1:3, each = 3))
#' )
#' makedummies(dat)
#'
#' ## data including NA
#'
#' dat <- data.frame(
#'     x = factor(rep(c("a", "b", "c"), each = 3)),
#'     y = rep(1:3, each = 3)
#' )
#' dat$x[4] <- NA; dat$y[6] <- NA
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
#'    \item{FALSE}{(default) : exclude the first dummy variable for base group}
#'    \item{string}{: exclude a selected dummy variable for base group}
#'   }
#' @param col Columns vector (all columns are used if \code{NULL} is given)
#' @param numerical Columns vector converting from \code{factor/ordered} to \code{numeric} (ignore if column is \code{numeric})
#' @param as.is Columns vector not converting
#'
#' @examples
#' mini_iris <- iris[c(1, 51, 101), 'Species', drop=FALSE]
#' 
#' makedummies(mini_iris, basal_level = FALSE) # default
#' makedummies(mini_iris, basal_level = TRUE)
#' makedummies(mini_iris, basal_level = "versicolor")
#'
#' @export
#'
makedummies.default <- function(dat, basal_level = FALSE,
                                col = NULL, numerical = NULL,
                                as.is = NULL, ...) {
    result <- convert_dummies(dat, basal_level, col, numerical, as.is)
    rownames(result) <- rownames(dat)
    return(result)
}


#' @rdname makedummies
#' @method makedummies matrix
#'
#' @export
#'
makedummies.matrix <- function(dat, ...) {
    dat <- data.frame(dat)
    makedummies.default(dat, ...)
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
#'
#'   # non-standard variable name
#'   dat2 <- tibble(
#'       `1` = factor(rep(c("c", "a", "b"), each = 3)),
#'       `@` = factor(rep(1:3, each = 3)),
#'       `&` = rep(4:6, each = 3)
#'   )
#'   dat2
#'
#'   makedummies(dat2, basal_level = TRUE)
#'   makedummies(dat2, as.is = "@", basal_level = TRUE)
#'   makedummies(dat2, numerical = "1", basal_level = TRUE)
#' }
#'
#' @export
#'
makedummies.tbl <- function(dat, basal_level = FALSE,
                            col = NULL, numerical = NULL,
                            as.is = NULL, ...) {
    result <- convert_dummies(dat, basal_level, col, numerical, as.is)
    return(as_tibble(result))
}


########################################
# internal functions
########################################

dummy_matrix <- function(dat, basal_level) {
    name <- names(dat)
    dat <- dat[[1]]
    level <- levels(droplevels(dat))

    m <- length(dat)
    n <- length(level)

    res <- matrix(0L, m, n)
    res[cbind(seq.int(m), dat)] <- 1L
    colnames(res) <- paste(name, level, sep = "_")

    ## setting NA
    res[is.na(dat),] <- NA

    ## basal_level option => delete basal level
    if (n <= 1L) {
        NULL # Ensure n > 1 to run subsequent conditions
    } else if (identical(basal_level, FALSE)) {
        res <- res[, -1L, drop = FALSE]
    } else if (is.character(basal_level)) {
        res <- res[, level != match.arg(basal_level, level), drop = FALSE]
    }

    if (ncol(res) == 1) {
        colnames(res) <- name
    }
    return(res)
}


convert_dummies <- function(dat, basal_level, col, numerical, as.is) {
    ## names of column
    if (is.null(col)) {
        name_col <- colnames(dat)
    } else {
        name_col <- unique(col)
    }

    ## process each column
    for (i in seq_along(name_col)) {
        name <- name_col[i]
        tmp <- dat[,name, drop = FALSE]

        if (!is.factor(tmp[[1]]) || (name %in% as.is)) {
            ## (non-factor and non-ordered) or as.is option is TRUE
            res <- tmp
        } else if (!(name %in% numerical)) {
            ## factor or ordered
            ## convert dummy variables
            res <- dummy_matrix(tmp, basal_level)
        } else {
            ## factor or ordered
            ## numerical option is TRUE => convert numeric
            res <- data.matrix(tmp)
        }

        if (i == 1) {
            result <- res
        } else {
            result <- cbind(result, res)
        }
    }
    return(result)
}

