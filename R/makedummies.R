#' Create Dummy Variables from Categorical Data
#'
#' Create dummy variables from categorical data.
#' This package can convert categorical data (factor and ordered) into
#' dummy variables and handle multiple columns simultaneously.
#' This package enables to select whether a dummy variable for base group
#' is included (for principal component analysis/factor analysis) or
#' excluded (for regression analysis) by an option.

#' @param dat data.frame
#' @param basal_level
#'   \describe{
#'    \item{TRUE}{include a dummy variable for base group}
#'    \item{FALSE}{(default) exclude a dummy variable for base group}
#'   }
#' @param col Columns vector (all columns are used if NULL is given)
#' @param numerical Columns vector converting from factor/ordered to numeric (ignore if column is numeric)
#' @param as.is Columns vector not converting
#' @return return as data.frame
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

#' @keywords function


makedummies <- function(dat, basal_level = FALSE, col = NULL, numerical = NULL, as.is = NULL) {
    ## names of column and row
    if (is.null(col)) {
        name_col <- colnames(dat)
    } else {
        name_col <- unique(col)
    }
    name_row <- rownames(dat)

    result <- NULL
    ## process each column
    for (name in name_col) {
        tmp <- dat[,name]
        if (name %in% as.is) { ## as.is option
            res <- as.matrix(tmp)
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
            }
        } else { ## non-factor and non-ordered => as-is
            res <- as.matrix(tmp)
            colnames(res) <- name
        }
        result <- cbind(result, res)
    }
    rownames(result) <- name_row
    return(data.frame(result))
}
