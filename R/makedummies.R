makedummies <- function(dat, basal_level = FALSE, sep = "_") {
    n_col <- ncol(dat)
    name_col <- colnames(dat)
    name_row <- rownames(dat)

    result <- NULL
    for (i in seq(n_col)) {
        ## process each column
        tmp <- dat[,name_col[i]]
        if (is.factor(tmp)) {
            ## factor or ordered => convert dummy variables
            level <- levels(droplevels(tmp))
            m <- length(tmp)
            n <- length(level)
            res <- matrix(0, m, n)
            res[cbind(seq(m), tmp)] <- 1
            colnames(res) <- paste(name_col[i], level, sep = sep)
            if (basal_level == FALSE) {
                res <- res[,-1]
            }
        } else {
            ## non-factor or non-ordered => as-is
            res <- as.matrix(tmp)
            colnames(res) <- name_col[i]
        }
        result <- cbind(result, res)
    }
    rownames(result) <- name_row
    return(data.frame(result))
}
