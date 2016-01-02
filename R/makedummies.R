makedummies <- function(dat, basal_level = FALSE, col = NULL) {
    if (is.null(col)) {
        name_col <- colnames(dat)
    } else {
        name_col <- unique(col)
    }
    name_row <- rownames(dat)

    result <- NULL
    for (name in name_col) {
        ## process each column
        tmp <- dat[,name]
        if (is.factor(tmp)) {
            ## factor or ordered => convert dummy variables
            level <- levels(droplevels(tmp))
            m <- length(tmp)
            n <- length(level)
            res <- matrix(0, m, n)
            res[cbind(seq(m), tmp)] <- 1
            colnames(res) <- paste(name, level, sep = "_")
            if (basal_level == FALSE) {
                res <- res[,-1]
            }
        } else {
            ## non-factor or non-ordered => as-is
            res <- as.matrix(tmp)
            colnames(res) <- name
        }
        result <- cbind(result, res)
    }
    rownames(result) <- name_row
    return(data.frame(result))
}
