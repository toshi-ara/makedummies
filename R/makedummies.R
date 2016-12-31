makedummies <- function(dat, basal_level = FALSE, col = NULL, numerical = NULL, as.is = NULL) {
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
        if (name %in% as.is) {
            ## as.is option
            res <- as.matrix(tmp)
            colnames(res) <- name
        } else if (is.factor(tmp)) {
            ## factor or ordered
            if (name %in% numerical) {
                ## convert numeric by numerical option
                res <- as.matrix(as.numeric(tmp))
                colnames(res) <- name
            } else {
                ## convert dummy variables
                level <- levels(droplevels(tmp))
                m <- length(tmp)
                n <- length(level)
                res <- matrix(0, m, n)
                res[cbind(seq(m), tmp)] <- 1
                colnames(res) <- paste(name, level, sep = "_")
                ## delete basal level
                if (basal_level == FALSE) {
                    res <- res[,-1]
                }
            }
        } else {
            ## non-factor and non-ordered => as-is
            res <- as.matrix(tmp)
            colnames(res) <- name
        }
        result <- cbind(result, res)
    }
    rownames(result) <- name_row
    return(data.frame(result))
}
