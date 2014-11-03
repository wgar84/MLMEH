MLMem <- function (node, tree, data, df = NULL, what = 'local')
  {
    subtree <- extract.clade (tree, node)
    if (is.null(df))
      df <- ldply(data[subtree $ tip.label], function (L) L [[what]])
    else
      df <- subset (df, .id %in% subtree $ tip.label)
    formula <-
      paste('cbind(', paste (colnames (df) [-1], collapse = ','), ') ~ .id - 1',
            sep = '')
    model <- lm (as.formula (formula), data = df)

    B <- var (coef (model))
    W <- CalculateMatrix (model)

    list ('B' = B, 'W' = W, 'means' = coef (model), 'subtree' = subtree)
  }
