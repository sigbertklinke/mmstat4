######################### Code Source ##########################
#
# All Code and Comments Below are
# Copyright Marc Schwartz
# e-mail: marc_schwartz@me.com
# This code is made available under the GNU Public License V2.0
# This is free software and comes with ABSOLUTELY NO WARRANTY.

# R Code to calculate various Measures of
# Association for m x n tables
# References include:
# 1. Agresti A. (2002): Categorical Data Analysis, Second Edition, J. Wiley and Sons
# 2. Stokes M., Davis C. & Koch G. (1997): Categorical Data Analysis Using the SAS System, SAS Institute
# 3. Liebetrau A.M. (1983): Measures of Association (Sage University Papers Series on Quantitative Applications
#    in the Social Sciences, Series no. 07-032), Sage Publications
# 4. SAS Institute (1999): SAS/STAT User's Guide V8, SAS Institute
# 5. SPSS, Inc. (2003): SPSS 11.5 Statistical Algorithms
#    (http://www.spss.com/tech/stat/Algorithms/11.5/crosstabs.pdf)
# 6. Sheskin DJ. (2004): Handbook of Parametric and Nonparametric Statistical Procedures, Chapman & Hall/CRC

# MOST MEASURES TAKE A 2 DIMENSIONAL TABLE/MATRIX "x" AS AN ARGUMENT

# See the 'vcd' CRAN package for some examples and code
# on calculations and p values

#' @name association
#' @aliases concordant
#' @aliases discordant
#' @aliases ties.row
#' @aliases ties.col
#' @aliases nom.phi
#' @aliases nom.cc
#' @aliases nom.TT
#' @aliases nom.CV
#' @aliases nom.lamba
#' @aliases nom.uncertainty
#' @aliases ord.gamma
#' @aliases ord.tau
#' @aliases ord.somers.d
#'
#' @title Association measures
#' @description Various association coefficients for nominal and ordinal data; the input formats follows [stats::chisq.test()].
#' * `concordant` concordant pairs
#' * `discordant` discordant pairs
#' * `ties.row` pairs tied on rows
#' * `ties.col` pairs tied on columns
#' * `nom.phi` Phi Coefficient
#' * `nom.cc` Contingency Coefficient (Pearson's C) and Sakoda' s Adjusted Pearson's C
#' * `nom.TT` Tshuprow's T (not meaningful for non-square tables)
#' * `nom.CV` Cramer's V (for 2 x 2 tables V = Phi)
#' * `nom.lambda` Goodman and Kruskal's Lambda with
#' \itemize{
#'   \item `lambda.cr` The row variable is used as independent, the column variable as dependent variable.
#'   \item `lambda.rc` The column variable is used as independent, the row variable as dependent variable.
#'   \item `lambda.symmetric` Symmetric Lambda (the mean of both above).
#' }
#' * `nom.uncertainty` Uncertainty Coefficient (Theil's U) with
#' \itemize{
#'   \item `ucc.cr` The row variable is used as independent, the column variable as dependent variable.
#'   \item `uc.rc` The column variable is used as independent, the row variable as dependent variable.
#'   \item `uc.symmetric` Symmetric uncertainty coefficient.
#' }
#' * `ord.gamma` Gamma coefficient
#' * `ord.tau` a vector with Kendall-Stuart Tau's
#' \itemize{
#'   \item `tau.a` Tau-a (for quadratic tables only)
#'   \item `tau.b` Tau-b
#'   \item `tau.c` Tau-c
#' }
#' * `ord.somers.d` Somers' d
#' * `eta` Eta coefficient for nominal/interval data
#'
#' @param x a numeric vector, table or matrix. `x` and `y` can also both be factors. \cr
#' For `eta` the independent nominal variable (factor or numeric).
#' @param y a numeric vector; ignored if `x` is a table or matrix.
#' If `x` is a factor, `y` should be a factor of the same length. \cr
#' For `eta` the dependent interval variable (numeric).
#' @param breaks either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2)
#' giving the number of intervals into which `x` is to be cut (only for `eta`).
#'
#' @source From the [archived `ryouready` package](https://cran.r-project.org/package=ryouready) by Mark Heckmann.
#' The code for the calculation of `nom.lambda`, `nom.uncertainty`, `ord.gamma`, `ord.tau`, `ord.somers.d`
#' was supplied by Marc Schwartz (under GPL 2) and checked against SPSS results.
#' @return the association coefficient(s)
#' @importFrom stats chisq.test complete.cases pchisq
#' @export
#'
#' @examples
#' ## Nominal data
#' # remove gender from the table
#' hec <- apply(HairEyeColor, 1:2, sum)
#' nom.phi(hec)
#' nom.cc(hec)
#' nom.TT(hec)
#' nom.CV(hec)
#' nom.lambda(hec)
#' nom.uncertainty(hec)
#' ## Ordinal data
#' # create a fake data set
#' ordx <- sample(5, size=100, replace=TRUE)
#' ordy <- sample(5, size=100, replace=TRUE)
#' concordant(ordx, ordy)
#' discordant(ordx, ordy)
#' ties.row(ordx, ordy)
#' ties.col(ordx, ordy)
#' ord.gamma(ordx, ordy)
#' ord.tau(ordx, ordy)
#' ord.somers.d(ordx, ordy)
#' ## Interval/nominal data
#' eta(iris$Species, iris$Sepal.Length)
#'

#' @rdname association
#' @export
concordant <- function(x, y=NULL)
{
  x <- get.table(x, y)

  # get sum(matrix values > r AND > c)
  # for each matrix[r, c]
  mat.lr <- function(r, c)
  {
    lr <- x[(r.x > r) & (c.x > c)]
    sum(lr)
  }

  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)

  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.lr, r = r.x, c = c.x))
}

#' @rdname association
#' @export
discordant <-function(x, y=NULL)
{
  x <- get.table(x, y)

  # get sum(matrix values > r AND < c)
  # for each matrix[r, c]
  mat.ll <- function(r, c)
  {
    ll <- x[(r.x > r) & (c.x < c)]
    sum(ll)
  }

  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)

  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.ll, r = r.x, c = c.x))
}

#' @rdname association
#' @export
ties.row <- function(x, y=NULL)
{
  x <- get.table(x, y)

  total.pairs <- 0

  rows <- dim(x)[1]
  cols <- dim(x)[2]

  for (r in 1:rows)
  {
    for (c in 1:(cols - 1))
    {
      total.pairs <- total.pairs + (x[r, c] * sum(x[r, (c + 1):cols]))
    }
  }

  total.pairs
}

#' @rdname association
#' @export
ties.col <- function(x, y=NULL)
{
  x <- get.table(x, y)

  total.pairs <- 0

  rows <- dim(x)[1]
  cols <- dim(x)[2]

  for (c in 1:cols)
  {
    for (r in 1:(rows - 1))
    {
      total.pairs <- total.pairs + (x[r, c] * sum(x[(r + 1):rows, c]))
    }
  }

  total.pairs
}

# Returns a valid table according chisq.test
get.table <- function(x, y, ord=FALSE) {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x) && (min(dim(x)) == 1L)) x <- as.vector(x)
  if (length(dim(x)) > 2L) stop("invalid 'x'")
  if (length(x) == 1L) stop("'x' must at least have 2 elements")
  if (is.matrix(x) && (length(dim(x))>2)) stop("Only 2d matrices or tables allowed")
  if (!is.matrix(x) && !is.null(y)) {
    stopifnot("'x' and 'y' must have the same length"=(length(x)==length(y)))
    OK <- complete.cases(x, y)
    if (ord) {
      x <- factor(x[OK])
      y <- factor(y[OK])
    } else {
      x <- ordered(x[OK])
      y <- ordered(y[OK])
    }
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
      stop("'x' and 'y' must have at least 2 levels")
    x <- table(x, y)
  }
  if (any(x < 0) || anyNA(x)) stop("all entries of 'x' must be nonnegative and finite")
  if (sum(x) == 0) stop("at least one entry of 'x' must be positive")
  x
}

#' @rdname association
#' @export
nom.phi <- function(x, y=NULL)
{
  x <- get.table(x, y)
  suppressWarnings(phi <- sqrt(chisq.test(x, correct = FALSE)$statistic / sum(x)))
  c("phi"=as.numeric(phi))
}

#' @rdname association
#' @export
nom.cc <- function(x, y=NULL)
{
  x <- get.table(x, y)

  # CC - Pearson's C
  chisq <- chisq.test(x, correct = FALSE)$statistic
  C <- sqrt(chisq / (chisq + sum(x)))

  # Sakoda's adjusted Pearson's C
  k <- min(dim(x))
  SC <- C / sqrt((k - 1) / k)
  #
  c("Pearson.C"=as.numeric(C), "Sakoda.C"=as.numeric(SC))
}

#' @rdname association
#' @export
nom.TT <- function(x, y=NULL)
{
  x <- get.table(x, y)

  TT <- sqrt(chisq.test(x, correct = FALSE)$statistic /
       (sum(x) * sqrt((dim(x)[1] - 1) * (dim(x)[2] - 1))))

  c("Tshuprow.T"=as.numeric(TT))
}

#' @rdname association
#' @export
nom.CV <- function(x, y=NULL)
{
  x <- get.table(x, y)

  suppressWarnings(CV <- sqrt(chisq.test(x, correct = FALSE)$statistic /
       (sum(x) * min(dim(x) - 1))))

  c("Cramer.V"=as.numeric(CV))
}

#' @rdname association
#' @export
nom.lambda <-  function(x, y=NULL)
{
  x <- get.table(x, y)

  SumRmax <- sum(apply(x, 1, max))
  SumCmax <- sum(apply(x, 2, max))
  MaxCSum <- max(colSums(x))
  MaxRSum <- max(rowSums(x))
  n <- sum(x)

  L.CR <- (SumRmax - MaxCSum) / (n - MaxCSum)
  L.RC <- (SumCmax - max(rowSums(x))) / (n - MaxRSum)
  L.S <- (SumRmax + SumCmax - MaxCSum - MaxRSum) /
        ((2 * n) - MaxCSum - MaxRSum)
  c("lambda.cr"=L.CR, "lambda.rc"=L.RC, "lambda.symmetric"=L.S)
}

#' @rdname association
#' @export
nom.uncertainty <- function(x, y=NULL)
{
  x <- get.table(x, y)
  SumR <- rowSums(x)
  SumC <- colSums(x)
  n <- sum(x)

  HY <- -sum((SumC / n) * log(SumC / n))
  HX <- -sum((SumR / n) * log(SumR / n))
  HXY <- -sum((x / n) * log(x / n))

  UC.RC <- (HX + HY - HXY) / HX
  UC.CR <- (HY + HX - HXY) / HY
  UC.S <- 2 * (HX + HY - HXY) / (HX + HY)

  c("uc.rc"=UC.RC, "uc.cr"=UC.CR, "uc.symmetric"=UC.S)
}

#' @rdname association
#' @export
ord.gamma <- function(x, y=NULL)
{
  x <- get.table(x, y, TRUE)

  c <- concordant(x)
  d <- discordant(x)

  c("gamma"=as.numeric((c - d) / (c + d)))
}

tau.a <-function(x, y=NULL)
{
  x <- get.table(x, y, TRUE)

  c <- concordant(x)
  d <- discordant(x)
  n <- sum(x)
  # check if table is quadratic otherwise issue warning ?

  c("tau.a"=(c - d) / (n*(n-1) / 2))
}

tau.b <- function(x, y=NULL)
{
  x <- get.table(x, y, TRUE)

  c <- concordant(x)
  d <- discordant(x)

  # An alternative computation is:
  #Tr <- ties.row(x)
  #Tc <- ties.col(x)
  #KTb <- (c - d) / sqrt((c + d + Tc) * (c + d + Tr))

  # The "preferred" computation is:
  n <- sum(x)
  SumR <- rowSums(x)
  SumC <- colSums(x)

  tau.b <- (2 * (c - d)) / sqrt(((n ^ 2) - (sum(SumR ^ 2))) * ((n ^ 2) - (sum(SumC ^ 2))))
  c("tau.b"=tau.b)
}

tau.c <- function(x, y=NULL)
{
  x <- get.table(x, y, TRUE)

  c <- concordant(x)
  d <- discordant(x)
  m <- min(dim(x))
  n <- sum(x)

  c("tau.c"=(m * 2 * (c - d)) / ((n ^ 2) * (m - 1)))
}

#' @rdname association
#' @export
ord.tau <- function(x, y=NULL)
{
  c(tau.a(x, y), tau.b(x, y), tau.c(x, y))
}

#' @rdname association
#' @export
ord.somers.d <-  function(x, y=NULL)
{
  x <- get.table(x, y, TRUE)

  c <- concordant(x)
  d <- discordant(x)
  n <- sum(x)
  SumR <- rowSums(x)
  SumC <- colSums(x)

  sd.cr <- (2 * (c - d)) / ((n ^ 2) - (sum(SumR ^ 2)))
  sd.rc <- (2 * (c - d)) / ((n ^ 2) - (sum(SumC ^ 2)))
  sd.s <- (2 * (c - d)) / ((n ^ 2) - (((sum(SumR ^ 2)) + (sum(SumC ^ 2))) / 2))

  c("sd.cr"=sd.cr, "sd.rc"=sd.rc, "sd.symmetric"=sd.s)
}

#### Cochran's Q ####

# Test for proportions in dependent samples
# a k > 2 generalization of the mcnemar test
# 'mat' is a matrix, where:
# each row is a subject
# each column is the 0/1 result of a test condition
cochranq.test <- function(mat)
{
  k <- ncol(mat)

  C <- sum(colSums(mat) ^ 2)
  R <- sum(rowSums(mat) ^ 2)
  T <- sum(rowSums(mat))

  num <- (k - 1) * ((k * C) - (T ^ 2))
  den <- (k * T) - R

  Q <- num / den

  df <- k - 1
  names(df) <- "df"
  names(Q) <- "Cochran's Q"

  p.val <- pchisq(Q, df, lower.tail = FALSE)

  QVAL <- list(statistic = Q, parameter = df, p.value = p.val,
               method = "Cochran's Q Test for Dependent Samples",
               data.name = deparse(substitute(mat)))
  class(QVAL) <- "htest"
  return(QVAL)
}

#' @rdname association
#' @export
eta <- function(x, y, breaks=NULL)
{
  if (is.factor(x))                           # convert factor to numeric in case it is a factor
    x <- as.numeric(x)
  if (! (is.vector(x) & is.vector(y)) )
      stop("'x' must be vectors or a factor, 'y' must be a vector", call.=FALSE)
  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  if (!is.null(breaks))                       # if x is interval data, breaks can be specified
    x <- droplevels(cut(x, breaks=breaks))    # as order does not matter drop levels
  l <- split(y, x)
  c("eta"=.eta(l))
}

# x: list with y values for each category
# code adapted from: http://stackoverflow.com/questions/3002638/eta-eta-squared-routines-in-r
#
.eta <- function(x)
{
  y <- unlist(x)                    # list with values by category
  mg <- sapply(x, mean)             # group means
  ng <- sapply(x, length)           # group size
  mtot <- mean(y)                   # total mean
  ssb <- sum(ng * (mg - mtot)^2)    # SSb
  sst <- sum((y - mtot)^2)          # SSt
  sqrt(ssb/sst)
}


