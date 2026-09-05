#' Convert Continuous Measurements to Binary Using an Acceptable Range
#'
#' Converts continuous measurements to binary indicators based on whether
#' each value falls inside or outside an acceptable operating range. An
#' observation is coded 1 if it falls below the lower bound or above the
#' upper bound of the range, and 0 if it falls inside the range. This is
#' the practical, real-data preprocessing rule used in dissertation Section
#' 6.4.1 (Brisbane River water quality application), generalized here so the
#' range bounds can come from either source a practitioner is likely to have.
#'
#' Known specification or standard limits: supply \code{lower_limit} and
#' \code{upper_limit} directly. Use this when the acceptable range for a
#' variable is already established, for example a regulatory or engineering
#' specification, or values a domain expert already knows for their process.
#'
#' Estimated from historical (Phase I) data: supply \code{reference} instead,
#' along with \code{lower_percentile} and \code{upper_percentile} (default
#' 0.01 and 0.99, the 1st and 99th percentile). The bounds are then computed
#' as the empirical percentiles of \code{reference}. This is the approach
#' used in dissertation Section 6.4.1, where the acceptable range for each
#' water quality variable was not independently known and was instead
#' estimated from a Phase I reference period.
#'
#' This is a separate function from \code{dichotomize_data()}, which
#' generates and thresholds data from an assumed theoretical distribution for
#' the simulation studies. The two are not interchangeable. This function
#' never assumes a distribution, and \code{dichotomize_data()} is unchanged
#' and still governs the simulation-study workflow.
#'
#' @param data Numeric vector of continuous measurements to convert.
#' @param lower_limit Known lower bound of the acceptable range. Provide
#'   together with \code{upper_limit}. Do not combine with \code{reference}.
#' @param upper_limit Known upper bound of the acceptable range. Provide
#'   together with \code{lower_limit}. Do not combine with \code{reference}.
#' @param reference Numeric vector of historical or Phase I reference data,
#'   used to estimate the range bounds when they are not already known. Do
#'   not combine with \code{lower_limit}/\code{upper_limit}.
#' @param lower_percentile Empirical lower percentile used to estimate the
#'   range from \code{reference} (default 0.01). Used only when
#'   \code{reference} is supplied.
#' @param upper_percentile Empirical upper percentile used to estimate the
#'   range from \code{reference} (default 0.99). Used only when
#'   \code{reference} is supplied.
#' @return Integer vector of binary indicators (0 or 1), the same length as
#'   \code{data}. A value of 1 means the observation fell outside the
#'   acceptable range.
#' @export
#'
#' @examples
#' # Known specification limits (e.g. a regulatory or engineering standard)
#' measurements <- c(6.1, 7.0, 7.8, 9.0, 5.5)
#' dichotomize_range(measurements, lower_limit = 6.5, upper_limit = 8.5)
#'
#' # Bounds estimated from historical data (dissertation Section 6.4.1 approach)
#' historical <- rnorm(1000, mean = 7, sd = 0.5)
#' new_readings <- c(6.8, 7.1, 9.2)
#' dichotomize_range(new_readings, reference = historical)
dichotomize_range <- function(data,
                               lower_limit = NULL,
                               upper_limit = NULL,
                               reference = NULL,
                               lower_percentile = 0.01,
                               upper_percentile = 0.99) {

  has_limits <- !is.null(lower_limit) || !is.null(upper_limit)
  has_reference <- !is.null(reference)

  if (has_limits && has_reference) {
    stop("Supply either `lower_limit`/`upper_limit` or `reference`, not both.")
  }
  if (!has_limits && !has_reference) {
    stop("Supply either `lower_limit` and `upper_limit`, or `reference`.")
  }

  if (has_limits) {
    if (is.null(lower_limit) || is.null(upper_limit)) {
      stop("Both `lower_limit` and `upper_limit` are required when specifying a known range.")
    }
    if (lower_limit >= upper_limit) {
      stop("`lower_limit` must be less than `upper_limit`.")
    }
    lower_bound <- lower_limit
    upper_bound <- upper_limit
  } else {
    if (lower_percentile < 0 || upper_percentile > 1 || lower_percentile >= upper_percentile) {
      stop("`lower_percentile` and `upper_percentile` must satisfy 0 <= lower_percentile < upper_percentile <= 1.")
    }
    lower_bound <- stats::quantile(reference, probs = lower_percentile, na.rm = TRUE, names = FALSE)
    upper_bound <- stats::quantile(reference, probs = upper_percentile, na.rm = TRUE, names = FALSE)
  }

  as.integer(data < lower_bound | data > upper_bound)
}

#' Check Pairwise Correlation Among Streams for the Independence Assumption
#'
#' The CSB-EWMA control chart assumes that all monitored streams are
#' independent (dissertation Section 6.2.7 and Limitations discussion). This
#' function reports the pairwise Pearson correlation among a set of candidate
#' streams and flags any pair whose absolute correlation exceeds a threshold,
#' so the user can decide which streams to keep before calling
#' \code{run_csb_ewma()} or \code{csb_ewma()}. It does not remove or adjust
#' any stream automatically. No validated method for correcting correlated
#' streams currently exists for this chart; see the companion paper's
#' Discussion section, which lists correlated-stream handling as future work.
#'
#' @param data A numeric matrix or data frame with candidate streams as
#'   columns.
#' @param threshold Absolute Pearson correlation above which a pair is
#'   flagged. Default is 0.3, the threshold used in dissertation Section
#'   6.2.7 to select independent streams for the Brisbane River application
#'   and restated generally in the Limitations discussion. This is a
#'   practical default carried over from that application, not a universally
#'   derived optimal value; adjust it if your own application calls for a
#'   different threshold.
#' @param use Passed to \code{stats::cor()}. Default \code{"pairwise.complete.obs"}.
#' @return A list with three elements: \code{correlation_matrix} (the full
#'   pairwise Pearson correlation matrix), \code{flagged_pairs} (a data frame
#'   of stream pairs with \code{abs(r) > threshold}, sorted by descending
#'   absolute correlation, zero rows if none), and \code{independence_ok}
#'   (\code{TRUE} if no pair exceeds the threshold).
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(600), ncol = 6)
#' colnames(x) <- paste0("stream_", 1:6)
#' check_stream_correlation(x)
check_stream_correlation <- function(data, threshold = 0.3, use = "pairwise.complete.obs") {

  if (!(is.matrix(data) || is.data.frame(data))) {
    stop("`data` must be a numeric matrix or data frame with streams as columns.")
  }
  if (threshold <= 0 || threshold > 1) {
    stop("`threshold` must be between 0 and 1.")
  }

  data <- as.matrix(data)
  stream_names <- colnames(data)
  if (is.null(stream_names)) {
    stream_names <- paste0("stream_", seq_len(ncol(data)))
    colnames(data) <- stream_names
  }

  correlation_matrix <- stats::cor(data, use = use, method = "pearson")

  k <- ncol(correlation_matrix)
  pairs <- utils::combn(k, 2)

  flagged <- data.frame(
    stream_1 = character(0),
    stream_2 = character(0),
    r = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(ncol(pairs))) {
    a <- pairs[1, i]
    b <- pairs[2, i]
    r_ab <- correlation_matrix[a, b]
    if (!is.na(r_ab) && abs(r_ab) > threshold) {
      flagged <- rbind(flagged, data.frame(
        stream_1 = stream_names[a],
        stream_2 = stream_names[b],
        r = r_ab,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(flagged) > 0) {
    flagged <- flagged[order(-abs(flagged$r)), ]
    rownames(flagged) <- NULL
  }

  list(
    correlation_matrix = correlation_matrix,
    flagged_pairs = flagged,
    independence_ok = nrow(flagged) == 0
  )
}

