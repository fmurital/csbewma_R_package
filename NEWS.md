# csbewma 1.1.0

## New features

* Added `dichotomize_range()`: converts continuous measurements to a binary
  indicator based on whether each value falls inside or outside an
  acceptable operating range, for practical real-data use as described in
  dissertation Section 6.4.1. The range can be supplied directly as known
  `lower_limit`/`upper_limit` values, or estimated from historical
  `reference` data using percentiles (default 1st/99th).
* Added `dichotomize_range_multi()`: applies `dichotomize_range()`
  separately to each variable in a dataset, since different variables
  monitored together typically have different acceptable ranges (for
  example, different variables in the same process rarely share one
  tolerance band). Every variable's range must be specified explicitly;
  there is no shared default across variables.
* Added `check_stream_correlation()`: reports the pairwise Pearson
  correlation among candidate streams and flags any pair whose absolute
  correlation exceeds a threshold (default 0.3, per dissertation Section
  6.2.7), as a diagnostic check of the independence assumption underlying
  the CSB-EWMA chart. This function is diagnostic only; it does not remove
  or adjust correlated streams, since no validated correction method for
  correlated streams currently exists for this chart.

## Bug fixes

* Fixed a label placement bug in `plot.csb_ewma()` and
  `plot_csb_ewma_direct()`: the "Signal at t = ..." annotation used
  `hjust = 0` instead of `hjust = 1`, which misaligned the label against
  the signal line.

## Documentation

* Added `URL` and `BugReports` fields to `DESCRIPTION`, pointing to the
  package's development repository.
* Added `@seealso` cross-references between `run_csb_ewma()` and
  `csb_ewma()` clarifying that they are not duplicates: `run_csb_ewma()`
  is the lower-level monitoring engine, useful directly when a variance
  cache should be precomputed once and reused across many chart runs (for
  example, in simulation studies); `csb_ewma()` is the higher-level
  convenience wrapper that dichotomizes continuous data, precomputes
  variance, calls `run_csb_ewma()`, and performs post-hoc identification
  automatically.
* Added unit tests (`testthat`) covering every exported function.

# csbewma 1.0.1

* First version accepted to CRAN.

# csbewma 1.0.0

* Initial development version.
