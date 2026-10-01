# Shared internal helpers.

# Points as a matrix with one point per row, as `ripr` stores them; a bare
# vector is a single point.
as_points <- function(x) {
  if (is.null(dim(x))) matrix(x, nrow = 1L) else as.matrix(x)
}

# Split a matrix of points (one per row) into a list of points; the JSON side
# wants points as arrays of coordinates.
rows <- function(m) {
  m <- as_points(m)
  lapply(seq_len(nrow(m)), function(i) as.vector(m[i, ]))
}

# The inverse of `rows()`; `NULL` for no points, which `rbind()` skips.
unrows <- function(l) {
  if (length(l) == 0L) return(NULL)
  do.call(rbind, lapply(l, as.numeric))
}

stop_unless <- function(ok, ...) {
  if (!isTRUE(ok)) stop(..., call. = FALSE)
}

need_ripr <- function() {
  stop_unless(
    requireNamespace("ripr", quietly = TRUE),
    "building a payload from ripr objects needs the ripr package; ",
    "a saved payload (see riprvis_payload()) draws without it"
  )
}

is_s7 <- function(x, class) {
  inherits(x, "S7_object") && S7::S7_inherits(x, class)
}

# The one serialisation the JS side is written against. `na = "null"` matters:
# the default writes NA as the string "NA", which reads as a valid number on
# the other side. Callers are responsible for I()-wrapping every vector that
# must stay an array when it has length one.
payload_json <- function(x) {
  as.character(
    jsonlite::toJSON(unclass(x), auto_unbox = TRUE, digits = 12, na = "null")
  )
}

row_lse <- function(x) {
  m <- apply(x, 1L, max)
  m + log(rowSums(exp(x - m)))
}

col_lse <- function(x) {
  m <- apply(x, 2L, max)
  m + log(colSums(exp(sweep(x, 2L, m))))
}
