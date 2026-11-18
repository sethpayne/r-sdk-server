# Package utilities loaded first (aaa sorts early).

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0L && !is.logical(a)) return(b)
  a
}

.ld_pkg_env <- new.env(parent = emptyenv())

LD_KIND_FLAG <- "flag"
LD_KIND_SEGMENT <- "segment"
