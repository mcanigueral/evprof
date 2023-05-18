
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.evprof <- list(
    evprof.start.hour = 0,
    evprof.tzone = "Europe/Amsterdam"
  )
  toset <- !(names(op.evprof) %in% names(op))
  if(any(toset)) options(op.evprof[toset])
  invisible()
}
