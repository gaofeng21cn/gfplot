#' @export
#'
get_color <- function(palette, n = 6) {
  if(length(palette) > 1) return(palette)

  switch(tolower(palette), nature = {
    (ggsci::pal_npg("nrc"))(n)
  }, jco = {
    (ggsci::pal_jco("default"))(n)
  }, lancet = {
    (ggsci::pal_lancet("lanonc"))(n)
  }, jama = {
    ggsci::pal_jama()(n)
  }, jama_classic = {
     head(c("#164870", "#10B4F3", "#FAA935", "#2D292A", "#87AAB9", "#CAC27E", "#818282"), n)
  },
  RColorBrewer::brewer.pal(n, "Set1")
  )
}

#' @export
generate_time_event <- function(clinical, limits, labels = NULL) {
  time <- clinical[, 1]
  event <- clinical[, 2] == 1
  df <- sapply(limits, function(limit) {
    res <- event
    res[time > limit] <- F
    res
  })
  colnames(df) <- labels
  df
}


.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(cowplot::theme_cowplot())
}
