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
