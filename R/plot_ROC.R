#' @export
#' @import ggplot2 cowplot precrec scales
plot_ROC <-  function(scores, labels, fontsize=18, palette = "nature")
{
  sscurves <- precrec::evalmod(scores = scores, labels = labels)
  roc <- precrec::auc(sscurves)[1, 4]
  if (auc(sscurves)$aucs[1] < 0.5) {
    sscurves <- precrec::evalmod(scores = -scores, labels = labels)
    roc <- precrec::auc(sscurves)[1, 4]
  }
  p <- autoplot(sscurves, curvetype = "ROC") + cowplot::theme_cowplot(font_size = fontsize, font_family = "Arial", line_size = 1) +
    theme(legend.position = "none") +
    annotate("text", x = 0.7, y = 0.1, label = paste0("AUC, ", format(round(roc, 3), nsmall =3)), size=fontsize/3)
  switch(palette,
         "jco"= {
           p + ggsci::scale_color_jco() +
             xlab("False Positive") + ylab("True Positive") +   scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent)
         },
         "lancet"= {
           p + ggsci::scale_color_lancet()
         },
         "jama"= {
           p + scale_color_manual(values = c("#164870", "#10B4F3", "#FAA935", "#2D292A", "#87AAB9", "#CAC27E", "#818282"))
         }, p + ggsci::scale_color_npg() )
}

#' @export
#' @import ggplot2 cowplot precrec
plot_multiROC <- function(scores, groups, fontsize=16, palette="nature") {
  msmdat1 <- precrec::mmdata(scores , groups, modnames = colnames(scores))
  mmcurves <- precrec::evalmod(msmdat1)

  inds <- subset(precrec::auc(mmcurves), curvetypes=="ROC")$auc < 0.5

  if(any(inds)) {
    scores[, inds] <- -scores[, inds]
    msmdat1 <- precrec::mmdata(scores , groups, modnames = colnames(scores))
    mmcurves <- precrec::evalmod(msmdat1)
  }

  p <- autoplot(mmcurves, "ROC")+ cowplot::theme_cowplot(font_size = fontsize, font_family = "Arial", line_size = 1)  +
    theme(legend.position = c(0.8, 0.2),
          legend.title = element_blank())
  switch(palette,
         "jco"= {
           p + ggsci::scale_color_jco() +
             xlab("False Positive") + ylab("True Positive") +   scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent)
         },
         "lancet"= {
           p + ggsci::scale_color_lancet()
         },
         "jama"= {
           p + scale_color_manual(values = c("#164870", "#10B4F3", "#FAA935", "#2D292A", "#87AAB9", "#CAC27E", "#818282"))
         }, p + ggsci::scale_color_npg() )
}
