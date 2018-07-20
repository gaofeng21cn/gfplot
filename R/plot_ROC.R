#' #' @export
#' #' @import ggplot2 cowplot precrec scales
#' plot_ROC <-  function(scores, labels, fontsize=16, palette = "jama")
#' {
#'   sscurves <- precrec::evalmod(scores = scores, labels = labels)
#'   roc <- precrec::auc(sscurves)[1, 4]
#'   if (precrec::auc(sscurves)$aucs[1] < 0.5) {
#'     sscurves <- precrec::evalmod(scores = -scores, labels = labels)
#'     roc <- precrec::auc(sscurves)[1, 4]
#'   }
#'   p <- autoplot(sscurves, curvetype = "ROC") + cowplot::theme_cowplot(font_size = fontsize, font_family = "Arial", line_size = 1) +
#'     theme(legend.position = "none") +
#'     annotate("text", x = 0.7, y = 0.1, label = paste0("AUC, ", format(round(roc, 3), nsmall =3)), size=fontsize/3)
#'   switch(palette,
#'          "jco"= {
#'            p + ggsci::scale_color_jco() +
#'              xlab("False Positive") + ylab("True Positive") +   scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent)
#'          },
#'          "lancet"= {
#'            p + ggsci::scale_color_lancet()
#'          },
#'          "jama"= {
#'            p + scale_color_manual(values = c("#164870", "#10B4F3", "#FAA935", "#2D292A", "#87AAB9", "#CAC27E", "#818282"))
#'          }, p + ggsci::scale_color_npg() )
#' }
#'
#' scores <- myeloma[, 7:10]
#' labels <- myeloma$event
#'
#' msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
#' mmcurves <- precrec::evalmod(msmdat1)
#'
#' inds <- subset(precrec::auc(mmcurves), curvetypes=="ROC")$auc < 0.5
#'
#' if(any(inds)) {
#'   scores[, inds] <- -scores[, inds]
#'   msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
#'   mmcurves <- precrec::evalmod(msmdat1)
#' }
#'
#' if(ncol(scores) > 1 && !is.null(ncol(scores))) {
#'   aucs <- t(sapply(scores, function(x) {
#'     roc2 <- pROC::roc(labels, x)
#'     auc <- pROC::ci(roc2)[c(2,1,3)]
#'   }))
#'   annot <- sprintf("%s, AUC %.3f (%.3f-%.3f)", colnames(scores), aucs[, 1], aucs[, 2], aucs[, 3])
#'
#'   autoplot(mmcurves, "ROC") + cowplot::theme_cowplot() +
#'     scale_color_manual(labels = annot, values = get_color(palette, length(annot))) +
#'     theme(legend.position = c(0.4, 0.2),
#'           legend.title = element_blank())
#' } else {
#'   roc2 <- pROC::roc(labels, scores)
#'   aucs <- pROC::ci(roc2)[c(2,1,3)]
#'   annot <- sprintf("AUC %.3f (%.3f-%.3f)", aucs[1], aucs[2], aucs[3])
#'
#'   autoplot(mmcurves, "ROC") + cowplot::theme_cowplot() +
#'     scale_color_manual(labels = annot, values = get_color(palette, length(annot))) +
#'     theme(legend.position = c(0.4, 0.2),
#'           legend.title = element_blank())
#' }


#' @export
#' @import ggplot2 cowplot precrec scales
plot_ROC <- function(scores, labels, palette="jama", legend.pos=c(0.4,0.2), title = NULL, font="Arial") {
  msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
  mmcurves <- precrec::evalmod(msmdat1)

  inds <- subset(precrec::auc(mmcurves), curvetypes=="ROC")$auc < 0.5

  if(any(inds)) {
    if(length(inds) == 1) {
      scores <- -scores
    } else {
      scores[, inds] <- -scores[, inds]
    }
    msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
    mmcurves <- precrec::evalmod(msmdat1)
  }

  if(ncol(scores) > 1 && !is.null(ncol(scores))) {
    aucs <- t(sapply(scores, function(x) {
      roc2 <- pROC::roc(labels, x)
      auc <- pROC::ci(roc2)[c(2,1,3)]
    }))
    annot <- sprintf("%s\nAUC %.3f (%.3f-%.3f)", colnames(scores), aucs[, 1], aucs[, 2], aucs[, 3])
  } else {
    roc2 <- pROC::roc(labels, scores)
    aucs <- pROC::ci(roc2)[c(2,1,3)]
    annot <- sprintf("AUC %.3f (%.3f-%.3f)", aucs[1], aucs[2], aucs[3])
  }

  p <- autoplot(mmcurves, "ROC") + cowplot::theme_cowplot(font_family = font) +
    scale_color_manual(labels = annot, values = get_color(palette, length(annot))) +
    theme(
      legend.position = legend.pos,
      legend.title = element_blank())  + labs(title = title)
  if(is.null(title)) p <- p + theme(legend.title = element_blank())

  switch(palette,
         "jco"= {
           p +
             xlab("False Positive") + ylab("True Positive") +   scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent)
         },  p)
}
