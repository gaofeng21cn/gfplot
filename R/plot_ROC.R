#' Plot ROC curve
#'
#' This function plots ROC curve for survival analysis.
#' @export
#' @return a ggplot2 object of the plot
#' @import ggplot2 scales
#' @examples
#' library(gfplot)
#' library(survminer)
#' data(myeloma)
#' extrafont::loadfonts()
#' plot_ROC(myeloma$CCND1, myeloma$event)
#' plot_ROC(myeloma[, 7:10], myeloma$event)
#'
plot_ROC <- function(scores, labels, force05 = T, palette="jama", legend.pos=c(0.4,0.2), title = NULL, font="Arial", percent.style=F) {
  msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
  mmcurves <- precrec::evalmod(msmdat1)

  inds <- subset(precrec::auc(mmcurves), curvetypes=="ROC")$auc < 0.5
  if(force05 == F) inds <- F

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

  if(percent.style) p <- p +
    xlab("False Positive") + ylab("True Positive") +
    scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent)

  p
}
