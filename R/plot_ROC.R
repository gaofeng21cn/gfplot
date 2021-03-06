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
plot_ROC <- function (scores, labels, force05 = F, palette = "jama",
                      legend.pos = c(0.2, 0.15), title = NULL, font = "Arial", percent.style = F)
{
  msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
  mmcurves <- precrec::evalmod(msmdat1)
  inds <- subset(precrec::auc(mmcurves), curvetypes == "ROC")$auc <
    0.5
  if (force05 == F)
    inds <- F
  if (any(inds)) {
    if (length(inds) == 1) {
      scores <- -scores
    }
    else {
      scores[, inds] <- -scores[, inds]
    }
    msmdat1 <- precrec::mmdata(scores, labels, modnames = colnames(scores))
    mmcurves <- precrec::evalmod(msmdat1)
  }
  if (ncol(scores) > 1 && !is.null(ncol(scores))) {
    aucs <- t(sapply(scores, function(x) {
      roc2 <- pROC::roc(labels, x)
      auc <- pROC::ci(roc2)[c(2, 1, 3)]
    }))
    annot <- sprintf("%s\nAUC %.2f (%.2f-%.2f)", colnames(scores),
                     aucs[, 1], aucs[, 2], aucs[, 3])
  }
  else {
    roc2 <- pROC::roc(labels, scores)
    aucs <- pROC::ci(roc2)[c(2, 1, 3)]
    annot <- sprintf("AUC %.2f (%.2f-%.2f)", aucs[1], aucs[2],
                     aucs[3])
  }
  p <- autoplot(mmcurves, "ROC") + cowplot::theme_cowplot(font_family = font) +
    scale_color_manual(labels = annot, values = get_color(palette,
                                                          length(annot))) +
    theme(legend.position = legend.pos, legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) + labs(title = title)
  if (is.null(title))
    p <- p + theme(legend.title = element_blank())
  if (percent.style)
    p <- p + xlab("False Positive") + ylab("True Positive") +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent)
  p
}

#' @export
#'
plot_TimeROC <- function(scores, survival, time_points, groups,
                         palette="jama", legend.pos=c(0.4,0.15), title = NULL, font="Arial", percent.style=F) {

  df.plot <- do.call(rbind, lapply(1:length(time_points), function(i) {
    p <- survivalROC(Stime = survival[, 1], status = survival[, 2], marker = scores, predict.time = time_points[i], method="KM")
    df <- data.frame(FP=p$FP, TP=p$TP, group=groups[i])
    df
  }))

  aucs <- do.call(c, lapply(1:length(time_points), function(i) {
    p <- survivalROC(Stime = survival[, 1], status = survival[, 2], marker = scores, predict.time = time_points[i], method="KM")
    p$AUC
  }))

  annot <- paste(groups, "AUC", sprintf("%.3f", aucs))

  p <- ggplot() + geom_line(data=df.plot, aes(FP, TP, color=group)) +
    labs(x="False Positive", y="True Positivie", title=title) + coord_equal() +
    cowplot::theme_cowplot(font_family = "Arial") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = legend.pos,
          legend.title = element_blank()) +
    scale_color_manual(labels = annot, values = get_color(palette, length(annot))) +
    geom_abline(intercept=0, slope = 1, color="grey50", linetype="dashed")

  if(percent.style) p <- p +
    scale_y_continuous(labels=percent) + scale_x_continuous(labels=percent)

  p
}

#' @export
#'
plot_MulROC <- function (scores, labels,  palette = "jama_classic", color=NULL,
                         legend.pos = c(0.4, 0.15), title = NULL, font = "Arial",
                         percent.style = F)
{
  df.plot <- do.call(rbind, lapply(1:length(scores), function(i) {
    index <- !is.na(scores[[i]])
    roc2 <- pROC::roc(labels[[i]][index], scores[[i]][index])
    df <- data.frame(FP = 1-roc2$specificities, TP = roc2$sensitivities, group = names(scores)[i])
    df
  }))

  aucs <- t(sapply(1:length(scores), function(i) {
    index <- !is.na(scores[[i]])
    roc2 <- pROC::roc(labels[[i]][index], scores[[i]][index])
    auc <- pROC::ci(roc2)[c(2, 1, 3)]
  }))
  annot <- paste0(stringr::str_pad(names(scores), max(sapply(names(scores), nchar))+1, "right"), "\t", sprintf("AUC %.2f", aucs[, 1]))

  if(!is.null(color)) {
    color_value <- color
  } else {
    color_value <- get_color(palette, length(annot))
  }


  p <- ggplot() + geom_path(data = df.plot, aes(FP, TP, color = group)) +
    labs(x = "1 - Specificity", y = "Sensitivity", title = title) +
    coord_equal() + cowplot::theme_cowplot(font_family = "Arial") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = legend.pos, legend.title = element_blank()) +
    scale_color_manual(labels = annot, values = color_value) + geom_abline(intercept = 0, slope = 1,
                                                                           color = "grey50", linetype = "dashed")
  if (percent.style)
    p <- p +
    scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) +
    labs(x = "False Positive", y = "True Positivie", title = title)
  p
}
