#' Plot Kaplan-Meier Curve
#'
#' This function plots Kaplan-Meier Curve for survival analysis.
#'
#' @param clinical a survival object created by Surv function in survival package
#' @param labels a vector containing subtyping labels of patients
#' @param limit a numeric indicating the time limit of this KM plot
#' @param annot a character indicating the annotation showed up in the plot
#' @param color a vector containing colors used for different subtypes
#' @param font a character indicating the font used in the plot (default: "Arial")
#' @param xlab a character indicating the label of x-axis (default: "Follow up")
#' @param ylab a character indicating the label of y-axis (default: "Survival Probability")
#' @param title a character indicating the title (default: NULL)
#' @param lenged.pos a character indicating whether the legend is (default: "top")
#' @param palette color paletter (default: "jama_classic")
#' @param risk.table a logical indicating whether show risk table or not (default: TRUE)
#' @param risk.table.ratio a numeric indicating the relative size of risk table (default: 0.4)
#' @param anno.pos position of annotation (default: "bottom")
#' @param anno.x.shift relative position of annotation in x-axis (default: 0.5)
#' @return a ggplot2 object of the plot
#' @import ggplot2 cowplot
#' @export
#' @examples
#'
#'  library(survival)
#'  library(survminer)
#'  extrafont::loadfonts()
#'  data(myeloma)
#'  clin <- Surv(myeloma$time, myeloma$event)
#'  labs <- factor(myeloma$molecular_group)
#'  plot_KMCurve(clin, labs)

plot_KMCurve <- function (clinical, labels, limit = NULL, annot = NULL, color = NULL,
                          font = "Arial", xlab = "Follow up", ylab = "Survival Probability",
                          title = NULL, legend.pos = "top", palette = "jama_classic",
                          risk.table = T, risk.table.ratio = 0.4, anno.pos = "bottom",
                          anno.x.shift = 0.5)
{
  time <- clinical[, 1]
  event <- clinical[, 2] == 1
  if (!is.null(limit)) {
    event[time > limit] <- F
    time[time > limit] <- limit
  }
  df <- data.frame(futime = time, fustat = event, group = labels)
  surv <- survival::survfit(survival::Surv(futime, fustat) ~
                              group, data = df)
  survstats <- survival::survdiff(survival::Surv(futime, fustat) ~
                                    group, data = df)
  survstats$p.value <- 1 - pchisq(survstats$chisq, length(survstats$n) -
                                    1)
  if (!is.null(color)) {
    if (!is.null(names(color))) {
      labels <- factor(labels, levels = names(color))
    }
  }
  else {
    color <- get_color(palette, n = length(unique(labels)))
  }
  if (class(labels) == "factor") {
    legend.labs <- na.omit(levels(droplevels(labels[!(is.na(time) |
                                                        is.na(event))])))
  }
  else if (class(labels) == "logical") {
    labels <- factor(labels, levels = c(F, T))
    legend.labs <- na.omit(levels(droplevels(labels)))
  }
  else {
    legend.labs <- na.omit(unique(labels))
    labels <- factor(labels, levels = legend.labs)
  }
  fancy_scientific <- function(l, dig = 3) {
    l <- format(l, digits = dig, scientific = TRUE)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("e", "%*%10^", l)
    parse(text = l)
  }
  p <- survminer::ggsurvplot(surv, data = df, xlab = xlab,
                             ylab = ylab, palette = color, legend = legend.pos, legend.labs = legend.labs,
                             risk.table = risk.table, risk.table.title = element_blank(),
                             risk.table.y.text = FALSE, ggtheme =cowplot::theme_cowplot())
  p$plot <- p$plot + ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = font),
          title = element_text(family = font),
          axis.text.x = element_text(family = font),
          legend.title = element_blank())
  anno.text <- ifelse(survstats$p.value == 0, "italic(P)<1%*%10^{-22}",
                      paste0("italic(P)==", fancy_scientific(survstats$p.value,
                                                             3)))
  anno.y.shift <- 0
  if (length(legend.labs) == 2) {
    hr <- survcomp::hazard.ratio(labels[!(is.na(time) | is.na(event))],
                                 time[!(is.na(time) | is.na(event))], event[!(is.na(time) |
                                                                                is.na(event))])
    anno.text <- c(anno.text, sprintf("HR == %3.2f~(%3.2f - %3.2f)",
                                      hr$hazard.ratio, hr$lower, hr$upper))
    anno.y.shift <- c(anno.y.shift + 0.15, 0)
  }
  if (!is.null(annot)) {
    anno.text <- c(anno.text, annot)
    anno.y.shift <- c(anno.y.shift + 0.15, 0)
  }
  if (anno.pos == "bottom") {
    p$plot <- p$plot + annotate("text", family = font, x = 0,
                                y = anno.y.shift, label = anno.text, hjust = 0, vjust = 0,
                                parse = TRUE)
  }
  else {
    p$plot <- p$plot + annotate("text", family = font, x = anno.x.shift *
                                  max(time, na.rm = T), y = 0.85 + anno.y.shift, label = anno.text,
                                hjust = 0, vjust = 2, parse = TRUE)
  }
  if (risk.table) {
    p$table <- p$table  +  theme(text = element_text(family = font),
                                 title = element_text(family = font), axis.text = element_text(family = font),
                                 axis.title.y = element_blank())
    pp <- plot_grid(plotlist = list(p$plot + theme(axis.title.x = element_blank()),
                                    p$table + labs(x = xlab)), labels = "", ncol = 1,
                    align = "v", rel_heights = c(1, risk.table.ratio))
    return(pp)
  }
  else return(p$plot)
}
