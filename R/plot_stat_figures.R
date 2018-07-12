plot_barplot <- function() {

  library(scales)
  my_trans <- function(from=0)
  {
    trans <- function(x) x-from
    inv <- function(x) x+from
    trans_new("myscale", trans, inv,
              domain = c(from, Inf))
  }

  library(ggplot2)
  library(ggpubr)


  ggplot(df.plot, aes(labs, rs, fill = labs)) +
    stat_summary(geom = "bar", fun.y = mean, position = "dodge", width=0.5) +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width=0.3) +
    stat_compare_means(comparisons = my_comparisons,  label.y = c(0.75,1,1.25)+1) +
    stat_compare_means(label.y = 2.75) +
    scale_y_continuous(expand = c(0,0), limits = c(-1,2), trans = my_trans(from=-1)) + scale_fill_manual(values = c("#E69E00","#0070B0","#CA78A6", "#009C73")) +
    theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank()) + labs(y="m6A risk score")

  barplot <- ggplot(df.plot, aes(labs, rs, fill = labs)) +
    stat_summary(geom = "bar", fun.y = mean, position = "dodge", width=0.5) +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width=0.3) +
    stat_compare_means(comparisons = my_comparisons,  label.y = c(0.75,1,1.25)+0.8, label = "p.signif") +
    # stat_compare_means(label.y = 2.75) +
    scale_y_continuous(expand = c(0,0), limits = c(-1,1.25), trans = my_trans(from=-1)) + scale_fill_manual(values = c("#E69E00","#0070B0","#CA78A6", "#009C73")) +
    theme(legend.title = element_blank(), legend.position = "none", axis.title.x = element_blank()) + labs(y="m6A risk score")
}



#' Plot correlation
#'
#' This function plots correlation between two variables.
#'
#' @return a ggplot2 object of the plot
#' @import ggplot2 cowplot
#' @export
#' @examples
#'
#'  library(cowplot)
#'  plot_cor(iris$Sepal.Length, iris$Sepal.Width)

plot_cor <- function(x, y, groups=NULL, xlab=NULL, ylab=NULL, legend.pos = "top") {

  pval <- paste(sprintf("Correlation = %.3f\nP", Hmisc::rcorr(x, y)$r[2,1]),
             ifelse(Hmisc::rcorr(x, y)$P[2,1]==0, "< 1e-22",  paste0("= ", signif(Hmisc::rcorr(x, y)$P[2,1], 3)) ))

  df.plot <- data.frame(a=x, b=y)

  p <-   ggplot(df.plot, aes(x=a, y=b)) +
    geom_smooth(method='lm',formula=y~x, se=F, linetype = "dashed", colour="grey50") +
    theme(legend.title = element_blank(), legend.position = legend.pos) +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, label = pval) + labs(x=xlab, y=ylab)

  if(!is.null(groups)) {
    p <- p + geom_point(aes(color=groups), alpha=0.5)
    if(length(unique(groups)) == 3) { p + scale_color_manual(values = c("#377EB8", "grey50", "#E41A1C")) }
    if(length(unique(groups)) == 2) { p + scale_color_manual(values = c("#377EB8", "#E41A1C")) }
    if(length(unique(groups)) > 3) { p + scale_color_brewer("Set1") }
  } else {
    p <- p + geom_point(alpha=0.5)
  }
  p
}


