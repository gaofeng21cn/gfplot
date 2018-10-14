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

#' @export
#' @import ggfortify
plot_PCA <- function(data, labs, title="Evaluate the batch effect between groups", palette = "nature") {
  library(ggfortify)

  df <- data.frame(group=labs, data, check.names = T)

  autoplot(prcomp(df[, -1]), data=df, colour="group") +
    theme(legend.title=element_blank()) +
    scale_fill_manual(labels = levels(factor(labs)), values = get_color(palette, length(levels(factor(labs))))) +
    ggtitle(title)
}


#' @export
#' @import ggplot2 cowplot
plot_RiskScore <- function(rs, event, legend.position = c(0.2, 0.8), palette = "nature", color=NULL) {
  if(is.logical(event)) event <- factor(event, levels = c(T, F), labels = c("Dead/Recurrence", "Disease free"))

  if(is.null(names(rs))) names(rs) <- 1:length(rs)
  df <- data.frame(pt=names(rs), rs=rs, event=event)
  df <- df %>% arrange(rs)
  df$pt <- factor(df$pt, levels = as.character(df$pt))

  if (is.null(color)) {
    color <- get_color(palette, length(levels(event)))
  }


  p <- ggplot(df, aes(pt, rs, fill=event)) + geom_bar(stat="identity", alpha=0.7) +
    cowplot::theme_cowplot(font_family = "Arial") +
    ylab("Risk score") +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_blank(),
          legend.position = legend.position, legend.key.width = unit(1, "cm")) +
    scale_fill_manual(labels = levels(event), values = color)
  p

}

#' @export
#' @import ggplot2 cowplot
plot_Boxplot <- function(value, label, palette = "nature") {
  p <- qplot(x= label, y= value, geom= "boxplot", color= label) +
    cowplot::theme_cowplot(font_family = "Arial") +
    theme(legend.position = "none", axis.title = element_blank()) +
    scale_color_manual(labels = label, values = get_color(palette, length(levels(label))))
  p
}
