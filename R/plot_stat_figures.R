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
