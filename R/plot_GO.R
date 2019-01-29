#' @export
#' @import ggplot2 cowplot dplyr
plot_GO <- function(gsea, n = 20) {
  gsea <- gsea %>% arrange(p.value)
  df.plot <- gsea %>% mutate(GeneRatio=overlap.size/query.size) %>%
    dplyr::select(Term=term.name, Count=overlap.size, GeneRatio, P=p.value) #%>% arrange((P))

  df.plot <- df.plot[!duplicated(df.plot$Term), ]
  df.plot$Term <-  factor(df.plot$Term, levels = df.plot$Term[order(df.plot$GeneRatio)])

  p <- ggplot(df.plot[1:min(n, nrow(df.plot)), ]) + geom_point(aes(x = GeneRatio, y = Term, size=Count, color=-log10(P))) + theme(axis.title.y = element_blank())
  p

}
