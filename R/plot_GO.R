#' @export
#' @import ggplot2 cowplot dplyr
plot_GO <- function (gsea, n = 20)
{
  gsea <- gsea %>% arrange(p_value)
  df.plot <- gsea %>% mutate(GeneRatio = intersection_size/query_size) %>%
    dplyr::select(Term = term_name, Count = intersection_size,
                  GeneRatio, P = p_value)
  df.plot <- df.plot[!duplicated(df.plot$Term), ]
  df.plot$Term <- factor(df.plot$Term, levels = df.plot$Term[order(df.plot$GeneRatio)])
  p <- ggplot(df.plot[1:min(n, nrow(df.plot)), ]) + cowplot::theme_cowplot(font_family = "Arial") +
    geom_point(aes(x = GeneRatio,
                   y = Term, size = Count, color = -log10(P))) +
    theme(axis.title.y = element_blank()) + labs(x="Gene Ratio")
  p
}
