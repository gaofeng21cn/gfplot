#' @export
#' @import ggplot2 cowplot ggradar
#'
plot_immune <- function(res, group, title="", legend.position = "left") {
  df <- data.frame(res, check.names = F)
  rownames(df) <- df[, 1]
  df <- df[, -1]
  df <- data.frame(t(df), check.names = F)
  df$RiskGroup <- group[rownames(df)]

  immune_radar <- df[!is.na(df$RiskGroup), ] %>%
    group_by(RiskGroup) %>%
    summarise_all(list(mean))

  immune_radar <- data.frame(immune_radar, check.names = F)
  colnames(immune_radar)[8] <- "T cell CD4+\n(non-regulatory)"
  colnames(immune_radar)[10] <- "T cell regulatory\n(Tregs)"

  grid.max <- max(data.matrix(immune_radar[, 2:11])) * 1.2
  grid.min <- median(data.matrix(immune_radar[, 2:11]))

  ggradar::ggradar(immune_radar,
          grid.min = 0, font.radar = "Arial",
          grid.mid = grid.min, grid.max = grid.max, plot.extent.x.sf = 1, plot.extent.y.sf = 1.2,
          grid.line.width = 0.2, grid.label.size = 0, axis.label.size = 4, axis.label.offset = 1,
          group.line.width = 0.2, group.point.size = 1,background.circle.transparency = 0,
          plot.legend = if (nrow(immune_radar) >1) TRUE else FALSE,
          plot.title = title) +
    #scale_color_manual(values = c("#377EB8", "#E41A1C")) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "left", legend.text=element_text(size=8),
          legend.key.size=unit(0.5,"cm"),legend.key.width=unit(0.5,"cm"),legend.key.height=unit(0.5,"cm"))


}
