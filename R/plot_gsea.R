#' Updated viewGSEA for HTSanalyzeR2
#' @export
viewGSEA <- function(object, gscName, gsName, title="") {
  gseaScores <- getFromNamespace("gseaScores", "DOSE")

  df <- gseaScores(object@geneList, object@listOfGeneSetCollections[[gscName]][[gsName]], fortify=TRUE)

  df$ymin=0
  df$ymax=0
  pos <- df$position == 1
  h <- diff(range(df$runningScore))/20
  df$ymin[pos] <- -h
  df$ymax[pos] <- h
  df$geneList <- object@geneList

  gsdata <- df

  color="#DAB546"
  color.line='firebrick'
  color.vline="steelblue"

  by = "all"

  # code from enrichplot
  p <- ggplot(gsdata, aes_(x = ~x)) +
    DOSE::theme_dose() + xlab("Position in the Ranked List of Genes")
  # if (by == "runningScore" || by == "all") {
  p.res <- p + geom_linerange(aes_(ymin=~ymin, ymax=~ymax), color=color)
  p.res <- p.res + geom_line(aes_(y = ~runningScore), color=color.line, size=1)
  rr <- object@result$GSEA.results[[gscName]]
  enrichmentScore <- rr[rr$Gene.Set.Term == gsName, "Observed.score"]
  es.df <- data.frame(es = which.min(abs(p$data$runningScore - enrichmentScore)))
  p.res <- p.res + geom_vline(data = es.df, aes_(xintercept = ~es),
                              colour = color.vline, linetype = "dashed")
  p.res <- p.res + ylab("Enrichment Score")
  p.res <- p.res + geom_hline(yintercept = 0)

  fancy_scientific <- function(l, dig = 3) {
    l <- format(l, digits = dig, scientific = TRUE)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("e", "%*%10^", l)
    parse(text = l)
  }

  p.value <-  rr[rr$Gene.Set.Term == gsName, "Adjusted.Pvalue"]

  p.res <- p.res + annotate("text",  x = Inf, y = Inf,
                            label = paste("ES:", signif(enrichmentScore, 3)),
                            hjust = 1.5, vjust = 2) +
    annotate("text",  x = Inf, y = Inf,
             label = ifelse(p.value == 0,
                            "italic(P)<1%*%10^{-22}",
                            paste0("italic(P)==", fancy_scientific(p.value, 3))),
             hjust = 1, vjust = 3, parse = TRUE)
  # }
  # if (by == "preranked" || by == "all") {
  #   df2 <- data.frame(x = which(p$data$position == 1))
  #   df2$y <- p$data$geneList[df2$x]
  #   p.pos <- p + geom_segment(data=df2, aes_(x=~x, xend=~x, y=~y, yend=0), color=color)
  #   p.pos <- p.pos + ylab("Ranked list metric") + xlim(0, length(p$data$geneList))
  # }
  # if (by == "runningScore")
  #   return(p.res + ggtitle(title))
  # if (by == "preranked")
  #   return(p.pos + ggtitle(title))
  #
  # p.pos <- p.pos + xlab(NULL) + theme(axis.text.x = element_blank(),
  #                                     axis.ticks.x = element_blank())
  # p.pos <- p.pos + ggtitle(title) +
  #   theme(plot.title=element_text(hjust=0.5, size=rel(2)))
  # plot_grid(p.res, p.pos,ncol=1, align="v", rel_heights = c(3,1))

  return(p.res + ggtitle(title))

}

#' GSEA view using fgsea
#' @export
ggGSEA <- function(input, gs, title="") {
  gseaScores <- getFromNamespace("gseaScores", "DOSE")

  df <- gseaScores(input,  gs, fortify=TRUE)
  enrichmentScore <- gseaScores(input,  gs)$ES

  set.seed(100)
  fgseaRes <- fgsea::fgsea(pathways = list(group=gs),
                           stats = input,
                           nperm=10000)
  p.value <- fgseaRes$pval

  df$ymin=0
  df$ymax=0
  pos <- df$position == 1
  h <- diff(range(df$runningScore))/20
  df$ymin[pos] <- -h
  df$ymax[pos] <- h
  df$geneList <- input

  gsdata <- df

  color="#DAB546"
  color.line='firebrick'
  color.vline="steelblue"

  by = "all"

  # code from enrichplot
  p <- ggplot(gsdata, aes_(x = ~x)) +
    DOSE::theme_dose() + xlab("Position")
  p.res <- p + geom_linerange(aes_(ymin=~ymin, ymax=~ymax), color=color)
  p.res <- p.res + geom_line(aes_(y = ~runningScore), color=color.line, size=1)

  es.df <- data.frame(es = which.min(abs(p$data$runningScore - enrichmentScore)))
  p.res <- p.res + geom_vline(data = es.df, aes_(xintercept = ~es),
                              colour = color.vline, linetype = "dashed")
  p.res <- p.res + ylab("Enrichment Score")
  p.res <- p.res + geom_hline(yintercept = 0)

  fancy_scientific <- function(l, dig = 3) {
    l <- format(l, digits = dig, scientific = TRUE)
    l <- gsub("^(.*)e", "'\\1'e", l)
    l <- gsub("e", "%*%10^", l)
    parse(text = l)
  }


  if(enrichmentScore > 0) {
    p.res <- p.res + annotate("text",  x = Inf, y = Inf,
                              label = paste("ES:", signif(enrichmentScore, 3)),
                              hjust = 1.5, vjust = 2) +
      annotate("text",  x = Inf, y = Inf,
               label = ifelse(p.value == 0,
                              "italic(P)<1%*%10^{-4}",
                              paste0("italic(P)==", fancy_scientific(p.value, 3))),
               hjust = 1, vjust = 3, parse = TRUE)
  } else {
    p.res <- p.res + annotate("text",  x = 0, y = enrichmentScore,
                              label = paste("ES:", signif(enrichmentScore, 3)),
                              hjust = 0, vjust = -1) +
      annotate("text",  x = 0, y = enrichmentScore,
               label = ifelse(p.value == 0,
                              "italic(P)<1%*%10^{-4}",
                              paste0("italic(P)==", fancy_scientific(p.value, 3))),
               hjust = 0, vjust = -2, parse = TRUE)
  }

  return(p.res + ggtitle(title))

}
