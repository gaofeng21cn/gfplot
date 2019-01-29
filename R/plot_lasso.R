#' @export
#' @import ggplot2 cowplot MASS glmnet reshape ggrepel
plot_lasso <- function(fit, s) {
  beta=coef(fit)

  tmp <- as.data.frame(as.matrix(beta))

  obj <- (coef(fit, s =s))
  ind <- obj@i +1
  sig.genes <- obj@Dimnames[[1]][ind]
  df_text <- data.frame(x=-5, y=tmp[, ncol(tmp)][ind], text=sig.genes)

  tmp$coef <- row.names(tmp)
  tmp <- reshape::melt(tmp, id = "coef")
  tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
  tmp$lambda <- fit$lambda[tmp$variable+1] # extract the lambda values
  tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm


  tmp <- tmp[tmp$coef != "(Intercept)" & tmp$lambda >= s, ]
  tmp$label <- NA
  tmp$label[tmp$lambda==min(tmp$lambda) & tmp$coef %in% sig.genes] <- tmp$coef[tmp$lambda==min(tmp$lambda) & tmp$coef %in% sig.genes]

  ggplot(tmp, aes(log10(lambda), value, color = coef, label=label)) +
    geom_line() +
    xlab("Lambda (log scale)") + ylab("Coefficients") +
    guides(color = guide_legend(title = ""),
           linetype = guide_legend(title = "")) +
    theme_bw() +
    theme(legend.key.width = unit(3,"lines"),
          legend.position = "none") + scale_x_reverse(limits = c(log10(max(tmp$lambda)), log10(min(tmp$lambda))- 0.1 * (log10(max(tmp$lambda)) - log10(min(tmp$lambda)))  )) +
    #geom_vline(aes(xintercept=log10(s)), linetype=2, color="grey50") +
    geom_text_repel(
      data          = tmp[!is.na(tmp$label), ],
      nudge_x       = 1,
      segment.size  = 0.2,
      segment.color = "grey50",
      direction     = "y",
      hjust         = 1
    )
}
