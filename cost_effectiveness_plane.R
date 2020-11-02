```{r ceplane, fig.height=3.9, fig.width=3.9, fig.pos = 'p', fig.align='center', fig.cap="\\label{ceplot}Cost-Effectiveness Plane. Reference arm: **********. Red points: Bivariate Normal model. Green points: Beta-Gamma model. Blue points: Hurdle model. Dashed line: Willingness-to-pay of \\pounds30,000", message=FALSE, warning=FALSE, echo=FALSE}
deltas <- data.frame(bvn.delta.q = mn$BUGSoutput$sims.matrix[,"delta.q"], bvn.delta.c = mh$BUGSoutput$sims.matrix[,"delta.c"],
                       bg.delta.q = mb$BUGSoutput$sims.matrix[,"delta.q"], bg.delta.c = mb$BUGSoutput$sims.matrix[,"delta.c"],
                       h.delta.q = mh$BUGSoutput$sims.matrix[,"delta.q"], h.delta.c = mh$BUGSoutput$sims.matrix[,"delta.c"])
ce_plane <- ggplot2::ggplot(data= deltas) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill="white", linetype=0),
                         panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         panel.grid.minor.y = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(color="black", fill=NA)) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = bvn.delta.q, y = bvn.delta.c), col="firebrick2", alpha=0.7, size=0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = bg.delta.q, y = bg.delta.c), col="palegreen3", alpha=0.7, size=0.5) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = h.delta.q, y = h.delta.c), col="royalblue3", alpha=0.7, size=0.5) +
    ggplot2::xlim(-0.2, 0.2) + ggplot2::ylim(-10000,10000) +
    ggplot2::geom_hline(yintercept=0) +
    ggplot2::geom_vline(xintercept=0) +
    ggplot2::geom_abline(slope=30000, intercept=0, linetype="dashed", color="gray60") +
    ggplot2::labs(title="Cost-Effectiveness Plane", x="Incremental QALYs", y="Incremental Costs (£)")
ce_plane
```
