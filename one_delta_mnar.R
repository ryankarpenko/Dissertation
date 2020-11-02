```{r dinbplot, fig.height=8.25, fig.width=2.7, fig.pos= 'p', fig.align='center', fig.cap="\\label{dinbplot}Sensitivity analysis for the shared-delta method of tipping-point MNAR in each model. Each dot represents one simulation in the model. Points above the red line have positive INB and favor the intervention (*****). Points below the red line have negative INB and favor the control arm (********).", message=FALSE, warning=FALSE, echo=FALSE}
temp.delta.h <- runif(5000, -1 * data$twoSD, 0)
temp.mu.q.1.h <- mh$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta.h * data$missPct[1])
temp.mu.q.2.h <- mh$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta.h * data$missPct[1])
temp.delta.q.h <- temp.mu.q.2.h - temp.mu.q.1.h
temp.inb.h <- (30000 * temp.delta.q.h) - mh$BUGSoutput$sims.list$delta.c

temp.delta.b <- runif(5000, -1 * data$twoSD, 0)
temp.mu.q.1.b <- mb$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta.b * data$missPct[1])
temp.mu.q.2.b <- mb$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta.b * data$missPct[1])
temp.delta.q.b <- temp.mu.q.2.b - temp.mu.q.1.b
temp.inb.b <- (30000 * temp.delta.q.b) - mb$BUGSoutput$sims.list$delta.c

temp.delta.n <- runif(5000, -1 * data$twoSD, 0)
temp.mu.q.1.n <- mn$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta.n * data$missPct[1])
temp.mu.q.2.n <- mn$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta.n * data$missPct[1])
temp.delta.q.n <- temp.mu.q.2.n - temp.mu.q.1.n
temp.inb.n <- (30000 * temp.delta.q.n) - mn$BUGSoutput$sims.list$delta.c

inb_data <- data.frame(bvn.delta = temp.delta.n, bvn.inb = temp.inb.n,
                       bg.delta = temp.delta.b, bg.inb = temp.inb.b,
                       h.delta = temp.delta.h, h.inb = temp.inb.h)
dinb.n <- ggplot2::ggplot(data=inb_data) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_line(color="grey90"),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color="grey90"),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color="black", fill=NA)) +
  ggplot2::geom_point(mapping=ggplot2::aes(x=bvn.delta, y=bvn.inb), col="black", alpha=0.7, size=0.35)+
  ggplot2::ylim(-3200, 8600) + 
  ggplot2::geom_hline(yintercept = 0, color="red", size=1) +
  ggplot2::labs(title="Bivariate Normal", x="Delta", y="Incremental Net Benefit")
dinb.b <- ggplot2::ggplot(data=inb_data) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_line(color="grey90"),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color="grey90"),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color="black", fill=NA)) +
  ggplot2::geom_point(mapping=ggplot2::aes(x=bg.delta, y=bg.inb), col="black", alpha=0.7, size=0.35)+
  ggplot2::geom_hline(yintercept = 0, color="red", size=1) +
  ggplot2::labs(title="Beta-Gamma", x="Delta", y="Incremental Net Benefit") +
  ggplot2::ylim(-3200, 8600)
dinb.h <- ggplot2::ggplot(data=inb_data) +
  ggplot2::theme(panel.background = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_line(color="grey90"),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color="grey90"),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.border = ggplot2::element_rect(color="black", fill=NA)) +
  ggplot2::geom_point(mapping=ggplot2::aes(x=h.delta, y=h.inb), col="black", alpha=0.7, size=0.35)+
  ggplot2::geom_hline(yintercept = 0, color="red", size=1) +
  ggplot2::labs(title="Hurdle", x="Delta", y="Incremental Net Benefit") +
  ggplot2::ylim(-3200, 8600)
gridExtra::grid.arrange(dinb.n, dinb.b, dinb.h, ncol=1)
```
