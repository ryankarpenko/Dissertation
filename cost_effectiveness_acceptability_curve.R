```{r ceacplane, fig.height=3.8, fig.width=3.8, fig.pos = 'p', fig.align='center', fig.cap="\\label{ceacplot}CEAC Plot. Reference arm: ********. Red line: Bivariate Normal model. Green line: Beta-Gamma model. Blue line: Hurdle model.", message=FALSE, warning=FALSE, echo=FALSE}
wtp <- seq(0,50000,500)
  p.h <- rep(NA, 101); p.b <- rep(NA, 101); p.n <- rep(NA, 101)
  for(k in 1:length(wtp)){
    p.h[k] <- sum(mh$BUGSoutput$sims.list$delta.c < (wtp[k] * mh$BUGSoutput$sims.list$delta.q)) / length(mh$BUGSoutput$sims.list$delta.q)
    p.b[k] <- sum(mb$BUGSoutput$sims.list$delta.c < (wtp[k] * mb$BUGSoutput$sims.list$delta.q)) / length(mb$BUGSoutput$sims.list$delta.q)
    p.n[k] <- sum(mn$BUGSoutput$sims.list$delta.c < (wtp[k] * mn$BUGSoutput$sims.list$delta.q)) / length(mn$BUGSoutput$sims.list$delta.q)
  }
  ceac.data <- data.frame(k=wtp, h = p.h, b = p.b, n = p.n)
  ggplot2::ggplot(data=ceac.data) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(color="grey90"),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color="grey90"),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color="black", fill=NA)) +
    ggplot2::geom_smooth(mapping=ggplot2::aes(x=k,y=n), color="firebrick2", size=1.5) +
    ggplot2::geom_smooth(mapping=ggplot2::aes(x=k,y=b), color="palegreen3", size=1.5) +
    ggplot2::geom_smooth(mapping=ggplot2::aes(x=k,y=h), color="royalblue3", size=1.5) +
    #ggplot2::scale_x_continuous(limits=c(0,50000), expand = ggplot2::expand_scale(add=1000)) +
    #ggplot2::scale_y_continuous(limits=c(0,1), expand = ggplot2::expand_scale(add=0.02)) +
    ggplot2::lims(x=c(0,50000), y=c(0,1)) +
    ggplot2::labs(x="Willingness-to-Pay (£)", y="Probability of Cost-Effectiveness", title="Cost-Effectiveness Acceptability Curve")
  
```
