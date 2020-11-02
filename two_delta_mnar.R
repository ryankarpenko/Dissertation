```{r twodelta,  fig.height=8.25, fig.width=2.7, fig.pos = 'p', fig.align='center', fig.cap="\\label{twodelta}Sensitivity analysis with separate tipping-point delta values for each arm. Light-colored dots represent positive INB. Dark-colored dots represent negative INB. Numerical labels represent the percentage of positive INB values in the corresponding square. Reference arm: ********", message=FALSE, warning=FALSE, echo=FALSE}
#BVN
set.seed(333)
temp.delta1.n <- runif(5000, -1 * data$twoSD, 0)
temp.delta2.n <- runif(5000, -1 * data$twoSD, 0)
temp.mu.q.1.n <- mn$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta1.n * data$missPct[1])
temp.mu.q.2.n <- mn$BUGSoutput$sims.matrix[,"mu.q[2]"] + (temp.delta2.n * data$missPct[2])
temp.delta.q.n <- temp.mu.q.2.n - temp.mu.q.1.n
temp.inb.n <- (30000 * temp.delta.q.n) - mn$BUGSoutput$sims.list$delta.c
labels.n <- rep(NA, 9)
intervals.n <- c(-0.4, -0.3, -0.2, -0.1, 0)
for(r in 1:4){
  for(c in 1:4){
    labels.n[4*(r-1) + c] <- round(sum(temp.inb.n > 0 & (temp.delta1.n > intervals.n[c] & temp.delta1.n <= intervals.n[c+1]) & (temp.delta2.n < intervals.n[6-r] & temp.delta2.n >= intervals.n[5-r])) /
                                   sum((temp.delta1.n > intervals.n[c] & temp.delta1.n < intervals.n[c+1]) & (temp.delta2.n < intervals.n[6-r] & temp.delta2.n >= intervals.n[5-r])), 3)
  }
}

bvnDeltas <- ggplot2::ggplot()+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", linetype=0),
                                  panel.grid.major = ggplot2::element_line(color="grey66"),
                                  axis.title = ggplot2::element_text(size=10),
                                  legend.position = "none") +
  #ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1, y = temp.delta2, color = as.factor(ifelse(temp.inb>0,1,0))), size=3, alpha=0.7) +
  ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1.n[temp.inb.n>0], y = temp.delta2.n[temp.inb.n>0], color = as.factor(1)), size=0.5, alpha=0.7) +
  ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1.n[temp.inb.n<=0], y = temp.delta2.n[temp.inb.n<=0], color = as.factor(0)), size=0.5, alpha=0.7) +
  ggplot2::scale_color_manual(values=c("navy","skyblue2"), labels=c("INB < 0","INB > 0")) +
  ggplot2::geom_label(mapping=ggplot2::aes(x = rep(c(-0.35,-0.25,-0.15,-0.05),4), y=c(rep(-0.05,4),rep(-0.15,4),rep(-0.25,4),rep(-0.35,4)),
                                           label=labels.n), size=2.5) +
  ggplot2::labs(x="Delta Open Arm", y="Delta EVAR Arm", color="", title="Bivariate Normal")

#BG
set.seed(222)
temp.delta1.b <- runif(5000, -1 * data$twoSD, 0)
temp.delta2.b <- runif(5000, -1 * data$twoSD, 0)
temp.mu.q.1.b <- mb$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta1.b * data$missPct[1])
temp.mu.q.2.b <- mb$BUGSoutput$sims.matrix[,"mu.q[2]"] + (temp.delta2.b * data$missPct[2])
temp.delta.q.b <- temp.mu.q.2.b - temp.mu.q.1.b
temp.inb.b <- (30000 * temp.delta.q.b) - mb$BUGSoutput$sims.list$delta.c
labels.b <- rep(NA, 9)
intervals.b <- c(-0.4, -0.3, -0.2, -0.1, 0)
for(r in 1:4){
  for(c in 1:4){
    labels.b[4*(r-1) + c] <- round(sum(temp.inb.b > 0 & (temp.delta1.b > intervals.b[c] & temp.delta1.b <= intervals.b[c+1]) & (temp.delta2.b < intervals.b[6-r] & temp.delta2.b >= intervals.b[5-r])) /
                                   sum((temp.delta1.b > intervals.b[c] & temp.delta1.b < intervals.b[c+1]) & (temp.delta2.b < intervals.b[6-r] & temp.delta2.b >= intervals.b[5-r])), 3)
  }
}

bgDeltas <- ggplot2::ggplot()+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", linetype=0),
                                  panel.grid.major = ggplot2::element_line(color="grey66"),
                                  axis.title = ggplot2::element_text(size=10),
                                  legend.position = "none") +
  #ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1, y = temp.delta2, color = as.factor(ifelse(temp.inb>0,1,0))), size=3, alpha=0.7) +
  ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1.b[temp.inb.b>0], y = temp.delta2.b[temp.inb.b>0], color = as.factor(1)), size=0.5, alpha=0.7) +
  ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1.b[temp.inb.b<=0], y = temp.delta2.b[temp.inb.b<=0], color = as.factor(0)), size=0.5, alpha=0.7) +
  ggplot2::scale_color_manual(values=c("navy","skyblue2"), labels=c("INB < 0","INB > 0")) +
  ggplot2::geom_label(mapping=ggplot2::aes(x = rep(c(-0.35,-0.25,-0.15,-0.05),4), y=c(rep(-0.05,4),rep(-0.15,4),rep(-0.25,4),rep(-0.35,4)),
                                           label=labels.b), size=2.5) +
  ggplot2::labs(x="Delta Open Arm", y="Delta EVAR Arm", color="", title="Beta-Gamma")

set.seed(111)
temp.delta1.h <- runif(5000, -1 * data$twoSD, 0)
temp.delta2.h <- runif(5000, -1 * data$twoSD, 0)
temp.mu.q.1.h <- mh$BUGSoutput$sims.matrix[,"mu.q[1]"] + (temp.delta1.h * data$missPct[1])
temp.mu.q.2.h <- mh$BUGSoutput$sims.matrix[,"mu.q[2]"] + (temp.delta2.h * data$missPct[2])
temp.delta.q.h <- temp.mu.q.2.h - temp.mu.q.1.h
temp.inb.h <- (30000 * temp.delta.q.h) - mh$BUGSoutput$sims.list$delta.c
labels.h <- rep(NA, 9)
intervals.h <- c(-0.4, -0.3, -0.2, -0.1, 0)
for(r in 1:4){
  for(c in 1:4){
    labels.h[4*(r-1) + c] <- round(sum(temp.inb.h > 0 & (temp.delta1.h > intervals.h[c] & temp.delta1.h <= intervals.h[c+1]) & (temp.delta2.h < intervals.h[6-r] & temp.delta2.h >= intervals.h[5-r])) /
                                   sum((temp.delta1.h > intervals.h[c] & temp.delta1.h < intervals.h[c+1]) & (temp.delta2.h < intervals.h[6-r] & temp.delta2.h >= intervals.h[5-r])), 3)
  }
}

hurdDeltas <- ggplot2::ggplot()+ ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", linetype=0),
                                  panel.grid.major = ggplot2::element_line(color="grey66"),
                                  axis.title = ggplot2::element_text(size=10),
                                  legend.position = "none") +
  #ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1, y = temp.delta2, color = as.factor(ifelse(temp.inb>0,1,0))), size=3, alpha=0.7) +
  ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1.h[temp.inb.h>0], y = temp.delta2.h[temp.inb.h>0], color = as.factor(1)), size=0.5, alpha=0.7) +
  ggplot2::geom_point(mapping=ggplot2::aes(x = temp.delta1.h[temp.inb.h<=0], y = temp.delta2.h[temp.inb.h<=0], color = as.factor(0)), size=0.5, alpha=0.7) +
  ggplot2::scale_color_manual(values=c("navy","skyblue2"), labels=c("INB < 0","INB > 0")) +
  ggplot2::geom_label(mapping=ggplot2::aes(x = rep(c(-0.35,-0.25,-0.15,-0.05),4), y=c(rep(-0.05,4),rep(-0.15,4),rep(-0.25,4),rep(-0.35,4)),
                                           label=labels.h), size=2.5) +
  ggplot2::labs(x="Delta Open Arm", y="Delta EVAR Arm", color="", title="Hurdle")

gridExtra::grid.arrange(bvnDeltas, bgDeltas, hurdDeltas, ncol=1)
```
