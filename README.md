# cumkm function in R
make cumulative curve for two event at the same time

![img](https://github.com/yellowbridge/cumkm/blob/master/2018-06-27_150625.png)

Example:
```R
library(survival)
library(ggplot2)
library(grid)
data("colon")
sfit1 <- survfit(Surv(time,status)~rx, data=colon)
sfit2 <- survfit(Surv(time,surg)~rx, data=colon)

cumkm(
  sfit1=sfit1,
  sfit2=sfit2,
  legend.position = c(0.15, 0.8),
  event.level=c('Death','Surgery'),
  left.right=c('Control group','Interv group'),
  main = 'Cumulative incidence of Death and Surgery',
  xlab = 'Time from randomization',
  ylab = 'Cumulative incidence (%)'
)
```
