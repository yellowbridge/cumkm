#author:bridge 2018-06-27
#version:1.0
cumkm <- function(sfit1,
                  sfit2,
                  event.level ,
                  legend.position = c(0.15, 0.85),
                  left.right,
                  main = 'Cumulative incidence of Death and Surgery',
                  xlab = 'Time from randomization',
                  ylab = 'Cumulative incidence (%)')
{
  require(survival)
  if (require(grid)) {
    library(grid)
  } else{
    install.packages(grid)
    library(grid)
  }
  
  level <- levels(factor(summary(sfit1, censored = T)$strata))
  if(any(is.na(left.right))){
    left.right <- level
  }
  #prepare data 
  df1 <- data.frame(
    surv = 1 - sfit1$surv,
    time = sfit1$time,
    strata = factor(summary(sfit1, censored = T)$strata),
    event = event.level[1]
  )
  df2 <- data.frame(
    surv = 1 - sfit2$surv,
    time = sfit1$time,
    strata = factor(summary(sfit2, censored = T)$strata),
    event = event.level[2]
  )
  
  df.left <- rbind(df1[df1$strata == level[1], ],
                   df2[df2$strata == level[1], ])[, -3]
  
  df.right <- rbind(df1[df1$strata == level[2], ],
                    df2[df2$strata == level[2], ])[, -3]
  
  #检查两组是否有同样的随访时间
  max.time.g1 = max(df.left$time)
  max.time.g2 = max(df.right$time)
  max.time <- max(max.time.g1, max.time.g2)
  
  # if(max.time.g1<max.time.g2){
  #   tmp <- df.left[df.left$time==max.time.g1,]
  #   tmp$time <- max.time.g2
  #   df.left <- rbind(df.left,tmp)
  # }else{
  #   tmp <- df.right[df.right$time==max.time.g2,]
  #   tmp$time <- max.time.g2
  #   df.right <- rbind(df.right,tmp)
  # }
  
  colnames(df.left)[1:3] <- c('surv.left', 'time.left', 'event.left')
  colnames(df.right)[1:3] <-
    c('surv.right', 'time.right', 'event.right')
  
  df.right$time.right <- 2 * max.time - df.right$time.right
  df.plot <-
    merge(data.frame(df.left, row.names = NULL),data.frame(df.right, row.names = NULL),by = 0,all = TRUE)[-1]
  df.plot$event.left <-factor(df.plot$event.left, levels = event.level)
  df.plot$event.right <-factor(df.plot$event.right, levels = event.level)
  
  
  #ggplot part
  ##change x axis and tick labels
  timeby <- 5
  xaxis.break <- seq(0, 2 * max.time, max.time / timeby)
  xaxis.label <- as.character(c(xaxis.break[1:(timeby + 1)],
                                rev(xaxis.break[1:(timeby)])))
  ##construct basic plot
  p <- ggplot() + theme_bw() +
    theme(
      text = element_text(family = 'serif'),
      axis.title.x = element_text(family = 'serif', vjust = 0.7),
      axis.title.y = element_text(family = 'serif', hjust = 0.7),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(3, 1.5, 1, 1.5), "lines"),
      legend.position = legend.position,
      legend.background = element_rect(fill = NULL),
      axis.line = element_line(size = 0.5,linetype = 'solid',color = 'black')
    ) +
    geom_vline(xintercept = max.time, linetype = 'dashed') +
    scale_x_continuous(xlab, breaks = xaxis.break, labels = xaxis.label) +
    scale_y_continuous(ylab,
                       breaks = seq(0, 1, 0.25),
                       limits = c(0, 1),
                       sec.axis = sec_axis( ~ . * 1)
    ) +
    scale_fill_discrete('Type of Event')
  
  ##add area ans step
  p1 <- p +
    geom_area(data = df.plot,
              aes(x = time.left, y = surv.left, fill = event.left),
              position = "identity",alpha = 0.5
    ) +
    geom_area(data = df.plot,
              aes(x = time.right, y = surv.right, fill = event.right),
              position = "identity",alpha = 0.5
    ) +
    geom_step(data = df.plot,
              aes(x = time.left, y = surv.left, linetype = event.left),
              position = "identity",size = 0.5
    ) +
    geom_step(data = df.plot,
              aes(x = time.right, y = surv.right, linetype = event.right),
              position = "identity",size = 0.5
    ) +
    guides(linetype = FALSE) #remove linetype legend
  
  plot(p1)
  require(grid)
  grid.text(
    main, x = unit(0.5, "npc"),y = unit(0.95, "npc"),
    gp = gpar(fontfamily = 'serif', fontsize = 15)
  )
  grid.text(
    left.right[1], x = unit(0.3, "npc"), y = unit(0.87, "npc"),
    gp = gpar(fontfamily = 'serif',fontsize=10)
  )
  grid.text(left.right[2],x = unit(0.7, "npc"), y = unit(0.87, "npc"),
            gp = gpar(fontfamily = 'serif',fontsize=10)
  )
}


