# Functions for plot-level analyses


bwmean <- function (b,w) {
  #bootstraped mean
  # if no weights include a vector with 1s
  # this first function calculates one round of bootstrapped mean
  temp <- cbind (b,w)
  boots <- temp[sample (nrow (temp), nrow (temp), replace = T),]
  weighted.mean (boots[,1], boots[,2])
}

TM.bwmean <- function (b,w,n= 10000) {
  # replicates bwmean [10000 times and generate mean and 95%CI]
  a <- replicate (n, bwmean (b,w))
  c (mean (a), quantile(a, probs =0.025), quantile(a, probs =0.975))
}


MapMortalityRates <- function (plot_rates) {
  # Figure 1 - mortality rates ==========
  # mortality rate per plot 
  # Calculate mean per plot weithged by their census interval
  
  mechanic = as.character (c(rgb(0,102/255,0,0.4),rgb(240/255,0,0,0.4),
                             rgb(225/255,128/255,0,0.4),rgb(0,76/255,153/255,0.4)))
  standing = as.character (c(rgb(0,102/255,0,1),rgb(240/255,0,0,1),
                             rgb(225/255,128/255,0,1),rgb(0,76/255,153/255,1)))

  
  map(database = "world",ylim=c(-23,15), xlim=c(-81,-35),
      border = 0,mar=rep(0,4))
  map.scale(-55, 13, relwidth=0.2, metric=T, ratio=F, cex=0.9) # subdiv)
  map('rivers', add =TRUE, col="lightcyan3")
  abline(h=0); box()
  
  legend(x = -49.9, y = -14.2, 
         legend = expression ('stem mortality (% y'^'-1'*')'), bty = 'o', box.col = 'white',cex = 1)
  
  legend(x = -53.5, y = -18.5, 
         legend = c (0.4,'2   ','      10'),
         pch = 21,
         pt.cex = round (c (min (plot_rates$m.rates.annual),
                            mean (plot_rates$m.rates.annual),
                            max (plot_rates$m.rates.annual)),1), 
         horiz = T, pt.lwd = 2, bty = 'o', box.col = 'white')
  
  points (cluster.overplot(plot_rates[,'Longitude.Decimal'], plot_rates[,'Latitude.Decimal'],away = c(1.5,1.3)),  
          cex = plot_rates$m.rates.annual, lwd = 2, col = as.character (plot_rates$mycol))
  stan <- c(51,61,44,55) # proportion of trees that die standing 
  stru <- 100 - stan # proportion of trees that die broken/uprooted 
  z1 <- data.frame (region = unique (mod$region), stan, stru) 
  x <-  c(-56,-47,-79,-45)
  y <- c(8.2,2.2,-15,-12)
  my.radius <- TM.bwmean(plot_rates$m.rates.annual, w =1)[1]
  for (i in 1:4) {
    z <- array (data = c (z1[i,2],z1[i,3]), dim = c(1,2))
    rownames (z) <- paste (x[i],y[i], sep =', ')
    draw.pie(x[i], y[i], z, radius = my.radius, col = c(standing [i], mechanic [i]), border = T)
  }
  
  z <- array (data = c (48.3,100-48.3), dim = c(1,2))
  rownames (z) <- paste (-41,6, sep =', ')
  draw.pie(-41, 6, z, radius = my.radius, 
           col = c('black', 'grey'), 
           border = T)
  
}


CompareRegions <- function (mod, plot_rates, census_rates) {
  mechanic = as.character (c(rgb(0,102/255,0,0.4),rgb(240/255,0,0,0.4),
                             rgb(225/255,128/255,0,0.4),rgb(0,76/255,153/255,0.4)))
  standing = as.character (c(rgb(0,102/255,0,1),rgb(240/255,0,0,1),
                             rgb(225/255,128/255,0,1),rgb(0,76/255,153/255,1)))
  
  
  par (mfrow = c(2,2), mar = c(2,3,0.3,0.5), cex = 0.7)
  leg <- unique (plot_rates[,c('region', 'mycol')])
  leg <- leg [order (leg$region),]
  bp <- boxplot ((plot_rates$m.rates.annual)~plot_rates$region, 
           ylab = '',
           col = as.character (leg$mycol), ylim = c(0,6),
           names = c ('North','East-Central','West','South'), border = '#3C3C3C')
  text (0.7,5.8, 'a', border = NULL,  font = 2, cex = 1.5)
  
  
  a <- aov ((census_rates$m.rates.annual)~census_rates$region)
  TukeyHSD (a)
  pos <- bp$stats[5,]+0.15
  text (c(1,2,3,4), pos, c('a','a','b','c'), border = NULL,  font = 2, cex = 1.2)
  
  
  
  
  par (mar = c(5,3,0.3,0.5))
  
  # BARPLOT
  meanS <- c(48,51, 61,44,55)
  lowS <- c(45,42,50,40,48)
  upS<- c(52,59,72,48,63)
  meanNS <- c(51,49,39, 55,44)
  lowNS <- c(48,41,28,51,37)
  upNS <- c(54,57,50,59,52)
  
  
  bpdata <- data.frame (region = rep (c('All','North', 'E-C','West','South',''),2)[-12], 
                        meanval = c(meanS,0,meanNS), 
                        up = c(upS,0,upNS),
                        low = c(lowS,0,lowNS), 
                        mod = c (rep ('standing', 6),rep ('broken/uprooted', 5)),
                        order = c(1:11))
  
  
  bpdata$region_col <- c ('black',rgb(0,102/255,0,1),
                          rgb(240/255,0,0,1),
                          rgb(225/255,128/255,0,1),
                          rgb(0,76/255,153/255,1),'grey','grey',rgb(0,102/255,0,0.4),
                          rgb(240/255,0,0,0.4),rgb(225/255,128/255,0,0.4),rgb(0,76/255,153/255,0.4))
  
  
  a <- barplot (bpdata$meanval,
                col = as.character (bpdata$region_col), width = 0.2,
                names.arg = '', ylim = c(0,90))
  axpos <- c((a[1] + a[2])/2,(a[3] + a[4])/2,(a[5] + a[6])/2,(a[7] + a[8])/2,(a[9] + a[10])/2)
  
  
  axis (side = 1, at = a, labels = bpdata$region, tick = F, cex = 0.5, las = 2, line = 0)
  arrows(x0=a, y0=bpdata$low,y1=bpdata$up, lwd = 1.5, angle = 90,
         code = 3, length = 0.05) 
  notstand <- a[c(7,8,9,10,11)]
  stand <- a[c(1,2,3,4,5)]
  text (x = stand[-1],y= upS[-1] + 3, c("ab","a","b","a"), border = NULL,  font = 2, cex = 1)
  text (x = notstand[-1],y= upNS[-1] + 3, c("ab","b","a","b"), border = NULL,  font = 2, cex = 1)
  text (x = stand[5], y = 80, 'standing', font = 2)
  text (x = 2.2, y = 80, 'broken/uprooted', font = 2)

  text (0.2,77, 'b', border = NULL,  font = 2, cex = 1.5)
  
  leg$mechanic <- mechanic
  leg$standing <- standing
  
  
  
  par (mar = c(2,3,0.3,0.5))
  bp <- boxplot ((mod$m.rates.likstanding*100)~mod$region, ylab = '',
           col = as.character (leg$mycol),
           ylim = c(0,3.5),names = c ('North','East-Central','West','South'), 
           yaxt = 'n', border = '#3C3C3C')
  text (0.7,3.3, 'c', border = NULL, font = 2, cex = 1.5)
  axis (side = 2, at = c(0,1,2,3))
  a <- aov ((mod$m.rates.likstanding*100)~mod$region)
  TukeyHSD (a)
  pos <- bp$stats[5,]+0.1
  text (c(1,2,3,4), pos, c('a','a','a','c'), border = NULL,  font = 2,  cex = 1.2)
  
  bp <- boxplot ((mod$m.rates.notstanding*100)~mod$region, 
           ylab = '',
           col = as.character (mechanic), ylim = c(0,3.5), 
           names = c ('North','East-Central','West','South'),
           yaxt = 'n', border = '#3C3C3C')
  
  pos <- bp$stats[5,]+0.1
  text (0.7,3.3, 'd', border = NULL, font = 2, cex = 1.5)
  axis (side = 2, at = c(0,1,2,3))
  a <- aov ((mod$m.rates.notstanding*100)~mod$region)
  TukeyHSD (a)
  text (c(1,2,3,4), pos, c('ac','a','b','bc'), border = NULL,  font = 2,  cex = 1.2)
}
