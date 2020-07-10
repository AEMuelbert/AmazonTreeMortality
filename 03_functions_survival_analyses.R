CompareSurvivalModels <- function (S, event = 'dead') {
  #***********
  # SURVIVAL Analyses
  #***********
  S_ind <- Surv (time =S$interval, event = S$dead)
  
  # FULL MODEL
  
  mFULL.Plot <- coxph (S_ind ~ poly (D40, 2)+
                         DrelGR0 + 
                         MaxD + 
                         MeangrD +
                         WD + 
                         WDA.gen+
                         frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  
  mFULLnoIndSize <- coxph (S_ind ~ #poly (D40, 2)+
                             DrelGR0 + 
                             MaxD + 
                             MeangrD +
                             WD + 
                             WDA.gen+
                             frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  mFULLDLin <- coxph (S_ind ~ D40+
                        DrelGR0 + 
                        MaxD + 
                        MeangrD +
                        WD + 
                        WDA.gen+
                        frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  mFULLnoIndGrowth <- coxph (S_ind ~ poly (D40, 2)+
                               #DrelGR0 + 
                               MaxD + 
                               MeangrD +
                               WD + 
                               WDA.gen+
                               frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  mFULLnoWD <- coxph (S_ind ~ poly (D40, 2)+
                        DrelGR0 + 
                        MaxD + 
                        MeangrD +
                        #WD + 
                        WDA.gen+
                        frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  
  mFULLnoWDA <- coxph (S_ind ~ poly (D40, 2)+
                         DrelGR0 + 
                         MaxD + 
                         MeangrD +
                         WD + 
                         #WDA.gen+
                         frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  
  
  
  mFULLnoMeangr <- coxph (S_ind ~ poly (D40, 2)+
                            DrelGR0 + 
                            MaxD + 
                            #MeangrD +
                            WD + 
                            WDA.gen+
                            frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  
  mFULLnoMaxD <- coxph (S_ind ~ poly (D40, 2)+
                          DrelGR0 + 
                          #MaxD + 
                          MeangrD +
                          WD + 
                          WDA.gen+
                          frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  
  m1 <- coxph (S_ind ~ 1 +
                 frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  m_Spp <- coxph (S_ind ~ 
                    MaxD + 
                    MeangrD +
                    WD + 
                    WDA.gen+
                    frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  m_Ind <- coxph (S_ind ~ poly (D40, 2)+
                    DrelGR0 + 
                    frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  m_relGR <- coxph (S_ind ~ DrelGR0 + 
                      frailty(Plot.Code, method = 'aic', sparse=10),data=S)
  
  mMeangr <- coxph (S_ind ~ MeangrD + 
                      frailty(Plot.Code, method = 'aic', sparse=10),data=S)

  
  AIC (mFULL.Plot,mFULLnoIndSize,mFULLDLin,mFULLnoIndGrowth,mFULLnoWD, mFULLnoWDA,mFULLnoMeangr, mFULLnoMaxD,m_Spp, m_Ind,m1, m_relGR, mMeangr)
  
}


CompareCI <- function (mBS, mWA, mEC, mGS) {
  ci <- rbind (data.frame (var = rownames (summary (mBS)$conf.int), summary (mBS)$conf.int, region = 'South', col =rgb(0,76/255,153/255,1)),
               data.frame (var = rownames (summary (mWA)$conf.int), summary (mWA)$conf.int, region = 'West', col = rgb(225/255,128/255,0,1)),
               data.frame (var = rownames (summary (mEC)$conf.int), summary (mEC)$conf.int, region = 'East-Central', col = rgb(240/255,0,0,1)),
               data.frame (var = rownames (summary (mGS)$conf.int), summary (mGS)$conf.int, region = 'North', col = rgb(0,102/255,0,1)))
  
  colnames (ci) <- c('var','exp.coef','exp_coef','lowci','upci','region', 'col')
  
  allvar <- unique (ci$var)
  
  ci$xlim.up <- 1
  ci$xlim.low <- 1
  for (i in 1:length (allvar)) {
    var <- allvar [i]
    ci$xlim.up <- ifelse (ci$var == var, max (ci[which (ci$var == var),'upci']), ci$xlim.up) 
    ci$xlim.low <- ifelse (ci$var == var, min (ci[which (ci$var == var),'lowci']), ci$xlim.low)  
  }
  
  ci$xlim.low <- ifelse (ci$xlim.low > 1 & ci$xlim.up > 1, 1,ci$xlim.low)
  ci$xlim.up <- ifelse (ci$xlim.low < 1 & ci$xlim.up < 1, 1,ci$xlim.up)
  ci
}


PlotCompareCI <- function (ci) {
  
  varnames <- c ('D', expression ('D'^'2'), 'Rel. growth','Max D','Mean growth','WD', 'WDA')
  par (mfrow = c (7,1), mar = c(1,1.5,0.5,0), cex = 0.5)
  for (i in 1:length (allvar)) {
    a <- ci [which (ci$var == allvar[i]),]
    
    l <- unique (a$xlim.low)
    u <- unique (a$xlim.up)
    vline <- 1
    
    if (allvar[i] %in% c('poly(D40, 2)2','poly(D40, 2)1')) {
      l <- log (l)
      u <- log (u)
      a$exp.coef <- log (a$exp.coef)
      a$upci <- log (a$upci)
      a$lowci <- log (a$lowci)
      vline <- 0
    }
    
    plot (a$exp.coef,a$region, xlim = c(l,u), pch = 20, axes = F,
          xlab = '', ylab = '', col = as.character (a$col), ylim = c(0.7,4.3))
    title (ylab = varnames [i],line = .1, cex = 0.2)
    abline (v = vline, col = 'grey', lwd = 2)
    arrows(x0=a$lowci,x1=a$upci, y0=c(1:4), lwd = 1.5, angle = 90,
           code = 3, length = 0.02, col = as.character (a$col)) 

    axis (side = 1, labels = F)
    
  }
}

PlotSurvReg <- function (models, data) {
  mod.par <- list (list (variable = 'D40', par.vari = seq(102,663,by=1), legend = 'Diameter mm', ylab = "Relative hazard"),
                   list (variable = 'DrelGR0', par.vari = seq(-0.92,6.02,by=0.1), legend = expression ('Relative growth % y'^'-1'), ylab = ""),
                   list (variable = 'MaxD', par.vari = seq(195,741,by=1), legend = 'Max D mm', ylab = "Relative hazard"),
                   list (variable = 'MeangrD', par.vari = seq(0,5,by=0.1), legend = expression ('Mean growth rate mm y' ^'-1'), ylab = ''),
                   list (variable = 'WD', par.vari = seq(0.26,0.93,by=0.01), legend = expression ('Wood density g cm'^'-3'), ylab = "Relative hazard"),
                   list (variable = 'WDA.gen', par.vari =seq(-571,-2,by=1), legend = expression ('WDA mm y'^'-1'), ylab = ""))
  
  covari <- c ('D40',
               'DrelGR0',
               'MaxD','MeangrD',
               'WD', 'WDA.gen')
  
  col.line <- list (rgb(0,102/255,0,0.7),
                    rgb(240/255,0,0,0.7),
                    rgb(225/255,128/255,0,0.7),
                    rgb(0,76/255,153/255,0.7),
                    rgb(0,0,0,1))
  col.error <- list (rgb(0,102/255,0,0.15),
                     rgb(240/255,0,0,0.15),
                     rgb(225/255,128/255,0,0.15),
                     rgb(0,76/255,153/255,0.15),
                     rgb(0,0,0,0.2))
  leg2 <- c ('Northern',
             'East Central', 
             'Western',
             'Southern',
             'All Amaz')
  
  par(mar=c(4.5,4.2,0.5,0.7))
  par(mfrow = c(3, 2))
  leg <- c('a','b','c','d','e','f')
  for (j in 1:length (covari)) {
    for (i in 1:length(col.error)) {
      datai <- as.data.frame (data [i])
      a <- as.numeric (quantile (datai[,covari[j]], probs = 0.05))
      b <- as.numeric (quantile (datai[,covari[j]], probs = 0.95))
      par.var <- seq(a,b,by= (abs(b-a))/1000)
      newdata <- as.data.frame(matrix (0,ncol = length (covari), nrow = length (par.var)))
      colnames (newdata) <-   covari
      newdata[,mod.par[[j]]$variable] <- par.var
      Sur.pred =predict(models[[i]], newdata,
                        type="risk",se.fit=T)
      Sur.pred$fit1=Sur.pred$fit/mean (Sur.pred$fit) #divide by mean, not sure why this is needed.
      Sur.pred$se.fit1=Sur.pred$se.fit/mean (Sur.pred$fit)
      
      
      if (i== 1) {
        
        if (j==1) {
          if (as.numeric (summary(models[[i]])$coefficients[,'p'][j]) < 0.05) {
            plot(Sur.pred$fit1~par.var,
                 ylim=c(0.5,1.5),type="l",col=col.line[[i]],lwd=2,cex.axis=1.3,cex.lab=1.5,cex.main=1.4,
                 main="",ylab= mod.par[[j]]$ylab,xlab=mod.par[[j]]$legend) 
            polygon(x=c(par.var,rev(par.var)),
                    y=c(Sur.pred$fit1-Sur.pred$se.fit1,rev(Sur.pred$fit1+Sur.pred$se.fit1)),
                    col=col.error[[i]],border=NA)
            
          } else {
            plot(Sur.pred$fit1~par.var,
                 ylim=c(0.5,1.5),type="l",col=col.line[[i]],lwd=2,cex.axis=1.3,cex.lab=1.5,cex.main=1.4,
                 main="",ylab=mod.par[[j]]$ylab,xlab=mod.par[[j]]$legend, lty = 'dotted')
          }
        } else {
          if (as.numeric (summary(models[[i]])$coefficients[,'p'][j+1]) < 0.05) {
            plot(Sur.pred$fit1~par.var,
                 ylim=c(0.5,1.5),type="l",col=col.line[[i]],lwd=2,cex.axis=1.3,cex.lab=1.5,cex.main=1.4,
                 main="",ylab=mod.par[[j]]$ylab,xlab=mod.par[[j]]$legend)  
            polygon(x=c(par.var,rev(par.var)),
                    y=c(Sur.pred$fit1-Sur.pred$se.fit1,rev(Sur.pred$fit1+Sur.pred$se.fit1)),
                    col=col.error[[i]],border=NA)
          } else {
            plot(Sur.pred$fit1~par.var,
                 ylim=c(0.5,1.5),type="l",col=col.line[[i]],lwd=2,cex.axis=1.3,cex.lab=1.5,cex.main=1.4,
                 main="",ylab=mod.par[[j]]$ylab,xlab=mod.par[[j]]$legend, lty = 'dotted')
          } 
        }
        
      } else {
        if (j==1) {
          if (as.numeric (summary(models[[i]])$coefficients[,'p'][j]) < 0.05) {
            points (par.var,Sur.pred$fit1,
                    ylim=c(0.5,1.5),type="l",col = col.line[[i]],lwd=2) 
            polygon(x=c(par.var,rev(par.var)),
                    y=c(Sur.pred$fit1-Sur.pred$se.fit1,rev(Sur.pred$fit1+Sur.pred$se.fit1)),
                    col=col.error[[i]],border=NA)
            
          } else {
            points (par.var,Sur.pred$fit1,
                    ylim=c(0.5,1.5),type="l",col = col.line[[i]],lwd=2, lty = 'dotted')
          }
        }else {
          if (as.numeric (summary(models[[i]])$coefficients[,'p'][j+1]) < 0.05) {
            points (par.var,Sur.pred$fit1,
                    ylim=c(0.5,1.5),type="l",col = col.line[[i]],lwd=2)  
            polygon(x=c(par.var,rev(par.var)),
                    y=c(Sur.pred$fit1-Sur.pred$se.fit1,rev(Sur.pred$fit1+Sur.pred$se.fit1)),
                    col=col.error[[i]],border=NA)
          } else {
            points (par.var,Sur.pred$fit1,
                    ylim=c(0.5,1.5),type="l",col = col.line[[i]],lwd=2, lty = 'dotted')
          }
        }
      }
    }
    
    abline(h=1,lty=2, col='grey')  
    if (j == 1) {
      legend ('topright', legend = leg2,
              col=unlist (col.line), lty = 1 ,lwd = 2, cex = 0.8,bty = "n")
    }
    legend ('topleft', legend = leg [j], bty = "n", cex = 1.5, text.font = 2)
  }
}
