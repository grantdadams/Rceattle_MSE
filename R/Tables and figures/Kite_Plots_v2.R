# Library
library(fmsb)
library(gmRi)


# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}


kite_plot(system = "EBS", species = "Pollock")


kite_plot <- function(system = "GOA", species = "Pollock"){
  
  # EMs
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
  
  EM_names_print <-  c("Fix M: HCR 1", "Est M: HCR 1", "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4")
  
  # OM Names
  om_names = c("SS_OM", "SSM_OM", "MS_OM")
  
  # Get output
  output_table = pm_summary_table(om_names, EM_names, format = FALSE, reverse = TRUE)
  OM.res = output_table[[system]]
  OM.res = OM.res[which(OM.res$Species == species),-1]
  rownames(OM.res) <- OM.res$Performance.metric
  OM.res <- OM.res[,-1]
  OM.res = t(OM.res)
  OM.res <- OM.res[,c(1,2,3,4,5,6,7,8)]
  OM.res <- apply(OM.res, 2, normalize) * 100 # Normalize between 0 and 1
  OM.res[which(is.nan(OM.res))] <- 100
  OM.res <- as.data.frame(OM.res)
  
  # Subset based on OM
  data_list <- list(
    # SS OM
    ss_om1 = OM.res[1:2,], ss_om5 = OM.res[8:9,], # Tier3
    ss_om3 = OM.res[3:4,], ss_om6 = OM.res[10:11,], # Cat1
    ss_om3 = OM.res[5:6,], ss_om4 = OM.res[12:13,], # Tier1
    ss_om7 = OM.res[7,], ss_om4 = OM.res[14,], # Fspr
    
    # SS-M OM
    ss_om1 = OM.res[15:16,], ss_om5 = OM.res[22:23,], # Tier3
    ss_om3 = OM.res[17:18,], ss_om6 = OM.res[24:25,], # Cat1
    ss_om3 = OM.res[19:20,], ss_om4 = OM.res[26:27,], # Tier1
    ss_om7 = OM.res[21,], ss_om4 = OM.res[28,], # Fspr
    
    # MS OM
    ss_om1 = OM.res[29:30,], ss_om5 = OM.res[36:37,], # Tier3
    ss_om3 = OM.res[31:32,], ss_om6 = OM.res[38:39,], # Cat1
    ss_om3 = OM.res[33:34,], ss_om4 = OM.res[40:41,], # Tier1
    ss_om7 = OM.res[35,], ss_om4 = OM.res[42,] # Fspr
  )
  
  # Colors
  MPcols <- gmri_pal("main")(8)
  MPcolsalpha <- alpha(MPcols[1:6], alpha = 0.6)
  MPcolsalpha <- rep("grey50", 6)
  colors <- list()
  ind <- 1
  for(i in 1:6){
    colors[[i]] <- c(MPcols[i], MPcolsalpha[i])
  }
  colors[[7]] <- MPcols[7]
  colors[[8]] <- MPcols[8]
  
  colors <- c(colors, colors, colors)
  
  # Plot
  par(mfrow = c(9,4),oma=c(0,0,0,0), mar=rep(0,4), mai = c(0,0,0,0), bg=NA)
  layout(mat = matrix(1:(4*9), 9, 4, byrow = TRUE),
         heights = c(0.25, rep(1,8)), # Heights of the two rows
         widths = c(0.25, rep(1,3))) # Widths of the two columns
  
  # Object order
  ss_om <- 1:8
  ssm_om <- 9:16
  ms_om <- 17:24
  orders <- c()
  
  for(i in 1:8){orders <- c(orders, ss_om[i], ssm_om[i], ms_om[i])}
  
  # Header
  plot.new()
  for(i in 1:3){
    plot(c(-1, 1), c(-1, 1), type="n", frame.plot=FALSE, axes=FALSE, 
         xlab="", ylab="")
    text(0,0,c("Single-spp fix M", "Single-spp est M", "Multi-spp")[i], cex = 1.5, font = 2)
  }
  
  
  # Plot it
  for(j in 1:length(data_list)){
    
    # - Print EM
    if(j %in% c(1, 4, 7, 10, 13, 16, 19, 22)){
      plot(c(-1, 1), c(-1, 1), type="n", frame.plot=FALSE, axes=FALSE, 
           xlab="", ylab="")
      text(0,0,rep(EM_names_print, each = 3)[j], cex = 1.2, srt = 90)
    }
    i = orders[j]
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
    data <- as.data.frame(rbind(rep(100,ncol(data_list[[i]])) , rep(0,ncol(data_list[[i]])) , data_list[[i]][nrow(data_list[[i]]):1,]))
    
    # plot with default options:
    radarchart( data, axistype=0,
                #custom polygon
                pcol=rev(colors[[i]]) , pfcol=rev(c(alpha(colors[[i]], alpha = 0.3))), plwd=3 , plty=list(1, c(2,1))[[ifelse(nrow(data_list[[i]]) == 1, 1, 2)]],
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                #custom labels
                vlabels=rep(NA, ncol(data))
    )
  }
}


# Legend
# plot with default options:
par(mar = c(2,2,2,2))
radarchart( data[1:2,], axistype=0,
            #custom polygon
            pcol=rev(colors[[i]]) , pfcol=rev(c(alpha(colors[[i]], alpha = 0.3))), plwd=3 , plty=list(1, c(2,1))[[ifelse(nrow(data_list[[i]]) == 1, 1, 2)]],
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
            #custom labels
            vlabels=c("Catch", "Catch IAV", "P(Open)", "SSB RMSE", "EM: P(Not overfishing)", "EM: P(Not overfished)", "OM: P(Not overfishing)", "OM: P(Not overfished)"), side = 1.7)



# Plot 
radarchart <- function(df, axistype=0, seg=4, pty=16, pcol=1:8, plty=1:6, plwd=1,
                       pdensity=NULL, pangle=45, pfcol=NA, cglty=3, cglwd=1,
                       cglcol="navy", axislabcol="blue", title="", maxmin=TRUE,
                       na.itp=TRUE, centerzero=FALSE, vlabels=NULL, vlcex=NULL,
                       caxislabels=NULL, calcex=NULL, side = 1,
                       paxislabels=NULL, palcex=NULL, ...) {
  if (!is.data.frame(df)) { cat("The data must be given as dataframe.\n"); return() }
  if ((n <- length(df))<3) { cat("The number of variables must be 3 or more.\n"); return() }
  if (maxmin==FALSE) { # when the dataframe does not include max and min as the top 2 rows.
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-side, side), c(-side, side), type="n", frame.plot=FALSE, axes=FALSE, 
       xlab="", ylab="", main=title, asp=1, ...) # define x-y coordinates without any plot
  theta <- seq(90, 450, length=n+1)*pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) { # complementary guide lines, dotted navy line by default
    polygon(xx*(i+CGap)/(seg+CGap), yy*(i+CGap)/(seg+CGap), lty=cglty, lwd=cglwd, border=cglcol)
    if (axistype==1|axistype==3) CAXISLABELS <- paste(i/seg*100,"(%)")
    if (axistype==4|axistype==5) CAXISLABELS <- sprintf("%3.2f",i/seg)
    if (!is.null(caxislabels)&(i<length(caxislabels))) CAXISLABELS <- caxislabels[i+1]
    if (axistype==1|axistype==3|axistype==4|axistype==5) {
      if (is.null(calcex)) text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol) else
        text(-0.05, (i+CGap)/(seg+CGap), CAXISLABELS, col=axislabcol, cex=calcex)
    }
  }
  if (centerzero) {
    arrows(0, 0, xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
  }
  else {
    arrows(xx/(seg+CGap), yy/(seg+CGap), xx*1, yy*1, lwd=cglwd, lty=cglty, length=0, col=cglcol)
  }
  PAXISLABELS <- df[1,1:n]
  if (!is.null(paxislabels)) PAXISLABELS <- paxislabels
  if (axistype==2|axistype==3|axistype==5) {
    if (is.null(palcex)) text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol) else
      text(xx[1:n], yy[1:n], PAXISLABELS, col=axislabcol, cex=palcex)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) VLABELS <- vlabels
  if (is.null(vlcex)) text(xx*1.2, yy*1.2, VLABELS) else
    text(xx*1.2, yy*1.2, VLABELS, cex=vlcex)
  series <- length(df[[1]])
  SX <- series-2
  if (length(pty) < SX) { ptys <- rep(pty, SX) } else { ptys <- pty }
  if (length(pcol) < SX) { pcols <- rep(pcol, SX) } else { pcols <- pcol }
  if (length(plty) < SX) { pltys <- rep(plty, SX) } else { pltys <- plty }
  if (length(plwd) < SX) { plwds <- rep(plwd, SX) } else { plwds <- plwd }
  if (length(pdensity) < SX) { pdensities <- rep(pdensity, SX) } else { pdensities <- pdensity }
  if (length(pangle) < SX) { pangles <- rep(pangle, SX)} else { pangles <- pangle }
  if (length(pfcol) < SX) { pfcols <- rep(pfcol, SX) } else { pfcols <- pfcol }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg+CGap)+(df[i,]-df[2,])/(df[1,]-df[2,])*seg/(seg+CGap)
    if (sum(!is.na(df[i,]))<3) { cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n",i,df[i,])) # for too many NA's (1.2.2012)
    } else {
      for (j in 1:n) {
        if (is.na(df[i, j])) { # how to treat NA
          if (na.itp) { # treat NA using interpolation
            left <- ifelse(j>1, j-1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left>1, left-1, n)
            }
            right <- ifelse(j<n, j+1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right<n, right+1, 1)
            }
            xxleft <- xx[left]*CGap/(seg+CGap)+xx[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
            yyleft <- yy[left]*CGap/(seg+CGap)+yy[left]*(df[i,left]-df[2,left])/(df[1,left]-df[2,left])*seg/(seg+CGap)
            xxright <- xx[right]*CGap/(seg+CGap)+xx[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
            yyright <- yy[right]*CGap/(seg+CGap)+yy[right]*(df[i,right]-df[2,right])/(df[1,right]-df[2,right])*seg/(seg+CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft; yytmp <- yyleft;
              xxleft <- xxright; yyleft <- yyright;
              xxright <- xxtmp; yyright <- yytmp;
            }
            xxs[j] <- xx[j]*(yyleft*xxright-yyright*xxleft)/(yy[j]*(xxright-xxleft)-xx[j]*(yyright-yyleft))
            yys[j] <- (yy[j]/xx[j])*xxs[j]
          } else { # treat NA as zero (origin)
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j]*CGap/(seg+CGap)+xx[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
          yys[j] <- yy[j]*CGap/(seg+CGap)+yy[j]*(df[i, j]-df[2, j])/(df[1, j]-df[2, j])*seg/(seg+CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], col=pfcols[i-2])
      } else {
        polygon(xxs, yys, lty=pltys[i-2], lwd=plwds[i-2], border=pcols[i-2], 
                density=pdensities[i-2], angle=pangles[i-2], col=pfcols[i-2])
      }
      points(xx*scale, yy*scale, pch=ptys[i-2], col=pcols[i-2])
    }
  }
}

