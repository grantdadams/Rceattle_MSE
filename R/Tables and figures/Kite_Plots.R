# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}


EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                              "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
output_table = pm_summary_table(om_names, EM_names, format = FALSE, reverse = TRUE)
OM.res = output_table$GOA
OM.res = OM.res[which(OM.res$Species == "Pollock"),-1]
rownames(OM.res) <- OM.res$Performance.metric
OM.res <- OM.res[,-1]
OM.res = t(OM.res)
OM.res <- OM.res[,c(1,2,3,4,5,6,7,8)]
OM.res <- apply(OM.res, 2, normalize) * 100 # Normalize between 0 and 1


# Subset based on OM
ss_om <- OM.res[1:14,]
ssm_om <- OM.res[15:28,]
ms_om <- OM.res[29:42,]

MPcols <- gmri_pal("main")(nrow(ms_om))


hexplot_OM(ss_om, MPcols)
hexplot_OM(ssm_om, MPcols)
hexplot_OM(ms_om, MPcols)




# Library
library(fmsb)
library(gmRi)

# Create data: note in High school for several students
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- as.data.frame(rbind(rep(100,ncol(ss_om)) , rep(0,ncol(ss_om)) , ss_om))

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )

# plot with default options:
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=MPcols , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


#' Function to compare performance metrics of MSE for a single management procedure
#'
#' @description https://github.com/Blue-Matter/Slick/blob/e4b44d3fd2371bc91e6d887957334b063a1eff88/inst/shiny_apps/Slick/Source/NonTech/Pages/SpiderOM.r
#' @param OM.res Matrix of performance metrics (columns) and management procedures (rows)
#' @param MPcols Colors for each management procedure
#'
#' @return
#' @export
#'
#' @examples
hexplot_OM <- function(OM.res, MPcols) {
  dd <- dim(OM.res)
  n.PM <- dd[2]
  n.MP <- dd[1]
  
  # - Set up plot dimensions
  fill.col <- '#cccccc'
  lwd <- 1
  mplab.cex <- 2.5
  pt.cex <- 2
  pt.col <- 'darkred'
  
  
  vertices <- polyCoords(n.PM) * 100
  
  meanVals <- apply(OM.res, 1, mean)
  MP.ord <- rev(order(meanVals))
  
  meanVals <- paste0(round(meanVals,0), '%')
  
  maxScore <- OM.res == 100
  
  par(mfrow=c(n.MP, 1), oma=c(0,0,0,0), mar=c(1,1,1,1), bg=NA)
  
  
  # loop over MPs
  if (all(is.finite(vertices))) {
    for (r in 1:n.MP) {
      plot(vertices, type="l", col=fill.col, axes=FALSE, xlab="", ylab="")
      polygon(vertices, col=fill.col, border=NA)
      
      coords <- NULL
      for (j in 1:n.PM) {
        pts <- calcCoord(vertices[j,], OM.res[r,j])
        coords <- rbind(coords, pts )
      }
      coords <- rbind(coords, coords[1,])
      polygon(coords, col=MPcols[r], border=NA)
      
      for (j in 1:n.PM) {
        if (maxScore[r,j])
          points(coords[j,], cex=pt.cex, col=pt.col, pch=16)
      }
      
      text(0,0, meanVals[r], col="black", cex=mplab.cex)
    }
  }
}
