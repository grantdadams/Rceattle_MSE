# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}


EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
output_table = pm_summary_table(om_names, EM_names, format = FALSE, reverse = TRUE)
OM.res = output_table$GOA
OM.res = OM.res[which(OM.res$Species == "Pollock"),-1]
rownames(OM.res) <- OM.res$Performance.metric
OM.res <- OM.res[,-1]
OM.res = t(OM.res)
OM.res <- apply(OM.res, 2, normalize) * 100 # Normalize between 0 and 1


MPcols <- viridisLite::viridis(nrow(OM.res))

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
    for (r in MP.ord) {
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