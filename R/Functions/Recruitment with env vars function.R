
#' plot_recruitment
#'
#' @description Function the plots the mean recruitment and 95% CI trends as estimated from Rceattle
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param model_names Names of models to be used in legend
#' @param line_col Colors of models to be used for line color
#' @param species Which species to plot e.g. c(1,4). Default = NULL plots them all
#' @param spnames Species names for legend
#' @param add_ci If the confidence interval is to be added
#' @param lwd Line width as specified by user
#' @param right_adj Multiplier for to add to the right side of the figure for fitting the legend.
#' @param mohns data.frame of mohn's rows extracted from \code{\link{retrospective}}
#' @param minyr First year to plot
#' @param height
#' @param width
#' @param save Save biomass?
#' @param incl_proj TRUE/FALSE, include projection years
#' @param mod_cex Cex of text for model name legend
#'
#' @export
#'
#' @return Returns and saves a figure with the population trajectory.
plot_recruitment_hat <- function(Rceattle,
                                 file = NULL,
                                 model_names = NULL,
                                 line_col = NULL,
                                 species = NULL,
                                 spnames = NULL,
                                 add_ci = FALSE,
                                 lwd = 3,
                                 save = FALSE,
                                 right_adj = 0,
                                 width = 7,
                                 height = 6.5,
                                 minyr = NULL,
                                 maxyr = NULL,
                                 incl_proj = FALSE,
                                 mod_cex = 1,
                                 pch = rep(16, length(Rceattle)),
                                 lty = rep(1, length(Rceattle)),
                                 alpha = 0.4,
                                 minus_dev = rep(FALSE, length(Rceattle))
) {
  
  
  # Convert single one into a list
  if(class(Rceattle) == "Rceattle"){
    Rceattle <- list(Rceattle)
  }
  
  
  # Species names
  if(is.null(spnames)){
    spnames =  Rceattle[[1]]$data_list$spnames
  }
  
  # Extract data objects
  Endyrs <-  sapply(Rceattle, function(x) x$data_list$endyr)
  years <- lapply(Rceattle, function(x) x$data_list$styr:x$data_list$endyr)
  if(incl_proj){
    years <- lapply(Rceattle, function(x) x$data_list$styr:x$data_list$projyr)
  }
  if(!is.null(maxyr)){
    years <- lapply(Rceattle, function(x) x$data_list$styr:min(c(maxyr, x$data_list$projyr)))
  }
  
  max_endyr <- max(unlist(Endyrs), na.rm = TRUE)
  nyrs_vec <- sapply(years, length)
  nyrs <- max(nyrs_vec)
  
  if(is.null(maxyr)){maxyr <- max((sapply(years, max)))}
  if(is.null(minyr)){minyr <- min((sapply(years, min)))}
  
  nspp <- Rceattle[[1]]$data_list$nspp
  
  minage <- Rceattle[[1]]$data_list$minage
  estDynamics <- Rceattle[[1]]$data_list$estDynamics
  
  
  if(is.null(species)){
    species <- 1:nspp
  }
  spp <- species
  
  
  # Get recruitment
  quantity <-
    array(NA, dim = c(nspp, nyrs,  length(Rceattle)))
  quantity_hat <-
    array(NA, dim = c(nspp, nyrs,  length(Rceattle)))
  meanR = matrix(NA, nrow = length(Rceattle), ncol = nspp)
  R0 = matrix(NA, nrow = length(Rceattle), ncol = nspp)
  
  for (i in 1:length(Rceattle)) {
    # - Get quantities
    meanR[i,] <- Rceattle[[i]]$quantities$mean_rec
    R0[i,] <- Rceattle[[i]]$quantities$R0
    quantity[, 1:nyrs_vec[i] , i] <- Rceattle[[i]]$quantities$R[,1:nyrs_vec[i]]
    quantity_hat[, 1:nyrs_vec[i] , i] <- Rceattle[[i]]$quantities$R_hat[,1:nyrs_vec[i]]
    
    if(minus_dev[i]){
      quantity_hat[, 1:nyrs_vec[i] , i] <- exp(log(Rceattle[[i]]$quantities$R[,1:nyrs_vec[i]]) - Rceattle[[i]]$estimated_params$rec_dev[,1:nyrs_vec[i]])
    }
  }
  
  ## Plot limits
  ymax <- c()
  ymin <- c()
  for (sp in 1:nspp) {
    ymax[sp] <- max(c(quantity[sp, , ], quantity_hat[sp, , ], 0), na.rm = T)
    ymin[sp] <- min(c(quantity[sp, , ], quantity_hat[sp, , ], 0), na.rm = T)
  }
  ymax <- ymax
  
  if (is.null(line_col)) {
    line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
  }
  
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (i in 1:loops) {
    if (i == 2) {
      filename <- paste0(file, "_recruitment_trajectory", ".png")
      png(
        file = filename ,
        width = width,# 169 / 25.4,
        height = height,# 150 / 25.4,
        units = "in",
        res = 300
      )
    }
    
    # Plot configuration
    layout(matrix(1:(length(spp) + 2), nrow = (length(spp) + 2)), heights = c(0.1, rep(1, length(spp)), 0.2))
    par(
      mar = c(0, 3 , 0 , 1) ,
      oma = c(0 , 0 , 0 , 0),
      tcl = -0.35,
      mgp = c(1.75, 0.5, 0)
    )
    plot.new()
    
    for (j in 1:length(spp)) {
      plot(
        y = NA,
        x = NA,
        ylim = c(ymin[spp[j]], ymax[spp[j]]),
        xlim = c(minyr, maxyr + (maxyr - minyr) * right_adj),
        xlab = "Year",
        ylab = NA,
        xaxt = c(rep("n", length(spp) - 1), "s")[j]
      )
      
      if(j == 2){
        mtext("Age-1 recruits (million)", side = 2, line = 1.7, cex = 0.9)
      }
      
      # Horizontal line at end yr
      if(incl_proj){
        abline(v = Rceattle[[length(Rceattle)]]$data_list$endyr, lwd  = lwd, col = "grey", lty = 2)
      }
      
      # Legends
      legend("topleft",
             legend = spnames[spp[j]],
             bty = "n",
             cex = 1)
      
      if (spp[j] == 1) {
        if(!is.null(model_names)){
          legend(
            "topright",
            legend = model_names,
            pch = pch,
            col = line_col,
            bty = "n",
            cex = mod_cex
          )
        }
      }
      
      # Mean quantity
      for (k in 1) {
        lines(
          x = years[[k]],
          y = quantity[spp[j], 1:length(years[[k]]), k],
          lty = lty[k],
          lwd = lwd,
          col = line_col[k]
        ) # Median
      }
      
      # R_hat
      for (k in 1:dim(quantity)[3]) {
        points(
          x = years[[k]],
          y = quantity_hat[spp[j], 1:length(years[[k]]), k],
          pch = pch[k],
          cex = 2,
          col = line_col[k]
        ) # Median
      }
      
      # # R0
      # for (k in 1:dim(quantity)[3]) {
      #   abline(
      #     h = R0[k, spp[j]],
      #     lty = 2,
      #     lwd = lwd,
      #     col = line_col[k]
      #   ) # Median
      # }
    }
    
    
    if (i == 2) {
      dev.off()
    }
  }
}

