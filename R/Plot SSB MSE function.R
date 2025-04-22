

#' plot_ssb_mse
#'
#' @description Function the plots the mean ssb/R/depletion and 95% CI trends as estimated from Rceattle MSE
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param model_names Names of models to be used in legend
#' @param line_col Colors of models to be used for line color
#' @param species Which species to plot e.g. c(1,4). Default = NULL plots them all
#' @param spnames Species names for legend
#' @param lwd Line width as specified by user
#' @param right_adj Multiplier for to add to the right side of the figure for fitting the legend.
#' @param minyr First year to plot
#' @param height
#' @param width
#' @param incl_proj TRUE/FALSE, include projection years
#' @param mod_cex Cex of text for model name legend
#'
#' @export
#'
#' @return Returns and saves a figure with the population trajectory.
plot_ssb_mse <- function(Rceattle,
                         file = NULL,
                         model_names = NULL,
                         line_col = NULL,
                         species = NULL,
                         spnames = NULL,
                         lwd = 3,
                         rec = FALSE,
                         depletion = FALSE,
                         right_adj = 0,
                         width = 7,
                         height = 6.5,
                         minyr = 1977,
                         maxyr = 2100,
                         incl_proj = FALSE,
                         mod_cex = 1,
                         lty = rep(1, length(Rceattle)),
                         alpha = 0.4,
                         ymax = NULL) {
  
  
  # Species names
  spnames =  Rceattle[[1]][[1]]$data_list$spnames
  nspp <- Rceattle[[1]][[1]]$data_list$nspp
  
  if(depletion == TRUE & rec == TRUE){
    stop("rec and depletion cant be true at the same time")
  }
  
  
  # Extract data objects
  if(incl_proj == FALSE){
    Years <- lapply(Rceattle[[1]], function(x) x$data_list$styr: x$data_list$endyr)
  }
  if(incl_proj){
    Years <- lapply(Rceattle[[1]], function(x) x$data_list$styr:x$data_list$projyr)
  }
  if(!is.null(maxyr)){
    Years <- lapply(Rceattle[[1]], function(x) x$data_list$styr:min(c(maxyr, x$data_list$projyr)))
  }
  HindYears <- lapply(Rceattle[[1]], function(x) x$data_list$styr:(x$data_list$srr_meanyr))
  ProjYears <- lapply(Rceattle[[1]], function(x) (x$data_list$srr_meanyr+1):x$data_list$projyr)
  Endyrs <- lapply(Rceattle[[1]], function(x) x$data_list$endyr)
  meanyrs <- lapply(Rceattle[[1]], function(x) x$data_list$srr_meanyr)
  fsh_list <- list()
  fsh_hat_list <- list()
  proj_fsh_hat_list <- list()
  nyrs_hind <- length(HindYears[[1]])
  nyrs <- length(Years[[1]])
  
  
  
  minage <- Rceattle[[1]][[1]]$data_list$minage
  
  if(is.null(species)){
    species <- 1:nspp
  }
  spp <- species 
  
  
  # Get depletion
  quantity <- list()
  
  for (i in 1:length(Rceattle)) {
    quantity[[i]] <- array(NA, dim = c(nspp, nyrs,  length(Rceattle[[i]])))
    for(j in 1:length(Rceattle[[i]])){
      if(rec){
        quantity[[i]][, 1:nyrs , j] <- Rceattle[[i]][[j]]$quantities$R[,1:nyrs] / 1000000
      } else if(depletion){
        quantity[[i]][, 1:nyrs , j] <- Rceattle[[i]][[j]]$quantities$depletionSSB[,1:nyrs]
      }else{
        quantity[[i]][, 1:nyrs , j] <- Rceattle[[i]][[j]]$quantities$biomassSSB[,1:nyrs] / 1000000
      }
    }
  }
  
  # - MSE objects
  quantity_med <- list()
  quantity_upper95 <- list()
  quantity_lower95 <- list()
  for (i in 1:length(Rceattle)) {
    
    # -- Get quantiles and mean across simulations
    quantity_med[[i]] <- apply( quantity[[i]][,,1:length(Rceattle[[i]])], c(1,2), function(x) quantile(x, probs = 0.5) )
    quantity_upper95[[i]] <- apply( quantity[[i]][,,1:length(Rceattle[[i]])], c(1,2), function(x) quantile(x, probs = 0.975) )
    quantity_lower95[[i]] <- apply( quantity[[i]][,,1:length(Rceattle[[i]])], c(1,2), function(x) quantile(x, probs = 0.025) )
  }
  
  
  
  # - Plot limits
  if(is.null(ymax)){ymax <- c()}
  ymin <- c()
  for (sp in 1:nspp) {
    for (i in 1:length(Rceattle)) {
      ymax[sp] <- max(c(quantity_upper95[[i]][sp, ], 0, ymax[sp]), na.rm = T)
      ymin[sp] <- min(c(quantity_lower95[[i]][sp, ], 0), na.rm = T)
    }
  }
  ymax <- ymax * 1.2
  
  
  # - Line colors
  if (is.null(line_col)) {
    line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
    
  }
  
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (i in 1:loops) {
    if (i == 2) {
      filename <- paste0(file, ifelse(rec, "_recruitment_trajectory", 
                                      ifelse(depletion, "_depletion_trajectory", 
                                             "_ssb_trajectory")), ".png")
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
      
      if(j == 2 & !rec & !depletion){
        mtext("Spawning stock biomass (million mt)", side = 2, line = 1.7, cex = 0.9)
      }      
      if(j == 2 & rec){
        mtext("Age-1 recruits (million)", side = 2, line = 1.7, cex = 0.9)
      }      
      if(j == 2 & depletion){
        mtext("SSB depletion", side = 2, line = 1.7, cex = 0.9)
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
            lty = lty,
            lwd = lwd,
            col = line_col,
            bty = "n",
            cex = mod_cex
          )
        }
      }
      
      
      # Credible interval
      for (k in 1:length(quantity)) {
        
        
        # - 95% CI
        polygon(
          x = c(Years[[1]], rev(Years[[1]])),
          y = c(quantity_upper95[[k]][spp[j], 1:nyrs], rev(quantity_lower95[[k]][spp[j], 1:nyrs])),
          col = adjustcolor( line_col[k], alpha.f = alpha),
          border = NA
        )
        
        lines(
          x = Years[[1]],
          y = quantity_med[[k]][spp[j],1:nyrs],
          col = line_col[k],
          lwd = 2
        )
        
        lines(
          x = HindYears[[1]],
          y = quantity_med[[k]][spp[j],1:nyrs_hind],
          col = 1,
          lwd = 2
        )
      }
      
      # # Mean quantity
      # for (k in 1:dim(quantity)[3]) {
      #   lines(
      #     x = years[[k]],
      #     y = quantity[spp[j], 1:nyrs, k],
      #     lty = lty[k],
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


