#' Plot natural mortality by age
#'
#' @description Function the plots the natural mortality at age (M1 + M2) as estimated from Rceattle. Returns and saves a figure with the M-at-age trajectory.
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param age Age to plot M at age
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param model_names Names of models to be used in legend
#' @param line_col Colors of models to be used for line color
#' @param spnames Species names for legend
#' @param species Which species to plot e.g. c(1,4). Default = NULL plots them all
#' @param lwd Line width as specified by user
#' @param right_adj Multiplier for to add to the right side of the figure for fitting the legend.
#' @param minyr first year to plot
#' @param mohns data.frame of mohn's rows extracted from \code{\link{retrospective}}
#' @param incl_proj TRUE/FALSE include projections years
#' @param incl_mean TRUE/FALSE include time series mean as horizontal line
#' @param add_ci TRUE/FALSE, includes 95 percent confidence interval
#'
#' @export
#'
plot_m_at_age_mse <-
  function(Rceattle,
           age = 1,
           file = NULL,
           model_names = NULL,
           line_col = NULL,
           species = NULL,
           spnames = NULL,
           add_ci = FALSE,
           lwd = 3,
           save = FALSE,
           right_adj = 0,
           top_adj = 1.2,
           width = 7,
           height = 6.5,
           minyr = NULL,
           incl_proj = FALSE,
           mod_cex = 1,
           alpha = 0.4,
           mod_avg = rep(FALSE, length(Rceattle)),
           mse = FALSE,
           OM = TRUE,
           reference = NULL) {
    
    
    
    # Species names
    spnames =  Rceattle$Sim_1$OM$data_list$spnames
    nmse <- length(Rceattle)
    
    
    # Extract data objects
    Years <- Rceattle$Sim_1$OM$data_list$styr:Rceattle$Sim_1$OM$data_list$projyr
    
    styr <- Rceattle$Sim_1$OM$data_list$styr
    endyr <- Rceattle$Sim_1$EM$EM$data_list$endyr
    nyrs_vec <- 1:length(Years)
    nyrs <- length(Years)
    projyr <- Rceattle$Sim_1$OM$data_list$projyr
    projyrs_vec <- (endyr:projyr)-styr+1
    if(is.null(minyr)){minyr <- Rceattle$Sim_1$OM$data_list$styr}
    
    nspp <- Rceattle$Sim_1$OM$data_list$nspp
    
    maxage <- max(Rceattle$Sim_1$OM$data_list$nages)
    nsex <- Rceattle$Sim_1$OM$data_list$nsex
    minage <- Rceattle$Sim_1$OM$data_list$minage
    estDynamics <- Rceattle$Sim_1$OM$data_list$estDynamics
    
    
    if(is.null(species)){
      species <- 1:nspp
    }
    spp <- species
    
    
    # Get depletion
    quantity <-
      array(NA, dim = c(nspp, 2, nyrs,  length(Rceattle)))
    quantity_sd <-
      array(NA, dim = c(nspp, 2, nyrs,  length(Rceattle)))
    log_quantity_sd <-
      array(NA, dim = c(nspp, 2, nyrs,  length(Rceattle)))
    log_quantity_mu <-
      array(NA, dim = c(nspp, 2, nyrs,  length(Rceattle)))
    
    for (i in 1:length(Rceattle)) {
      for(j in 1:length(projyrs_vec)){
        if(j == 1){
          quantity[,, 1:projyrs_vec[j], i] <- Rceattle[[i]]$EM$EM$quantities$M[,,age,1]
        } else{
          quantity[,, projyrs_vec[j], i] <- Rceattle[[i]]$EM[[j]]$quantities$M[,,age,1]
        }
      }
    }
    
    # - MSE objects
    # -- Get quantiles and mean across simulations
    quantity_upper95 <- apply( quantity[,,,1:nmse], c(1,2,3), function(x) quantile(x, probs = 0.975, na.rm = TRUE) )
    quantity_lower95 <- apply( quantity[,,,1:nmse], c(1,2,3), function(x) quantile(x, probs = 0.025, na.rm = TRUE) )
    quantity_upper50 <- apply( quantity[,,,1:nmse], c(1,2,3), function(x) quantile(x, probs = 0.75, na.rm = TRUE) )
    quantity_lower50 <- apply( quantity[,,,1:nmse], c(1,2,3), function(x) quantile(x, probs = 0.25, na.rm = TRUE) )
    
    # -- Put back in array for indexing below
    quantity <- array(apply( quantity[,,,1:nmse], c(1,2,3), mean ), dim = c(nspp, 2, nyrs,  1))
    quantity_upper95 <- array(quantity_upper95, dim = c(nspp, 2, nyrs,  1))
    quantity_lower95 <- array(quantity_lower95, dim = c(nspp, 2, nyrs,  1))
    quantity_upper50 <- array(quantity_upper50, dim = c(nspp, 2, nyrs,  1))
    quantity_lower50<- array(quantity_lower50, dim = c(nspp, 2, nyrs,  1))
    
    
    
    ## Plot limits
    ymax <- c()
    ymin <- c()
    ind <- 1
    for (sp in 1:nspp) {
      for(sex in 1:nsex[sp]){
        ymax[ind] <- max(c(quantity_upper95[sp,sex, , ], 0), na.rm = T)
        ymin[ind] <- min(c(quantity_lower95[sp,sex, , ], 100), na.rm = T)
        
        ind <- ind+1
      }
    }
    ymax <- ymax * top_adj
    
    
    # Plot trajectory
    loops <- ifelse(is.null(file), 1, 2)
    for (i in 1:loops) {
      if (i == 2) {
        filename <- paste0(file, "_m_trajectory", ".png")
        png(
          file = filename ,
          width = width,# 169 / 25.4,
          height = height,# 150 / 25.4,
          units = "in",
          res = 300
        )
      }
      
      # Plot configuration
      plot_length <- sum(rep(1, length(spp)) * nsex[spp])
      layout(matrix(1:(plot_length + 2), nrow = (plot_length + 2)), heights = c(0.1, rep(1, plot_length), 0.2))
      par(
        mar = c(0, 3 , 0 , 1) ,
        oma = c(0 , 0 , 0 , 0),
        tcl = -0.35,
        mgp = c(1.75, 0.5, 0)
      )
      plot.new()
      
      for (j in 1:length(spp)) {
        for(sex in 1:nsex[spp[j]]){
          plot(
            y = NA,
            x = NA,
            ylim = c(ymin[spp[j]], ymax[spp[j]]),
            xlim = c(endyr, projyr + (endyr - styr) * right_adj),
            xlab = "Year",
            ylab = "M",
            xaxt = c(rep("n", length(spp) - 1), "s")[j]
          )
          
          # Legends
          legend("topleft",
                 legend = spnames[spp[j]],
                 bty = "n",
                 cex = 1)
          
          
          # Credible interval
          for (k in 1:dim(quantity_upper50)[4]) {
            # - 95% CI
            polygon(
              x = c(endyr:projyr, rev(endyr:projyr)),
              y = c(quantity_upper95[spp[j],sex, projyrs_vec, k], rev(quantity_lower95[spp[j],sex,projyrs_vec, k])),
              col = adjustcolor( line_col[k], alpha.f = alpha/2),
              border = NA
            )
            
            # - 50% CI
              polygon(
                x = c(endyr:projyr, rev(endyr:projyr)),
                y = c(quantity_upper50[spp[j],sex, projyrs_vec, k], rev(quantity_lower50[spp[j], sex, projyrs_vec, k])),
                col = adjustcolor( line_col[k], alpha.f = alpha),
                border = NA
              )
          }
          
          # Mean quantity
          for (k in 1:dim(quantity)[4]) {
            lines(
              x = endyr:projyr,
              y = quantity[spp[j],sex, projyrs_vec, k],
              lty = 1,
              lwd = lwd,
              col = line_col[k]
            ) # Median
            
          }
          
          # OM value
          abline(
            h = Rceattle$Sim_1$OM$quantities$M[spp[j],sex,age,1],
            lty = 2,
            lwd = lwd,
            col = 1
          ) # Median
        }
      }
      
      
      if (i == 2) {
        dev.off()
      }
    }
  }