
#' Landings fits
#'
#' Plot of fitted landings data on natural-scale (r4ss-style)
#'
#' @param file name of a file to identified the files exported by the
#'   function.
#' @param Rceattle Single or list of Rceattle model objects exported from \code{\link{Rceattle}}
#' @param single.plots if TRUE plot invidual fits else make multiplot
#' @param model_names Names of models to be used in legend
#' @param line_col Colors of models to be used for line color
#' @param species Species names for legend
#' @param right_adj How much right side of the x-axis for fitting the legend. As percentage.
#' @param top_adj How much top side of the y-axis for fitting the legend. As percentage (default = 1.2).
#' @param incl_proj TRUE/FALSE include projections years
#' @param width plot width
#' @param height plot hight
#' @param mse Is if an MSE object from \code{\link{load_mse}} or \code{\link{mse_run}}. Will plot data from OMs.
#' @export

plot_catch_mse_proj <- function(Rceattle,
                                file = NULL,
                                model_names = NULL,
                                line_col = NULL,
                                species = NULL,
                                right_adj = 0,
                                top_adj = 1.2,
                                incl_proj = FALSE,
                                single.plots=FALSE,
                                width=NULL,
                                height=NULL,
                                lwd = 1,
                                ymax = NULL,
                                maxyr = NULL,
                                mse = FALSE){
  
  # Species names
  if(is.null(species)){
    species =  Rceattle[[1]][[1]]$data_list$spnames
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
  ProjYears <- lapply(Rceattle[[1]], function(x) (x$data_list$srr_meanyr+1):x$data_list$projyr)
  Endyrs <- lapply(Rceattle[[1]], function(x) x$data_list$endyr)
  meanyrs <- lapply(Rceattle[[1]], function(x) x$data_list$srr_meanyr)
  fsh_list <- list()
  fsh_hat_list <- list()
  proj_fsh_hat_list <- list()
  
  for(i in 1:length(Rceattle)){
    
    fsh_list[[i]] <- list()
    fsh_hat_list[[i]] <- list()
    proj_fsh_hat_list[[i]] <- list()
    
    for(j in 1:length(Rceattle[[i]])){
      # Get observed
      fsh_list[[i]][[j]] <- Rceattle[[i]][[j]]$data_list$fsh_biom[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% Years[[i]] ),]
      fsh_list[[i]][[j]]$Log_sd <- Rceattle[[i]][[j]]$quantities$fsh_log_sd_hat[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% Years[[i]] )]
      
      no_zero <- which(fsh_list[[i]][[j]]$Catch > 0)
      fsh_list[[i]][[j]]$Lower95 <- 0
      fsh_list[[i]][[j]]$Upper95 <- 0
      fsh_list[[i]][[j]]$Upper95[no_zero]  <- qlnorm(0.975, meanlog = log(fsh_list[[i]][[j]]$Catch[no_zero]), sdlog = fsh_list[[i]][[j]]$Log_sd[no_zero])
      fsh_list[[i]][[j]]$Lower95[no_zero]  <- qlnorm(0.025, meanlog = log(fsh_list[[i]][[j]]$Catch[no_zero]), sdlog = fsh_list[[i]][[j]]$Log_sd[no_zero])
      
      
      # Get estimated
      fsh_hat_list[[i]][[j]] <- Rceattle[[i]][[j]]$data_list$fsh_biom[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% Years[[i]] ),]
      fsh_hat_list[[i]][[j]]$Catch <- Rceattle[[i]][[j]]$quantities$fsh_bio_hat[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% Years[[i]] )]
      fsh_hat_list[[i]][[j]]$Log_sd <- Rceattle[[i]][[j]]$quantities$fsh_log_sd_hat[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% Years[[i]] )]
      
      # Porjected
      proj_fsh_hat_list[[i]][[j]] <- Rceattle[[i]][[j]]$data_list$fsh_biom[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% ProjYears[[i]] ),]
      proj_fsh_hat_list[[i]][[j]]$Catch <- Rceattle[[i]][[j]]$quantities$fsh_bio_hat[which(Rceattle[[i]][[j]]$data_list$fsh_biom$Year %in% ProjYears[[i]] )]
    }
  }
  
  
  # - MSE objects
  mse_list <- list()
  for(i in 1:length(Rceattle)){
    mse_list[[i]] <- fsh_hat_list[[i]][[1]]
    # -- Get quantiles and mean across simulations
    catch_list_tmp <- simplify2array(lapply(fsh_hat_list[[i]], function(x) x$Catch))
    mse_list[[i]]$Upper95 <- apply( catch_list_tmp, 1, function(x) quantile(x, probs = 0.975) )
    mse_list[[i]]$Lower95 <- apply( catch_list_tmp, 1, function(x) quantile(x, probs = 0.025) )
    mse_list[[i]]$Upper50 <- apply( catch_list_tmp, 1, function(x) quantile(x, probs = 0.75) )
    mse_list[[i]]$Lower50 <- apply( catch_list_tmp, 1, function(x) quantile(x, probs = 0.25) )
    mse_list[[i]]$Catch <- apply( catch_list_tmp, 1, function(x) mean(x) ) # Get mean quantity
  }
  
  # Plot
  minyr <- min(unlist(Years), na.rm = TRUE)
  if(is.null(maxyr)){maxyr <- max((sapply(Years, max)))}
  nyrs_vec <- sapply(Years, length)
  nyrs <- max(nyrs_vec)
  
  nspp <- Rceattle[[1]][[1]]$data_list$nspp
  
  
  # Fleet characteristics
  fleet_control <- (Rceattle[[1]][[1]]$data_list$fleet_control)
  fsh_biom <- (Rceattle[[1]][[1]]$data_list$fsh_biom)
  flts <- sort(unique(fsh_biom$Fleet_code))
  nflts <- length(flts)
  
  
  # # Median catch across projection for MSE
  # median_catch <- data.frame(Fleet = flts, Median = rep(0, nflts))
  # 
  # for(i in 1:nflts){
  #   flt = flts[i]
  #   
  #   # - Mean catch by fleet
  #   median_catch$Median[i] <- median(unlist(sapply(proj_fsh_hat_list, function(x)
  #     x$Catch[which(x$Fleet_code == flt)])), na.rm = TRUE)
  # }
  
  
  # Axis
  if(is.null(ymax)){
    ymax <- c()
    
    for(fsh in 1:nflts){
      for(i in 1:length(Rceattle)){
        fsh_ind <- which(fsh_list[[i]][[j]]$Fleet_code == flts[fsh])
        ymax[fsh] <- max(c(fsh_list[[i]][[j]]$Upper95[fsh_ind], fsh_hat_list[[i]][[j]]$Catch[fsh_ind], ymax[fsh]), na.rm = T)
      }
    }
    ymax <- top_adj * ymax
  }
  
  # Assume colors if not provided
  if (is.null(line_col)) {
    line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
  }
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (j in 1:loops) {
    
    # # Plot/save each survey individually
    # if(single.plots==TRUE){
    #   if(is.null(width)) width = 5
    #   if(is.null(height)) height = 3.5
    #   for(fsh in 1:nflts){
    #     Par = list(mfrow=c(1,1), mar = c(3.5, 3.5, 0.5, 0.1), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
    #     
    #     # Save
    #     if(j == 2){
    #       filename <- paste0(file, "fleet",flts[j]," ",as.character(fleet_control$Fleet_name[flts[fsh]]), "_fishery_catch", ".png")
    #       png(file = filename, width = width, height = height, res = 200, units = "in")
    #     }
    #     
    #     par(Par)
    #     plot(NA, NA, ylab="Catch", xlab="Year", ylim = c(0, (ymax[fsh])), xlim = c(minyr, maxyr + (maxyr - minyr) * right_adj), type='n', xaxt="n", yaxt="n")
    #     axis(1,labels=TRUE,cex=0.8)
    #     axis(2,labels=TRUE,cex=0.8)
    #     
    #     # Loop through models
    #     for (k in 1:nmods) {
    #       
    #       # Subset data by fleet and model
    #       fsh_tmp <- fsh_list[[k]] %>%
    #         filter(Fleet_code == flts[fsh])
    #       
    #       if(mse){
    #         fsh_tmp <- fsh_tmp %>% filter(Year <= meanyrs[k]) # Only show historical catch if MSE models
    #       }
    #       
    #       fsh_hat_tmp <- fsh_hat_list[[k]] %>%
    #         filter(Fleet_code == flts[fsh])
    #       
    #       
    #       # - Plot predicted catch
    #       lines(fsh_hat_tmp$Year, (fsh_hat_tmp$Catch),lwd=lwd,col=line_col[k])
    #       
    #       
    #       # - Plot MSE shading
    #       if(mse){
    #         fsh_hat_tmp <- fsh_hat_list[[k]] %>%
    #           filter(Year > meanyrs[k] & Fleet_code == flts[fsh])
    #         
    #         # 95% CI
    #         polygon(
    #           x = c(fsh_hat_tmp$Year, rev(fsh_hat_tmp$Year)),
    #           y = c(fsh_hat_tmp$Upper95, rev(fsh_hat_tmp$Lower95)),
    #           col = adjustcolor( line_col[k], alpha.f = alpha),
    #           border = NA
    #         )
    #         
    #         # 50% CI
    #         polygon(
    #           x = c(fsh_hat_tmp$Year, rev(fsh_hat_tmp$Year)),
    #           y = c(fsh_hat_tmp$Upper50, rev(fsh_hat_tmp$Lower50)),
    #           col = adjustcolor( line_col[k], alpha.f = alpha),
    #           border = NA
    #         )
    #         
    #       }
    #     }
    #     
    #     # Index name
    #     legend('topleft',as.character(fleet_control$Fleet_name[flts[fsh]]),bty="n",y.intersp = -0.2,cex=0.8)
    #     
    #     # Model names
    #     if(!is.null(model_names)){
    #       legend(
    #         "topright",
    #         legend = model_names,
    #         pch = rep(16, length(line_col)), cex=0.8,
    #         col = line_col,
    #         bty = "n"
    #       )
    #       
    #     }
    #     # Save plot
    #     if(j == 2){dev.off()}
    #   }
    # }
    
    #---------------------------------------------
    # Plot/save each survey together
    #---------------------------------------------
    if(single.plots==FALSE){
      
      # Set heights of plot
      if(is.null(width)) width = 7
      if(is.null(height)) height = ifelse(nflts==1,5,ifelse(nflts==2,3.,2.5))*round(nflts/2+0.01,0)
      
      
      Par = list(mfrow=c(ifelse(nflts == 1, 1, round(nflts/3+0.01,0)),ifelse(nflts==1,1,3)),
                 mai=c(0.35,0.15,0,.15),
                 omi = c(0.2,0.25,0.2,0) + 0.1,
                 mgp=c(2,0.5,0), 
                 tck = -0.02,cex=0.8)
      
      # Save
      if(j == 2){
        filename <- paste0(file,"_fishery_catch", ".png")
        png(file = filename, width = width, height = height, res = 200, units = "in")
      }
      par(Par)
      
      
      for(fsh in 1:nflts){
        
        xlim <- c(minyr, maxyr)
        if(fsh == 1){
          xlim <- c(minyr, maxyr + (maxyr - minyr) * right_adj)
        }
        
        plot(NA, NA, ylab="", xlab="", ylim = c(0, (ymax[fsh])), xlim = xlim, type='n', xaxt="n", yaxt="n")
        axis(1,labels=TRUE,cex=0.8)
        axis(2,labels=TRUE,cex=0.8)
        
        # Index name
        legend('topleft',as.character(fleet_control$Fleet_name[flts[fsh]]),bty="n",y.intersp = -0.2,cex=0.8)
        
        # Model names
        if(fsh == 1){
          if(!is.null(model_names)){
            legend(
              "topright",
              legend = model_names,
              pch = rep(16, length(line_col)), cex=0.8,
              col = line_col,
              bty = "n"
            )
          }
        }
        
        # Loop through models
        for (k in 1:length(Rceattle)) {
          
          # Subset data by fleet and model
          fsh_tmp <- fsh_list[[k]][[1]] %>%
            filter(Fleet_code == flts[fsh]) %>% 
            filter(Year <= meanyrs[[k]][1]) # Only show historical catch if MSE models
          
          # - Plot predicted CPUE
          lines(fsh_tmp$Year, (fsh_tmp$Catch), lwd=2, col = 1)
          
          
          # - Plot MSE shading
          fsh_hat_tmp <- mse_list[[k]] %>%
            filter(Year > meanyrs[[k]][1] & Fleet_code == flts[fsh])
          
          # 95% CI
          polygon(
            x = c(fsh_hat_tmp$Year, rev(fsh_hat_tmp$Year)),
            y = c(fsh_hat_tmp$Upper95, rev(fsh_hat_tmp$Lower95)),
            col = line_col[k],
            border = NA
          )
          
          # # 50% CI
          # polygon(
          #   x = c(fsh_hat_tmp$Year, rev(fsh_hat_tmp$Year)),
          #   y = c(fsh_hat_tmp$Upper50, rev(fsh_hat_tmp$Lower50)),
          #   col = adjustcolor( line_col[k], alpha.f = 0.6),
          #   border = NA
          # )
          
          # Horizontal median line of projection
          # abline(h = median_catch$Median[fsh], lty = 2, lwd = 2)
          
        }
      }
      mtext(paste("Year"), side=1, outer=TRUE, at=0.5,line=1,cex=1)
      mtext(paste("Catch"), side=2, outer=TRUE, at=0.5,line=1,cex=1)
      if(j == 2){dev.off()}
    }
  }
} # End of fit






#' plot_ssb
#'
#' @description Function the plots the mean ssb and 95% CI trends as estimated from Rceattle
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
#' @param save Save ssb?
#' @param incl_proj TRUE/FALSE, include projection years
#' @param mod_cex Cex of text for model name legend
#' @param mod_avg Vector of length Rceattle denoting if it is a model average object
#' @param mse Is if an MSE object from \code{\link{load_mse}} or \code{\link{mse_run}}
#' @param OM if mse == TRUE, use the OM (TRUE) or EM (FALSE) for plotting?
#'
#' @export
#'
#' @return Returns and saves a figure with the population trajectory.
plot_ssb_mse_proj <- function(Rceattle,
                              file = NULL,
                              model_names = NULL,
                              line_col = NULL,
                              species = NULL,
                              spnames = NULL,
                              add_ci = FALSE,
                              lwd = 3,
                              rec = FALSE,
                              save = FALSE,
                              right_adj = 0,
                              width = 7,
                              height = 6.5,
                              minyr = 1977,
                              maxyr = 2100,
                              incl_proj = FALSE,
                              mod_cex = 1,
                              lty = rep(1, length(Rceattle)),
                              alpha = 0.4,
                              mod_avg = rep(FALSE, length(Rceattle)),
                              mse = FALSE,
                              OM = TRUE,
                              ymax = NULL,
                              reference = NULL) {
  
  
  # Species names
  spnames =  Rceattle[[1]][[1]]$data_list$spnames
  
  
  
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
  nyrs <- length(Years[[1]])
  
  
  
  minage <- Rceattle[[1]][[1]]$data_list$minage
  
  if(is.null(species)){
    species <- 1:nspp
  }
  spp <- species 
  nspp <- length(species)
  
  
  # Get depletion
  quantity <- list()
  
  for (i in 1:length(Rceattle)) {
    quantity[[i]] <- array(NA, dim = c(nspp, nyrs,  length(Rceattle[[i]])))
    for(j in 1:length(Rceattle[[i]])){
      if(rec){
        quantity[[i]][, 1:nyrs , j] <- Rceattle[[i]][[j]]$quantities$R[,1:nyrs] / 1000000
      } else{
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
    if(!mse){
      line_col <- rev(oce::oce.colorsViridis(length(Rceattle)))
    }
    if(mse){
      line_col <- 1
    }
  }
  if(!is.null(reference)){
    line_col <- c(line_col, 1)
  }
  
  
  # Plot trajectory
  loops <- ifelse(is.null(file), 1, 2)
  for (i in 1:loops) {
    if (i == 2) {
      filename <- paste0(file, ifelse(rec, "_recruitment_trajectory", "_ssb_trajectory"), ".png")
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
      
      if(j == 2 & !rec){
        mtext("Spawning stock biomass (million mt)", side = 2, line = 1.7, cex = 0.9)
      }      
      if(j == 2 & rec){
        mtext("Age-1 recruits (million)", side = 2, line = 1.7, cex = 0.9)
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
          x = c(Years[[k]], rev(Years[[k]])),
          y = c(quantity_upper95[[k]][spp[j], 1:nyrs], rev(quantity_lower95[[k]][spp[j], 1:nyrs])),
          col = adjustcolor( line_col[k], alpha.f = 0.8),
          border = NA
        )
        
        lines(
          x = Years[[k]],
          y = quantity_med[[k]][spp[j],1:nyrs],
          col = line_col[k],
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


