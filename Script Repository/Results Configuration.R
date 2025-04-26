##### North Sea Ecosystem Model Results ####

#### Begin ####
{
  ## Clear workspace ##
  rm(list=ls())
  
  ## Set Working Directory ##
  workdir <- "C:/Users/mwoodsto/Documents/North Sea Ecosim Runs"
  setwd(workdir)
  
  plotwd <- "D:/North Sea Ecosystem Model/1990 Start Time/Plots"
  
  ## Scenario directory ##
  mean_scendir <- paste(workdir,"/Mean Mammals/mc_NS_mean",sep="")
  low_scendir <- paste(workdir,"/Low Mammals/mc_New Ecosim scenario",sep="")
  high_scendir <- paste(workdir,"/High Mammals/mc_High Mammals",sep="")
  SCAN_scendir <- paste(workdir,"/SCAN Mammals/mc_New Ecosim scenario",sep="")
  
  ## Load libraries ##
  packages = c("ncdf4", "RNetCDF","REdaS","ggplot2","R.utils","wesanderson","tidyverse",
               "scales","cowplot","here","broom","ggridges","viridis","forcats","hrbrthemes","svMisc",
               "marmap","rworldmap","sf","ggpubr","colortools","FSA","ggspatial","rcompanion")
  
  package.check <- lapply(packages,FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  
  
  ## Global Attributes ##
  n_sim<- 1000  #Number of iterations
  n_spec<-48 #Number of functional groups
  n_fisher<- 12 #Number of fisheries in the model
  year_start<- 1990 #Model Start Year
  year_end<- 2014  #Model End Year
  n_year <- 25  #Could be different/implemented if you include a spin up to the years of concern
  species <- read.csv("Species.csv")
  fleet <- read.csv("fleets.csv")
}

#### Figure 1 ----
{
  #### Create map ####
  Bathy = getNOAA.bathy(lon1 = -5, lon2 = 10, lat1 = 48, lat2 = 60, 
                        resolution = 1)
  Bathy <- Bathy * -1
  Bathy[Bathy<0] <- NA
  bf = fortify.bathy(Bathy)
  
  autoplot.bathy(Bathy, geom=c("tile")) +
    scale_fill_gradient2(low="dodgerblue", mid="lightskyblue", high="darkblue") +
    labs(y = "Latitude", x = "Longitude", fill = "Depth (m)") + 
    theme(text=element_text(size=16,family="serif",face="bold"),plot.background = element_rect(colour="black",size=1))+
    #xlim(-5,10)+ylim(48,60)+
    theme_bw()+
    annotate(geom = "text", x = 3, y = 55.9, label = "4b", color = "black", size = 6)+
    annotate(geom = "text", x = 3, y = 52.3, label = "4c", color = "black", size = 6)+
    annotate(geom = "text", x = 0, y = 50.2, label = "7d", color = "black", size = 6)+
    geom_segment(aes(x = -1.8, y = 57.5, xend = 7.8, yend = 57.5),colour="black",size=2)+
    geom_segment(aes(x = -1.9, y = 49.7, xend = -1.9, yend = 50.7),colour="black",size=2)+
    geom_segment(aes(x = 1, y = 51, xend = 1.7, yend = 51),colour="black",size=2)+
    geom_segment(aes(x = 0.3, y = 53.2, xend = 6.1, yend = 53.2),colour="black",size=2)+
    geom_segment(aes(x = 7.8, y = 57.5, xend = 8.3, yend = 56.9),colour="black",size=2)+
    annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),style = north_arrow_fancy_orienteering)
  
  setwd(plotwd)
  ggsave("Map.png",last_plot())
}

#### Ecosystem Indicators ----
{
  #* Kempton's Q ----
  cat("\nKempton Sq Calculation\n")
  #** High Scenario ----
  kempton_sq <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(high_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(high_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(high_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(high_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
  }
  
  iter <- iter - 1
  
  kempton_sq <- kempton_sq[,,1:iter]
  
  number <- n_year * iter
  kempton_sq_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(kempton_sq_df) <- c("Set","Year","Value")
  
  count <- 1
  for (set in 1:iter){
    kempton_sq_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    kempton_sq_df$Year[c(count:(count+(n_year-1)))] <- c(kempton_sq[,1,set])
    kempton_sq_df$Value[c(count:(count+(n_year-1)))] <- c(kempton_sq[,2,set])
    count <- count + n_year
  }
  
  summed_high <- groupwiseMedian(Value~Year,data=kempton_sq_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_high <- as.data.frame(cbind(summed_high$Year,summed_high$Median,summed_high$Bca.lower,summed_high$Bca.upper))
  colnames(data_high) <- c("Year","Median","Low","High")
  data_high$Scenario <- c("High Predator")
  

  #** Mean Scenario ----
  kempton_sq <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    
  }
  
  iter <- iter - 1
  
  kempton_sq <- kempton_sq[,,1:iter]
  
  number <- n_year * iter
  kempton_sq_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(kempton_sq_df) <- c("Set","Year","Value")
  
  
  count <- 1
  for (set in 1:iter){
    kempton_sq_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    kempton_sq_df$Year[c(count:(count+(n_year-1)))] <- c(kempton_sq[,1,set])
    kempton_sq_df$Value[c(count:(count+(n_year-1)))] <- c(kempton_sq[,2,set])
    count <- count + n_year
  }
  
  summed_mean <- groupwiseMedian(Value~Year,data=kempton_sq_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_mean <- as.data.frame(cbind(summed_mean$Year,summed_mean$Median,summed_mean$Bca.lower,summed_mean$Bca.upper))
  colnames(data_mean) <- c("Year","Median","Low","High")
  data_mean$Scenario <- c("Mean Predator")
  
  
  #** Low Scenario ----
  kempton_sq <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(low_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(low_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(low_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(low_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    
  }
  
  iter <- iter - 1
  
  kempton_sq <- kempton_sq[,,1:iter]
  
  number <- n_year * iter
  kempton_sq_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(kempton_sq_df) <- c("Set","Year","Value")
  
  
  count <- 1
  for (set in 1:iter){
    kempton_sq_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    kempton_sq_df$Year[c(count:(count+(n_year-1)))] <- c(kempton_sq[,1,set])
    kempton_sq_df$Value[c(count:(count+(n_year-1)))] <- c(kempton_sq[,2,set])
    count <- count + n_year
  }
  
  summed_low <- groupwiseMedian(Value~Year,data=kempton_sq_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_low <- as.data.frame(cbind(summed_low$Year,summed_low$Median,summed_low$Bca.lower,summed_low$Bca.upper))
  colnames(data_low) <- c("Year","Median","Low","High")
  data_low$Scenario <- c("Low Predator")
  
  
  #** SCAN Scenario ----
  kempton_sq <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("kemptonsq_annual.csv",skip=9)
        kempton_sq[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    
  }
  
  iter <- iter - 1
  
  kempton_sq <- kempton_sq[,,1:iter]
  
  number <- n_year * iter
  kempton_sq_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(kempton_sq_df) <- c("Set","Year","Value")
  
  
  count <- 1
  for (set in 1:iter){
    kempton_sq_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    kempton_sq_df$Year[c(count:(count+(n_year-1)))] <- c(kempton_sq[,1,set])
    kempton_sq_df$Value[c(count:(count+(n_year-1)))] <- c(kempton_sq[,2,set])
    count <- count + n_year
  }
  
  summed_SCAN <- groupwiseMedian(Value~Year,data=kempton_sq_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_SCAN <- as.data.frame(cbind(summed_SCAN$Year,summed_SCAN$Median,summed_SCAN$Bca.lower,summed_SCAN$Bca.upper))
  colnames(data_SCAN) <- c("Year","Median","Low","High")
  data_SCAN$Scenario <- c("SCAN Predator")
  
  all_dat <- rbind(data_high,data_mean,data_low,data_SCAN)
  
  all_dat$Scenario <- factor(all_dat$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))
  
  library(colortools)
  cols<-tetradic("blue",plot=F)
  cols[3] <- "orange"
  cols[4] <- "black"
  
  
  fig3.1<-ggplot(all_dat,aes(x=Year,y=Median,colour=Scenario))+
    scale_colour_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
    geom_ribbon(aes(xmin=min(Year),xmax=max(Year),ymin=Low,ymax=High,fill=Scenario),alpha=0.2,linetype="dotted")+
    scale_fill_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
    geom_line(size=1.5)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+theme_classic()+ylab("Kempton's Q")+xlab("")
  
  #* Total Catch ----
  
  #** High Scenario ----
  catch <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(high_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(high_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(high_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(high_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
  }
  
  iter <- iter - 1
  
  catch <- catch[,,1:iter]
  
  number <- n_year * iter
  catch_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(catch_df) <- c("Set","Year","Value")
  
  count <- 1
  for (set in 1:iter){
    catch_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    catch_df$Year[c(count:(count+(n_year-1)))] <- c(catch[,1,set])
    catch_df$Value[c(count:(count+(n_year-1)))] <- as.numeric(c(catch[,2,set]))
    count <- count + n_year
  }
  
  summed_high <- groupwiseMedian(Value~Year,data=catch_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_high <- as.data.frame(cbind(summed_high$Year,summed_high$Median,summed_high$Bca.lower,summed_high$Bca.upper))
  colnames(data_high) <- c("Year","Median","Low","High")
  data_high$Scenario <- c("High Predator")
  
  
  #** Mean Scenario ----
  catch <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    
  }
  
  iter <- iter - 1
  
  catch <- catch[,,1:iter]
  
  number <- n_year * iter
  catch_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(catch_df) <- c("Set","Year","Value")
  
  
  count <- 1
  for (set in 1:iter){
    catch_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    catch_df$Year[c(count:(count+(n_year-1)))] <- c(catch[,1,set])
    catch_df$Value[c(count:(count+(n_year-1)))] <- c(catch[,2,set])
    count <- count + n_year
  }
  
  summed_mean <- groupwiseMedian(Value~Year,data=catch_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_mean <- as.data.frame(cbind(summed_mean$Year,summed_mean$Median,summed_mean$Bca.lower,summed_mean$Bca.upper))
  colnames(data_mean) <- c("Year","Median","Low","High")
  data_mean$Scenario <- c("Mean Predator")
  
  
  #** Low Scenario ----
  catch <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(low_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(low_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(low_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(low_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    
  }
  
  iter <- iter - 1
  
  catch <- catch[,,1:iter]
  
  number <- n_year * iter
  catch_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(catch_df) <- c("Set","Year","Value")
  
  
  count <- 1
  for (set in 1:iter){
    catch_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    catch_df$Year[c(count:(count+(n_year-1)))] <- c(catch[,1,set])
    catch_df$Value[c(count:(count+(n_year-1)))] <- c(catch[,2,set])
    count <- count + n_year
  }
  
  summed_low <- groupwiseMedian(Value~Year,data=catch_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_low <- as.data.frame(cbind(summed_low$Year,summed_low$Median,summed_low$Bca.lower,summed_low$Bca.upper))
  colnames(data_low) <- c("Year","Median","Low","High")
  data_low$Scenario <- c("Low Predator")
  
  
  #** SCAN Scenario ----
  catch <- array(0,dim=c(n_year,2,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
      }
    }
    else if (file_count < 100){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("totalcatch_annual.csv",skip=9)
        catch[,,iter] <- as.matrix(dat)
        iter <- iter + 1
        
      }
    }
    
  }
  
  iter <- iter - 1
  
  catch <- catch[,,1:iter]
  
  number <- n_year * iter
  catch_df <- data.frame(Set <- rep(0,number),
                              Year <- rep(0,number),
                              Value <- rep(0,number))
  colnames(catch_df) <- c("Set","Year","Value")
  
  
  count <- 1
  for (set in 1:iter){
    catch_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    catch_df$Year[c(count:(count+(n_year-1)))] <- c(catch[,1,set])
    catch_df$Value[c(count:(count+(n_year-1)))] <- as.numeric(c(catch[,2,set]))
    count <- count + n_year
  }

  summed_SCAN <- groupwiseMedian(Value~Year,data=catch_df,conf=0.95,percentile = F,R=1000,digits = 3)
  
  data_SCAN <- as.data.frame(cbind(summed_SCAN$Year,summed_SCAN$Median,summed_SCAN$Bca.lower,summed_SCAN$Bca.upper))
  colnames(data_SCAN) <- c("Year","Median","Low","High")
  data_SCAN$Scenario <- c("SCAN Predator")
  
  all_dat <- rbind(data_high,data_mean,data_low,data_SCAN)
  
  all_dat$Scenario <- factor(all_dat$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))
  
  library(colortools)
  cols<-tetradic("blue",plot=F)
  cols[3] <- "orange"
  cols[4] <- "black"
  
  
  fig3.2<-ggplot(all_dat,aes(x=Year,y=Median,colour=Scenario))+
    scale_colour_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
    geom_ribbon(aes(xmin=min(Year),xmax=max(Year),ymin=Low,ymax=High,fill=Scenario),alpha=0.2,linetype="dotted")+
    scale_fill_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
    geom_line(size=1.5)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+theme_classic()+xlab("")+xlim(1990,2014)
  
  #* Cumulative Biomass vs. Trophic Level ----
  
  cat("\nCumB vs TL and CumB vs CumB Calculation\n")
  
  biom <- tl <- mort <- array(0,dim=c(n_year,n_spec,n_sim))
  
  iter <- 1
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        dat <- read.csv("biomass_annual.csv",skip=9)
        dat2 <- read.csv("tl_annual.csv",skip=9)
        dat3 <- read.csv("mortality_annual.csv",skip=9)
        biom[,,iter] <- as.matrix(dat[,-1])
        tl[,,iter] <- as.matrix(dat2[,-1])
        mort[,,iter] <- as.matrix(dat3[,-1])
        iter <- iter + 1
        
      }
    }
    else if (file_count < 100){
      foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        
        dat <- read.csv("biomass_annual.csv",skip=9)
        dat2 <- read.csv("tl_annual.csv",skip=9)
        dat3 <- read.csv("mortality_annual.csv",skip=9)
        biom[,,iter] <- as.matrix(dat[,-1])
        tl[,,iter] <- as.matrix(dat2[,-1])
        mort[,,iter] <- as.matrix(dat3[,-1])
        iter <- iter + 1
      }
    }
    else if (file_count < 1000){
      foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        
        dat <- read.csv("biomass_annual.csv",skip=9)
        dat2 <- read.csv("tl_annual.csv",skip=9)
        dat3 <- read.csv("mortality_annual.csv",skip=9)
        biom[,,iter] <- as.matrix(dat[,-1])
        tl[,,iter] <- as.matrix(dat2[,-1])
        mort[,,iter] <- as.matrix(dat3[,-1])
        iter <- iter + 1
      }
    }
    else if (file_count < 10000){
      foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
        
        
        dat <- read.csv("biomass_annual.csv",skip=9)
        dat2 <- read.csv("tl_annual.csv",skip=9)
        dat3 <- read.csv("mortality_annual.csv",skip=9)
        biom[,,iter] <- as.matrix(dat[,-1])
        tl[,,iter] <- as.matrix(dat2[,-1])
        mort[,,iter] <- as.matrix(dat3[,-1])
        iter <- iter + 1
      }
    }
    
  }
  
  biom <- biom[,,1:(iter-1)]
  tl <- tl[,,1:(iter-1)]
  mort <- mort[,,1:(iter-1)]
  
  pb <- mort*biom
  
  
  tl <- format(round(tl,1),nsmall=1)
  
  cumB <- cumP <- data.frame(rep(0,10000000),rep(0,10000000),rep(0,10000000))
  colnames(cumB) <- c("TL","CumB","Set")
  colnames(cumP) <- c("TL","CumP","Set")
  
  count <- 1
  for (set in 1:iter){
    tl_inc <- 1
    cumulative <- cumulativeP <- 0
    while(tl_inc < 5){
      cumB$TL[count] <- cumP$TL[count] <- tl_inc
      cumB$Set[count] <-  cumP$Set[count] <- set
      
      for (spec in 1:n_spec){
        if (tl[1,spec,set]==format(round(tl_inc,1),nsmall=1)){
          cumulative <- cumulative + biom[1,spec,set]
          cumulativeP <- cumulativeP + pb[1,spec,set]
        }
      } 
      cumB$CumB[count] <- cumulative
      cumP$CumP[count] <- cumulativeP
      count <- count + 1
      tl_inc <- tl_inc + 0.1
    }
  }
  cumB <- cumB[1:(count-1),]
  cumP <- cumP[1:(count-1),]
  
  
  avg_cumB_start <- aggregate(cumB$CumB,by=list(cumB$TL),FUN="mean")
  sd_cumB_start <- aggregate(cumB$CumB,by=list(cumB$TL),FUN="sd")
  
  avg_cumP_start <- aggregate(cumP$CumP,by=list(cumP$TL),FUN="mean")
  sd_cumP_start <- aggregate(cumP$CumP,by=list(cumP$TL),FUN="sd")
  
  dat_start <- cbind(avg_cumB_start,sd_cumB_start,avg_cumP_start,sd_cumP_start)
  colnames(dat_start) <- c("TL","cumB","TL2","SD","TL3","cumP","TL4","SD2")
  
  cumB2 <- cumP2 <- data.frame(rep(0,10000000),rep(0,10000000),rep(0,10000000))
  colnames(cumB2) <- c("TL","CumB","Set")
  colnames(cumP2) <- c("TL","CumP","Set")
  
  count <- 1
  
  for (set in 1:iter){
    tl_inc <- 1
    cumulative <- cumulativeP <- 0
    while(tl_inc < 5){
      cumB2$TL[count] <- cumP2$TL[count] <- tl_inc
      cumB2$Set[count] <- cumP2$Set[count] <- set
      
      for (spec in 1:n_spec){
        if (tl[dim(biom)[1],spec,set]==format(round(tl_inc,1),nsmall=1)){
          cumulative <- cumulative + biom[dim(biom)[1],spec,set]
          cumulativeP <- cumulativeP + pb[dim(pb)[1],spec,set]
        }
      } 
      cumB2$CumB[count] <- cumulative
      cumP2$CumP[count] <- cumulativeP
      count <- count + 1
      tl_inc <- tl_inc + 0.1
    }
  }
  
  cumB2 <- cumB2[1:(count-1),]
  cumP2 <- cumP2[1:(count-1),]
  
  avg_cumB_end <- aggregate(cumB2$CumB,by=list(cumB2$TL),FUN="mean")
  sd_cumB_end <- aggregate(cumB2$CumB,by=list(cumB2$TL),FUN="sd")
  
  avg_cumP_end <- aggregate(cumP2$CumP,by=list(cumP2$TL),FUN="mean")
  sd_cumP_end <- aggregate(cumP2$CumP,by=list(cumP2$TL),FUN="sd")
  
  dat_end <- cbind(avg_cumB_end,sd_cumB_end,avg_cumP_start,sd_cumP_end)
  colnames(dat_end) <- c("TL","cumB","TL2","SD","TL3","cumP","TL4","SD2")
  
  
  all_dat <- data.frame(TL <- c(dat_start$TL,dat_end$TL),
                        cumB <- c(dat_start$cumB,dat_end$cumB),
                        SD <- c(dat_start$SD,dat_end$SD),
                        cumP <- c(dat_start$cumP,dat_end$cumP),
                        SD2 <- c(dat_start$SD2,dat_end$SD2),
                        Year <-c(rep("1990",dim(dat_start)[1]),rep("2014",dim(dat_end)[1])))
  
  colnames(all_dat) <- c("TL","cumB","SD","cumP","SD2","Year")
  
  
  fig3.3<-ggplot(all_dat,aes(x=TL,y=cumB,colour=Year))+scale_colour_manual(values=c("black","red"))+scale_fill_manual(values=c("black","red"))+
    geom_ribbon(aes(ymin=cumB-SD,ymax=cumB+SD,fill=Year,colour=Year),alpha=0.1)+
    geom_line(size=2)+
    theme_classic()+ylab("Cumulative Biomass (t/km2)")
  
  fig3.4 <-ggplot(all_dat,aes(x=cumB,y=cumP,colour=Year))+scale_colour_manual(values=c("black","red"))+scale_fill_manual(values=c("black","red"))+
    geom_ribbon(aes(ymin=cumP-SD2,ymax=cumP+SD2,fill=Year,colour=Year),alpha=0.1)+
    geom_line(size=2)+
    theme_classic()+ylab("Cumulative Production (/year)")+xlab("Cumulative Biomass (t/km2)")
  
  fig3top <- ggarrange(fig3.1,fig3.2,labels=c('A', 'B'),
                       common.legend = T)
  
  fig3bottom <- ggarrange(fig3.3,fig3.4,labels=c('C', 'D'),
                          common.legend = T)
  
  fig3 <-ggarrange(fig3top,fig3bottom,ncol=1,
                   common.legend = F)
  fig3
  
  setwd(plotwd)
  ggsave("Ecosystem Indicators.png",fig3)
}


#### Biomass ----
biom <- array(0,dim=c(n_year,n_spec+1,n_sim))

iter <- 1
for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
      
      dat <- read.csv("biomass_annual.csv",skip=9)
      biom[,,iter] <- as.matrix(dat)
      iter <- iter + 1
    }
  }
  else if (file_count < 100){
    foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
      
      dat <- read.csv("biomass_annual.csv",skip=9)
      biom[,,iter] <- as.matrix(dat)
      iter <- iter + 1
      
    }
  }
  else if (file_count < 1000){
    foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
      
      dat <- read.csv("biomass_annual.csv",skip=9)
      biom[,,iter] <- as.matrix(dat)
      iter <- iter + 1
      
    }
  }
  else if (file_count < 10000){
    foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
      
      dat <- read.csv("biomass_annual.csv",skip=9)
      biom[,,iter] <- as.matrix(dat)
      iter <- iter + 1
      
    }
  }
}

length(biom[is.na(biom)])
iter <- iter - 1

biom <- biom[,,1:iter]

number <- n_year * file_count *n_spec
biom_df <- data.frame(Set <- rep(0,number),
                      Year <- rep(0,number),
                      Species <- rep("",number),
                      Value <- rep(0,number))
colnames(biom_df) <- c("Set","Year","Species","Value")

count <- 1
for (set in 1:file_count){
  for (spec in 1:n_spec){
    biom_df$Set[c(count:(count+(n_year-1)))] <- c(set)
    biom_df$Species[c(count:(count+(n_year-1)))] <- c(species[spec,2])
    biom_df$Year[c(count:(count+(n_year-1)))] <- c(biom[,1,set])
    biom_df$Value[c(count:(count+(n_year-1)))] <- as.numeric(c(biom[,spec+1,set]))
    count <- count + n_year
  }
}

biom_df[biom_df$Value == "NaN",]


avg_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="mean")
sd_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="sd")

data <- cbind(avg_biom,sd_biom)

colnames(data) <- c("Year","Species","Average","Year2","Species2","SD")

head(data)

data %>% filter(Species %in% c("White-beaked dolphin")) %>%
  ggplot(aes(x=Year,y=Average,colour=Species))+
  geom_ribbon(aes(xmin=min(Year),xmax=max(Year),ymin=Average-SD,ymax=Average+SD),alpha=0.2)+
  geom_line(size=2)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+ylab("Biomass t/km2")+theme_classic()

#### Biomass (End/Start) ----
biom <- array(0,dim=c(n_year,n_spec,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("biomass_annual.csv")
  biom[,,file_count] <- as.matrix(dat[,-1])
}


number <- n_sim * n_spec
biom_es <- data.frame(Set <- rep(0,number),
                      Species <- rep("",number),
                      Macro <- rep("",number),
                      Value <- rep(0,number))
colnames(biom_es) <- c("Set","Species","Macrogroup","Value")

count <- 1
for (set in 1:n_sim){
  for (spec in 1:n_spec){
    biom_es$Set[count] <- set
    biom_es$Species[count] <- species[spec,2]
    biom_es$Macrogroup[count] <- species[spec,4]
    biom_es$Value[count] <- biom[n_year,spec,set]/biom[1,spec,set]
    count <- count + 1
  }
}



head(biom_es)
biom_es %>% filter(Macrogroup %in% c("Groundfishes")) %>%
  ggplot()+geom_hline(yintercept=1)+
  geom_boxplot(aes(x=Species,y=Value,colour=Value),outlier.alpha = 0.1)+facet_wrap(.~Macrogroup)+theme_classic()




#### Fisheries Catches and Value (End/Start) Plot ----
{
  ## High Scenario ##
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(high_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(high_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(high_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(high_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    c <- read.csv("landings_annual.csv")
    c[,5] <- c(file_count)
    v <- read.csv("value-fleet-group_annual.csv")
    v[,5] <- c(file_count)
    
    
    if (file_count ==1){
      cat <- array(0,dim=c(dim(c),n_sim))
      val <- array(0,dim=c(dim(v),n_sim))
    }
    cat[,,file_count] <- as.matrix(c)
    val[,,file_count] <- as.matrix(v)
  }
  
  number <- n_fisher * n_sim *2
  res <- data.frame(Fleet <- rep("",number),
                    Value <- rep(0,number),
                    Set <- rep(0,number),
                    Type <- rep("",number))
  
  
  colnames(res) <- c("Fleet","Value","Set","Type")
  
  count <- 1
  for (set in 1:100){
    for (type in 1:2){
      if (type == 1){
        
        catch <- data.frame(cat[,,set])
        colnames(catch) <- c("Year","Fleet","Group","Value","Set")
        
        agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Landings"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      } else {
        value <- data.frame(val[,,set])
        colnames(value) <- c("Year","Fleet","Group","Value","Set")
        
        agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Value"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      }
    }
  }
  res<-res[1:count-1,]
  
  dat_high <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))
  
  dat_high$Scenario <- c("High Predator")
  
  ## Low Scenario ##
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(low_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(low_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(low_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(low_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    c <- read.csv("landings_annual.csv")
    c[,5] <- c(file_count)
    v <- read.csv("value-fleet-group_annual.csv")
    v[,5] <- c(file_count)
    
    
    if (file_count ==1){
      cat <- array(0,dim=c(dim(c),n_sim))
      val <- array(0,dim=c(dim(v),n_sim))
    }
    cat[,,file_count] <- as.matrix(c)
    val[,,file_count] <- as.matrix(v)
  }
  
  number <- n_fisher * n_sim *2
  res <- data.frame(Fleet <- rep("",number),
                    Value <- rep(0,number),
                    Set <- rep(0,number),
                    Type <- rep("",number))
  
  
  colnames(res) <- c("Fleet","Value","Set","Type")
  
  count <- 1
  for (set in 1:100){
    for (type in 1:2){
      if (type == 1){
        
        catch <- data.frame(cat[,,set])
        colnames(catch) <- c("Year","Fleet","Group","Value","Set")
        
        agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Landings"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      } else {
        value <- data.frame(val[,,set])
        colnames(value) <- c("Year","Fleet","Group","Value","Set")
        
        agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Value"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      }
    }
  }
  res<-res[1:count-1,]
  
  dat_low <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))
  
  dat_low$Scenario <- c("Low Predator")
  
  ## Mean Scenario ##
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    c <- read.csv("landings_annual.csv")
    c[,5] <- c(file_count)
    v <- read.csv("value-fleet-group_annual.csv")
    v[,5] <- c(file_count)
    
    
    if (file_count ==1){
      cat <- array(0,dim=c(dim(c),n_sim))
      val <- array(0,dim=c(dim(v),n_sim))
    }
    cat[,,file_count] <- as.matrix(c)
    val[,,file_count] <- as.matrix(v)
  }
  
  number <- n_fisher * n_sim *2
  res <- data.frame(Fleet <- rep("",number),
                    Value <- rep(0,number),
                    Set <- rep(0,number),
                    Type <- rep("",number))
  
  
  colnames(res) <- c("Fleet","Value","Set","Type")
  
  count <- 1
  for (set in 1:100){
    for (type in 1:2){
      if (type == 1){
        
        catch <- data.frame(cat[,,set])
        colnames(catch) <- c("Year","Fleet","Group","Value","Set")
        
        agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Landings"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      } else {
        value <- data.frame(val[,,set])
        colnames(value) <- c("Year","Fleet","Group","Value","Set")
        
        agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Value"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      }
    }
  }
  res<-res[1:count-1,]
  
  dat_mean <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))
  
  dat_mean$Scenario <- c("Mean Predator")
  
  ## SCAN Scenario ##
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    c <- read.csv("landings_annual.csv")
    c[,5] <- c(file_count)
    v <- read.csv("value-fleet-group_annual.csv")
    v[,5] <- c(file_count)
    
    
    if (file_count ==1){
      cat <- array(0,dim=c(dim(c),n_sim))
      val <- array(0,dim=c(dim(v),n_sim))
    }
    cat[,,file_count] <- as.matrix(c)
    val[,,file_count] <- as.matrix(v)
  }
  
  number <- n_fisher * n_sim *2
  res <- data.frame(Fleet <- rep("",number),
                    Value <- rep(0,number),
                    Set <- rep(0,number),
                    Type <- rep("",number))
  
  
  colnames(res) <- c("Fleet","Value","Set","Type")
  
  count <- 1
  for (set in 1:100){
    for (type in 1:2){
      if (type == 1){
        
        catch <- data.frame(cat[,,set])
        colnames(catch) <- c("Year","Fleet","Group","Value","Set")
        
        agg_catch <- aggregate(catch$Value,by=list(catch$Year,catch$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_catch %>% filter(agg_catch$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Landings"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      } else {
        value <- data.frame(val[,,set])
        colnames(value) <- c("Year","Fleet","Group","Value","Set")
        
        agg_val <- aggregate(value$Value,by=list(value$Year,value$Fleet),FUN="sum")
        
        for (fish in 1:n_fisher){
          sub_agg <- agg_val %>% filter(agg_val$Group.2==fish)
          
          res$Set[count] <- set
          res$Type[count] <- "Value"
          res$Fleet[count] <- fleet$Fleet[fish]
          res$Value[count] <- sub_agg$x[n_year]/sub_agg$x[1]
          count <- count + 1
        }
      }
    }
  }
  res<-res[1:count-1,]
  
  dat_SCAN <- merge(res,fleet,by="Fleet") %>% select(c(colnames(res),"Macrogroup"))
  
  dat_SCAN$Scenario <- c("SCAN Predator")
  
  
  
  all_dat <- rbind(dat_high,dat_low,dat_mean,dat_SCAN)
  cols<-tetradic("blue",plot=F)
  cols[3] <- "firebrick"
  cols[2] <- "black"
  cols[4] <- "gold"
  
  head(all_dat)
  
  all_day <- all_dat
  rev(fleet$Fleet)
  all_day$Fleet <- factor(all_day$Fleet,levels=rev(c("Beam Trawl","Demersal Trawl & Seine","Dredge","Nephrops Trawl","Sandeel Trawl","Shrimp Trawls","Drift and Fixed Nets","Hooked Gears","Pots","Shellfish Gear","Pelagic Trawl","Other Gear")))
  all_day$Scenario <- factor(all_day$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))
  
  
  head(all_day)
  
  land <- all_day %>% filter(Type %in% "Landings")
  
  
  aggregate(land$Value,by=list(land$Scenario,land$Fleet),FUN="mean")
  
  
  
  
  fig2<-ggplot(all_day)+
    geom_boxplot(aes(x=Fleet,y=Value,fill=Macrogroup),alpha=0.5,outlier.alpha=0.2)+
    scale_fill_manual(values=c(cols))+
    theme_bw()+
    facet_grid(Scenario~Type)+
    geom_hline(yintercept = 1)+
    coord_flip()+
    ylab("Relative Change (2014/1990)")+
    xlab("")+
    theme(legend.position = "top",legend.title = element_blank(),panel.grid = element_blank(),text=element_text(size=12))+
    ylim(0,3.75)
  
  test <- all_day[all_day$Type=="Landings",]
  test2 <- aggregate(test$Value,by=list(test$Fleet,test$Scenario),FUN="mean")
  
  setwd(plotwd)
  ggsave("Landings and Value Relative Change.png",fig2)
}

#### BMSY and FMSY Plot ----
{
  fmort <- read.csv("D:/North Sea Ecosystem Model/Stock Assessment Trends/Fishing Mortality.csv")
  fmort <- fmort[-26,]
  bench <- read.csv("D:/North Sea Ecosystem Model/Stock Assessment Trends/Current Fishery Benchmarks.csv")
  
  biom <- array(0,dim=c(n_year,n_spec+1,n_sim))
  
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count] <- as.matrix(dat)
  }
  
  number <- n_year * n_sim * n_spec
  biom_df <- data.frame(Set <- rep(0,number),
                        Year <- rep(0,number),
                        Species <- rep("",number),
                        Value <- rep(0,number))
  colnames(biom_df) <- c("Set","Year","Species","Value")
  
  count <- 1
  for (set in 1:n_sim){
    for (spec in 1:n_spec){
      biom_df$Set[c(count:(count+(n_year-1)))] <- c(set)
      biom_df$Species[c(count:(count+(n_year-1)))] <- c(species[spec,2])
      biom_df$Year[c(count:(count+(n_year-1)))] <- c(biom[,1,set])
      biom_df$Value[c(count:(count+(n_year-1)))] <- c(biom[,spec+1,set])
      count <- count + n_year
    }
  }
  
  avg_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="mean")
  
  sd_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="sd")
  colnames(avg_biom) <- c("Year","Species","Average")
  colnames(sd_biom) <- c("Year2","Species2","SD")
  
  data <- cbind(avg_biom,sd_biom) 
  
  ## Cod ##
  dat <- data %>%
    filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
  colnames(cod) <- c("Year","Average","SD")
  cod$Species <- c("Cod")
  
  cod <- cod[,c("Year","Species","Average","SD")]
  
  ## Whiting ##
  dat <- data %>%
    filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
  colnames(whiting) <- c("Year","Average","SD")
  whiting$Species <- c("Whiting")
  whiting <- whiting[,c("Year","Species","Average","SD")]
  
  
  ## Haddock ##
  dat <- data %>%
    filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
  colnames(haddock) <- c("Year","Average","SD")
  haddock$Species <- c("Haddock")
  
  haddock <- haddock[,c("Year","Species","Average","SD")]
  
  ## Herring ##
  dat <- data %>%
    filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
  colnames(herring) <- c("Year","Average","SD")
  herring$Species <- c("Herring")
  
  herring <- herring[,c("Year","Species","Average","SD")]
  
  
  ## Hake ##
  hake <- data %>%
    filter(Species %in% c("Hake")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Turbot ##
  turbot <- data %>%
    filter(Species %in% c("Turbot")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Plaice ##
  plaice <- data %>%
    filter(Species %in% c("Plaice")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Sole ##
  sole <- data %>%
    filter(Species %in% c("Sole")) %>%
    select(c("Year","Species","Average","SD"))
  
  mean_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)
  
  
  for (a in 1:dim(mean_dat)[1]){
    for (year in 1:dim(fmort)[1]){
      for (spec in 2:dim(fmort)[2]){
        if (fmort[year,1]==mean_dat$Year[a] && colnames(fmort)[spec]==mean_dat$Species[a]){
          mean_dat$Fmort[a] <- fmort[year,spec]
        }
      }
    }
  }
  
  for (a in 1:dim(mean_dat)[1]){
    for (spec in 1:dim(bench)[1]){
      if (bench$Species[spec]==mean_dat$Species[a]){
        mean_dat$BMSY[a] <- bench$BMSY[spec]
        mean_dat$FMSY[a] <- bench$F.MSY[spec]
      }
    }
  }
  
  mean_dat$B_BMSY <- mean_dat$Average/mean_dat$BMSY
  mean_dat$F_FMSY <- mean_dat$Fmort/mean_dat$FMSY
  
  mean_dat$Scenario <- c("Mean Predator")
  
  ## Low Scenario ##
  biom <- array(0,dim=c(n_year,n_spec+1,n_sim))
  
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(low_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(low_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(low_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(low_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count] <- as.matrix(dat)
  }
  
  number <- n_year * n_sim * n_spec
  biom_df <- data.frame(Set <- rep(0,number),
                        Year <- rep(0,number),
                        Species <- rep("",number),
                        Value <- rep(0,number))
  colnames(biom_df) <- c("Set","Year","Species","Value")
  
  count <- 1
  for (set in 1:n_sim){
    for (spec in 1:n_spec){
      biom_df$Set[c(count:(count+(n_year-1)))] <- c(set)
      biom_df$Species[c(count:(count+(n_year-1)))] <- c(species[spec,2])
      biom_df$Year[c(count:(count+(n_year-1)))] <- c(biom[,1,set])
      biom_df$Value[c(count:(count+(n_year-1)))] <- c(biom[,spec+1,set])
      count <- count + n_year
    }
  }
  
  
  avg_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="mean")
  sd_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="sd")
  colnames(avg_biom) <- c("Year","Species","Average")
  colnames(sd_biom) <- c("Year2","Species2","SD")
  
  data <- cbind(avg_biom,sd_biom) 
  
  ## Cod ##
  dat <- data %>%
    filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
  colnames(cod) <- c("Year","Average","SD")
  cod$Species <- c("Cod")
  
  cod <- cod[,c("Year","Species","Average","SD")]
  
  ## Whiting ##
  dat <- data %>%
    filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
  colnames(whiting) <- c("Year","Average","SD")
  whiting$Species <- c("Whiting")
  whiting <- whiting[,c("Year","Species","Average","SD")]
  
  
  ## Haddock ##
  dat <- data %>%
    filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
  colnames(haddock) <- c("Year","Average","SD")
  haddock$Species <- c("Haddock")
  
  haddock <- haddock[,c("Year","Species","Average","SD")]
  
  ## Herring ##
  dat <- data %>%
    filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
  colnames(herring) <- c("Year","Average","SD")
  herring$Species <- c("Herring")
  
  herring <- herring[,c("Year","Species","Average","SD")]
  
  
  ## Hake ##
  hake <- data %>%
    filter(Species %in% c("Hake")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Turbot ##
  turbot <- data %>%
    filter(Species %in% c("Turbot")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Plaice ##
  plaice <- data %>%
    filter(Species %in% c("Plaice")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Sole ##
  sole <- data %>%
    filter(Species %in% c("Sole")) %>%
    select(c("Year","Species","Average","SD"))
  
  low_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)
  
  
  for (a in 1:dim(low_dat)[1]){
    for (year in 1:dim(fmort)[1]){
      for (spec in 2:dim(fmort)[2]){
        if (fmort[year,1]==low_dat$Year[a] && colnames(fmort)[spec]==low_dat$Species[a]){
          low_dat$Fmort[a] <- fmort[year,spec]
        }
      }
    }
  }
  
  for (a in 1:dim(low_dat)[1]){
    for (spec in 1:dim(bench)[1]){
      if (bench$Species[spec]==low_dat$Species[a]){
        low_dat$BMSY[a] <- bench$BMSY[spec]
        low_dat$FMSY[a] <- bench$F.MSY[spec]
      }
    }
  }
  
  low_dat$B_BMSY <- low_dat$Average/low_dat$BMSY
  low_dat$F_FMSY <- low_dat$Fmort/low_dat$FMSY
  
  low_dat$Scenario <- c("Low Predator")
  
  
  ## High Scenario ##
  biom <- array(0,dim=c(n_year,n_spec+1,n_sim))
  
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(high_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(high_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(high_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(high_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count] <- as.matrix(dat)
  }
  
  number <- n_year * n_sim * n_spec
  biom_df <- data.frame(Set <- rep(0,number),
                        Year <- rep(0,number),
                        Species <- rep("",number),
                        Value <- rep(0,number))
  colnames(biom_df) <- c("Set","Year","Species","Value")
  
  count <- 1
  for (set in 1:n_sim){
    for (spec in 1:n_spec){
      biom_df$Set[c(count:(count+(n_year-1)))] <- c(set)
      biom_df$Species[c(count:(count+(n_year-1)))] <- c(species[spec,2])
      biom_df$Year[c(count:(count+(n_year-1)))] <- c(biom[,1,set])
      biom_df$Value[c(count:(count+(n_year-1)))] <- c(biom[,spec+1,set])
      count <- count + n_year
    }
  }
  
  avg_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="mean")
  sd_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="sd")
  colnames(avg_biom) <- c("Year","Species","Average")
  colnames(sd_biom) <- c("Year2","Species2","SD")
  
  data <- cbind(avg_biom,sd_biom) 
  
  ## Cod ##
  dat <- data %>%
    filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
  colnames(cod) <- c("Year","Average","SD")
  cod$Species <- c("Cod")
  
  cod <- cod[,c("Year","Species","Average","SD")]
  
  ## Whiting ##
  dat <- data %>%
    filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
  colnames(whiting) <- c("Year","Average","SD")
  whiting$Species <- c("Whiting")
  whiting <- whiting[,c("Year","Species","Average","SD")]
  
  
  ## Haddock ##
  dat <- data %>%
    filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
  colnames(haddock) <- c("Year","Average","SD")
  haddock$Species <- c("Haddock")
  
  haddock <- haddock[,c("Year","Species","Average","SD")]
  
  ## Herring ##
  dat <- data %>%
    filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
  colnames(herring) <- c("Year","Average","SD")
  herring$Species <- c("Herring")
  
  herring <- herring[,c("Year","Species","Average","SD")]
  
  
  ## Hake ##
  hake <- data %>%
    filter(Species %in% c("Hake")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Turbot ##
  turbot <- data %>%
    filter(Species %in% c("Turbot")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Plaice ##
  plaice <- data %>%
    filter(Species %in% c("Plaice")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Sole ##
  sole <- data %>%
    filter(Species %in% c("Sole")) %>%
    select(c("Year","Species","Average","SD"))
  
  high_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)
  high_dat$Scenario <- c("High Predator")
  
  for (a in 1:dim(high_dat)[1]){
    for (year in 1:dim(fmort)[1]){
      for (spec in 2:dim(fmort)[2]){
        if (fmort[year,1]==high_dat$Year[a] && colnames(fmort)[spec]==high_dat$Species[a]){
          high_dat$Fmort[a] <- fmort[year,spec]
        }
      }
    }
  }
  
  for (a in 1:dim(high_dat)[1]){
    for (spec in 1:dim(bench)[1]){
      if (bench$Species[spec]==high_dat$Species[a]){
        high_dat$BMSY[a] <- bench$BMSY[spec]
        high_dat$FMSY[a] <- bench$F.MSY[spec]
      }
    }
  }
  
  high_dat$B_BMSY <- high_dat$Average/high_dat$BMSY
  high_dat$F_FMSY <- high_dat$Fmort/high_dat$FMSY
  
  
  
  ## SCAN Scenario ##
  biom <- array(0,dim=c(n_year,n_spec+1,n_sim))
  
  for (file_count in 1:n_sim){
    if (file_count < 10){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial000",file_count,sep="") 
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 100){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial00",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 1000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial0",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    if (file_count < 10000){
      foldername<-paste0(SCAN_scendir,"/mc_output_trial",file_count,sep="")
      if ((dir.exists(foldername))){
        setwd(foldername)
      }
    }
    
    dat <- read.csv("biomass_annual.csv")
    biom[,,file_count] <- as.matrix(dat)
  }
  
  number <- n_year * n_sim * n_spec
  biom_df <- data.frame(Set <- rep(0,number),
                        Year <- rep(0,number),
                        Species <- rep("",number),
                        Value <- rep(0,number))
  colnames(biom_df) <- c("Set","Year","Species","Value")
  
  count <- 1
  for (set in 1:n_sim){
    for (spec in 1:n_spec){
      biom_df$Set[c(count:(count+(n_year-1)))] <- c(set)
      biom_df$Species[c(count:(count+(n_year-1)))] <- c(species[spec,2])
      biom_df$Year[c(count:(count+(n_year-1)))] <- c(biom[,1,set])
      biom_df$Value[c(count:(count+(n_year-1)))] <- c(biom[,spec+1,set])
      count <- count + n_year
    }
  }
  
  avg_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="mean")
  sd_biom <- aggregate(biom_df$Value,by=list(biom_df$Year,biom_df$Species),FUN="sd")
  colnames(avg_biom) <- c("Year","Species","Average")
  colnames(sd_biom) <- c("Year2","Species2","SD")
  
  data <- cbind(avg_biom,sd_biom) 
  
  ## Cod ##
  dat <- data %>%
    filter(Species %in% c("Cod (Adult)","Cod (Juvenile, 0-2; 0-40 cm")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_cod_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_cod_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  cod <- merge(agg_cod_avg,agg_cod_sd,by="Group.1")
  colnames(cod) <- c("Year","Average","SD")
  cod$Species <- c("Cod")
  
  cod <- cod[,c("Year","Species","Average","SD")]
  
  ## Whiting ##
  dat <- data %>%
    filter(Species %in% c("Whiting (Adult)","Whiting (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_whit_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_whit_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  whiting <- merge(agg_whit_avg,agg_whit_sd,by="Group.1")
  colnames(whiting) <- c("Year","Average","SD")
  whiting$Species <- c("Whiting")
  whiting <- whiting[,c("Year","Species","Average","SD")]
  
  
  ## Haddock ##
  dat <- data %>%
    filter(Species %in% c("Haddock (Adult)","Haddock (Juvenile 0-1; 0-20 cm)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_hadd_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_hadd_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  haddock <- merge(agg_hadd_avg,agg_hadd_sd,by="Group.1")
  colnames(haddock) <- c("Year","Average","SD")
  haddock$Species <- c("Haddock")
  
  haddock <- haddock[,c("Year","Species","Average","SD")]
  
  ## Herring ##
  dat <- data %>%
    filter(Species %in% c("Herring (Adult)","Herring (Juvenile 0-1)")) %>%
    select(c("Year","Species","Average","SD"))
  
  agg_herr_avg <- aggregate(dat$Average,by=list(dat$Year),FUN="sum")
  agg_herr_sd <- aggregate(dat$SD,by=list(dat$Year),FUN="sum")
  
  herring <- merge(agg_herr_avg,agg_herr_sd,by="Group.1")
  colnames(herring) <- c("Year","Average","SD")
  herring$Species <- c("Herring")
  
  herring <- herring[,c("Year","Species","Average","SD")]
  
  
  ## Hake ##
  hake <- data %>%
    filter(Species %in% c("Hake")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Turbot ##
  turbot <- data %>%
    filter(Species %in% c("Turbot")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Plaice ##
  plaice <- data %>%
    filter(Species %in% c("Plaice")) %>%
    select(c("Year","Species","Average","SD"))
  
  ## Sole ##
  sole <- data %>%
    filter(Species %in% c("Sole")) %>%
    select(c("Year","Species","Average","SD"))
  
  SCAN_dat <- rbind(cod,whiting,haddock,herring,hake,turbot,plaice,sole)
  SCAN_dat$Scenario <- c("High Predator")
  
  for (a in 1:dim(SCAN_dat)[1]){
    for (year in 1:dim(fmort)[1]){
      for (spec in 2:dim(fmort)[2]){
        if (fmort[year,1]==SCAN_dat$Year[a] && colnames(fmort)[spec]==SCAN_dat$Species[a]){
          SCAN_dat$Fmort[a] <- fmort[year,spec]
        }
      }
    }
  }
  
  for (a in 1:dim(SCAN_dat)[1]){
    for (spec in 1:dim(bench)[1]){
      if (bench$Species[spec]==SCAN_dat$Species[a]){
        SCAN_dat$BMSY[a] <- bench$BMSY[spec]
        SCAN_dat$FMSY[a] <- bench$F.MSY[spec]
      }
    }
  }
  
  SCAN_dat$B_BMSY <- SCAN_dat$Average/SCAN_dat$BMSY
  SCAN_dat$F_FMSY <- SCAN_dat$Fmort/SCAN_dat$FMSY
  
  SCAN_dat$Scenario <- "SCAN Predator"
  
  
  all_dat <- rbind(mean_dat,low_dat,high_dat,SCAN_dat)
  
  library(colortools)
  cols<-tetradic("blue",plot=F)
  cols[3] <- "firebrick"
  cols[2] <- "black"
  cols[4] <- "gold"
  
  head(all_dat)
  
  all_dat %>% filter(Species %in% "Turbot") %>% filter(Scenario %in% "Mean Predator")
  
  
  
  
  ## Make Plot ##
  fig1 <- ggplot(all_dat)+
    geom_point(aes(x=F_FMSY,y=B_BMSY,shape=Scenario,colour=Scenario,alpha=Year),size=2)+scale_colour_manual(values=c(cols[c(2,1,3,4)]))+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept=1)+theme_classic()+facet_wrap(.~Species,ncol=2)+scale_alpha_continuous(range = c(0.1,1),breaks=c(1990,2000,2010,2014))+theme(text=element_text(size=14,family="serif"),legend.text = element_text(size=12))+ylab("B/BMSY")+xlab("F/FMSY")
  
  setwd(plotwd)
  ggsave("BMSY and FMSY.png",fig1,width=9,height=7,units = c("in"))
}

head(all_dat)
test<-all_dat %>% filter(Scenario %in% "SCAN Predator")

test[test$Species=="Turbot",]

#### TL Catch ----
cat("\nTL Catch Calculation\n")

## High Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(high_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(high_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(high_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(high_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_high <- cbind(avg_tlc,sd_tlc)
colnames(data_high) <- c("Year","Average","Year2","SD")
data_high$Scenario <- c("High Predator")



## Mean Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(mean_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(mean_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(mean_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(mean_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_mean <- cbind(avg_tlc,sd_tlc)
colnames(data_mean) <- c("Year","Average","Year2","SD")
data_mean$Scenario <- c("Mean Predator")



## Low Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(low_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(low_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(low_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(low_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_low <- cbind(avg_tlc,sd_tlc)
colnames(data_low) <- c("Year","Average","Year2","SD")
data_low$Scenario <- c("Low Predator")


## SCAN Scenario ##
tlc <- array(0,dim=c(n_year,2,n_sim))

for (file_count in 1:n_sim){
  if (file_count < 10){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial000",file_count,sep="") 
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 100){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial00",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 1000){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial0",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  if (file_count < 10000){
    foldername<-paste0(SCAN_scendir,"/mc_output_trial",file_count,sep="")
    if ((dir.exists(foldername))){
      setwd(foldername)
    }
  }
  
  dat <- read.csv("tlc_annual.csv")
  tlc[,,file_count] <- as.matrix(dat)
}
number <- n_year * n_sim
tlc_df <- data.frame(Set <- rep(0,number),
                     Year <- rep(0,number),
                     Value <- rep(0,number))
colnames(tlc_df) <- c("Set","Year","Value")

count <- 1
set <-1
for (set in 1:n_sim){
  tlc_df$Set[c(count:(count+(n_year-1)))] <- c(set)
  tlc_df$Year[c(count:(count+(n_year-1)))] <- c(tlc[,1,set])
  tlc_df$Value[c(count:(count+(n_year-1)))] <- c(tlc[,2,set])
  count <- count + n_year
}

avg_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="mean")
sd_tlc <- aggregate(tlc_df$Value,by=list(tlc_df$Year),FUN="sd")

data_SCAN <- cbind(avg_tlc,sd_tlc)
colnames(data_SCAN) <- c("Year","Average","Year2","SD")
data_SCAN$Scenario <- c("SCAN Predator")

all_dat <-rbind(data_high,data_mean,data_low,data_SCAN)
all_dat$Scenario <- factor(all_dat$Scenario,levels=rev(c("SCAN Predator","Low Predator","Mean Predator","High Predator")))


fig3.2<-ggplot(all_dat,aes(x=Year,y=Average,colour=Scenario))+scale_colour_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  geom_ribbon(aes(xmin=min(Year),xmax=max(Year),ymin=Average-SD,ymax=Average+SD,fill=Scenario),alpha=0.2,linetype="dotted")+scale_fill_manual(values=c(cols[1],cols[4],cols[3],cols[2]))+
  geom_line(size=1.5)+theme(text = element_text(size=16,family="serif"),axis.text = element_text(size=16,family="serif"))+theme_classic()+ylab("TL Catch")+xlab("")+ylim(3,4.5)


