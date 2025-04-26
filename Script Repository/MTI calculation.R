#**************************************************************************
#**** File: Calculate Mixed Trophic Impact From Ecosystem Model Output ****
#****   Project: Ecosystem Modelling of the Oceanic Gulf of Mexico     ****
#****                    Developer: Matt Woodstock                     ****
#**************************************************************************

## Citation ##
#* Woodstock, M.S., T.T. Sutton, T. Frank, Y. Zhang. (2021). 
#*   An early warning sign: trophic structure changes in the 
#*   oceanic Gulf of Mexico from 2011-2018. Ecological Modelling.

## Contact Email ##
#* fishesofthedeep@gmail.com - Matt Woodstock


## Notes ##
#* Most of this has to do with taking Ecopath with Ecosim model output and calculating MTI (Part 1)
#* This methodology could be used for any ecosystem where you have both diet and predation mortality matrices (Part 2)

## Clear Working Directory
rm(list=ls())

## Load Packages ##
packages = c("MASS","dplyr","tidyr")

package.check <- lapply(packages,FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


library(PBSmodelling)
library(snowfall)
library(parallel)
library(snow)
library(foreach)
library(doSNOW)
library(PBSadmb)
library(tidyr)
library(janitor)
library(dplyr)

#Set working directory#
workdir <- "D:/North Sea Ecosystem Model/1990 Start Time"
setwd(workdir)
#* For EwE output, @indir is your mc_Ecosimscenario directory
indir<- "D:/North Sea Ecosystem Model/1990 Start Time/North Sea Ecosim Runs/Mean Mammals/mc_NS_mean"

outdir <-"D:/North Sea Ecosystem Model/1990 Start Time/Plots/MTI Results" #*N files created = N iterations:: Best to make a separate directory

## Load Species names ##
## You need to create these files ##
#* I provide an example. Take note of the formatting issues with importing special characters in R
species <- read.csv("Species.csv")

#species<-as.matrix(read.csv("FILE NAME.csv",header=F)) #List of functional groups in the model
#species_adj<-as.matrix(read.csv("FILE NAME.csv",header=F)) #Functional groups spelled as R will input them (i.e., special characters become periods)

## Global Parameters ##
n_spec<-length(species[,2]) #Number of functional groups
n_fisher<- 12 #Number of fisheries in the model
year_start<- 1990 #Model Start Year
year_end<- 2014  #Model End Year
n_year<- 25 #Number of years of the simulation
n_year_concern <- 25  #Could be different/implemented if you include a spin up to the years of concern
  
  
  #### Part 1:  Gather Ecopath with Ecosim Output and Calculate MTI For Each Year and Iteration####
#* If you already have a diet matrix, skip this

setwd(indir)
files <- list.files()

MTI<-matrix(0,ncol=n_spec+n_fisher,nrow=n_spec+n_fisher) #* Actual result that includes indirect impacts
##Load Information##

# run the simulations section

#set up parallel 
no_cores <- detectCores()  #determine number of cores on computer
cl<-makeCluster(no_cores)  #setup the size of your parallel cluster based on number of cores
registerDoSNOW(cl)         #register cluster to get it ready for parallel processing

#set up text progress bar....this is personal code just to have a progress bar letting your know % completion
pb <- txtProgressBar(max = n_distinct(files), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

ls=foreach(file_count=1:n_distinct(files),.options.snow = opts,.combine='rbind',.packages =c('PBSmodelling')) %dopar% {
  
  if (files[file_count] != "mc_input"){
    setwd(paste(indir,"/",files[file_count],sep=""))

    #Input diet composition#
    diet_array<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern))
    for (a in 1:length(species[,2])){
      filename<-paste("prey_",species[a,2],"_annual.csv",sep="")
      if (file.exists(filename)){
        diet<-read.csv(filename,header=T) #Necessary data always starts on row 10
        prey_taxa<-colnames(diet) #Collected prey items of predator
        for (b in 1:length(diet[,1])){
          for (c in 1:length(species[,2])){
            for (d in 2:length(diet[1,])){
              if (species[c,3]==prey_taxa[d]){
                diet_array[c,a,b]<-diet[b,d] #You are creating a diet matrix by skipping all non-preys
              }
            }
          }
        }
      }
    }

      filename<-"catch-fleet-group_annual.csv" #Fishery Catches
      fish_contrib<-array(0,dim=c((n_spec+n_fisher),n_fisher,n_year_concern))
      #** If you have alot of fisheries or a long model, create a subset to reduce run time **
      if (file.exists(filename)){
        fishery<-read.csv(filename,header=T)
        for (y in year_start:year_end){ #Input years your model starts and ends
          for (x in 1:n_fisher){
            sub_catch <- subset(fishery,fishery$year==y & fishery$fleet==x)
            for (z in 1:length(sub_catch[,1])){
              for (w in 1:n_spec){
                if (sub_catch[z,1]==y && sub_catch[z,2]==x && sub_catch[z,3]==w){
                  fish_contrib[w,x,y-(year_start-1)]<-sub_catch[z,4]
                }
              }
            }
          }
        }
      }
    
    ## Calculate "Diet Composition" for fishery and append to diet_array
        for (y in year_start:year_end){
          for (x in 1:n_fisher){
                diet_array[,(n_spec+x),y-(year_start-1)]<-fish_contrib[,x,y-(year_start-1)]/sum(fish_contrib[,x,y-(year_start-1)]) #Calculate proporitional contribution of a species to fishery
          }
        }
    
    # Need biomass and Q/B
    biomass <- read.csv("biomass_annual.csv")
    qb <- read.csv("consumption-biomass_annual.csv")
    
    #* First columns are Year. The dimensions should be = [n_year,n_spec]
    biomass <- biomass[,-1] 
    qb <- qb[,-1]
    
    #* Calculate consumption
    bqb <- biomass * qb
    
    Prodij <- array(0,dim=dim(diet_array))
    Prodi <- matrix(0,nrow = dim(diet_array)[1],ncol=dim(diet_array)[3])
    Predcontij <- array(0,dim=dim(diet_array))
    Predcontji <- array(0,dim=dim(diet_array))
    direct <- array(0,dim=dim(diet_array))
    
    for (year in 1:n_year_concern){
      Prodij[,,year] <- diet_array[,,year] * bqb[col(as.matrix(diet_array[,,year]))]
      
      #* Replace final diet row with total fishery contribution
      #* This step is required because we previously converted the landings+discards into a percent
      #* 
        Prodij[,((n_spec+1):(n_spec+n_fisher)),year]<-fish_contrib[,,year]
        
      #* Net Production
      Prodi[,year] <- rowSums(Prodij[,,year])
      Prodi[,1]
      for(spec in 1:dim(Prodij)[1]){
        Predcontij[spec,,year] <- Prodij[spec,,year] / Prodi[spec,year]
      }
      
      Predcontij[,,year][is.na(Predcontij[,,year])] <- 0
      
      Predcontji[,,year] <- t(Predcontij[,,year])
      Predcontji[is.na(Predcontji)] <- 0
      
      #* Direct MTI
      direct[,,year] <- diet_array[,,year] - Predcontji[,,year]
    }
    #************************************************************************
    #*
    setwd(outdir)
    #* If you are not interested in having excel files, comment those lines out
    #* Will significantly increase run time, but may be wise to have in case the loop cannot finish for any reason
    for (year in 1:n_year_concern){
      filename<-paste(files[file_count]," Year ", year, ".csv",sep="")
      identity.matrix <- diag(ncol(direct[,,year]))
      MTI <- MASS::ginv(identity.matrix - direct[,,year]) - identity.matrix #Calculation of total MTI for each interaction
      write.csv(MTI,filename)
    }
  }
  setTxtProgressBar(pb,file_count)
}


# end parallel job
close(pb)
stopCluster(cl) #end the cluster for parallel processing
closeAllConnections() 

#****************************Done****************************************
#*
#### Assess MTI ####
setwd("D:/North Sea Ecosystem Model/1990 Start Time/Plots/MTI Results")


files <- list.files()

all_dat <- array(NA,dim=c((n_spec+n_fisher),(n_spec+n_fisher),n_year,1000))

pb <- txtProgressBar(min=0,max=n_distinct(files),style=3)
for (a in 1:n_distinct(files)){
  dat <- read.csv(files[a])
  dat <- dat[,-1]
  
  name <- unlist(strsplit(files[a],split = "mc_output_trial"))
  name2 <- unlist(strsplit(name,split = " Year "))
  name3 <- unlist(strsplit(name2,split = ".csv"))
  
  year <- as.numeric(name3)[2]
  iter <- as.numeric(name3)[1]
  
  all_dat[,,year,iter] <- as.matrix(dat)
  
  setTxtProgressBar(pb,a)
}

p<-matrix(0,nrow=n_spec+n_fisher,ncol=n_spec+n_fisher) #Matrix to store p-values

for (spec1 in 1:dim(all_dat)[1]){
  for (spec2 in 1:dim(all_dat)[2]){
    sub <- matrix(0,ncol=2,nrow=25000)
    
    sub[,1] <- all_dat[spec1,spec2,,]
    sub[,2] <- rep(seq(1:25),1000)
    colnames(sub) <- c("MTI","Year")

    sub <-as.data.frame(sub)

    avg <- sub %>% group_by(Year) %>% 
      summarise(mean(c(na.omit(MTI)))) %>% 
      rename(Average = `mean(c(na.omit(MTI)))`)
    
    mod1<-lm(avg$Average~avg$Year) #Development of linear model
    summary(mod1)
    p[spec2,spec1]<-round(summary(mod1)$coefficients[2,4],4)
  }
}
setwd("D:/North Sea Ecosystem Model/1990 Start Time/Plots/")

write.csv(p,"MTI P Values.csv") #Export p-value matrix


final <- data.frame(Predator = rep("",(n_spec+n_fisher)*n_year*dim(all_dat)[4]),
                    Prey = rep("",(n_spec+n_fisher)*n_year*dim(all_dat)[4]),
                    Year = rep(0,(n_spec+n_fisher)*n_year*dim(all_dat)[4]),
                    MTI  = rep(0,(n_spec+n_fisher)*n_year*dim(all_dat)[4]))

count <- 1
for (a in 1:dim(all_dat)[1]){
  for (b in 1:dim(all_dat)[2]){
    for (c in 1:dim(all_dat)[3]){
      if (c == 1 || c == 25){
        final$Predator[count] <- a
        final$Prey[count] <- b
        final$Year[count] <- c
        final$MTI[count] <- mean(c(na.omit(all_dat[b,a,c,])))
        count <- count + 1
      }
    }
  }
}

year_one <- final %>% filter(Year %in% 1)
year_last <- final %>% filter(Year %in% 25)
library(ggplot2)

year_one$Predator <- as.factor(year_one$Predator)
year_one$Prey <- as.factor(year_one$Prey)

one_mti <-ggplot(year_one,aes(x=reorder(Prey,seq(1:3600)),y=reorder(Predator,-seq(1:3600)),fill=MTI))+geom_tile()+
  scale_fill_gradientn(colours=c("red","white","blue"),limits=c(-1,1),labels=c("Negative","Neutral","Positive"),breaks=c(-1,0,1))+scale_x_discrete(position="top")+
  xlab("")+ylab("")+ 
  #geom_text(aes(label=Sig),vjust=0.75)+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=8),
        legend.position = "right")+
  geom_vline(xintercept=48.5)+
  geom_hline(yintercept = 12.5)+
  geom_vline(xintercept=9.5)+
  geom_hline(yintercept = 51.5)

setwd('D:/North Sea Ecosystem Model/1990 Start Time/Plots')
ggsave("MTI Year One.png",last_plot(),height=10,width=12,units = "in")

diff <- year_one
diff$MTI <- year_last$MTI-year_one$MTI

diff_mti <- ggplot(diff,aes(x=reorder(Prey,seq(1:3600)),y=reorder(Predator,-seq(1:3600)),fill=MTI))+geom_tile()+
  scale_fill_gradientn(colours=c("red","white","blue"),limits=c(-0.3,0.3),labels=c("Negative","Neutral","Positive"),breaks=c(-0.3,0,0.3))+scale_x_discrete(position="top")+
  xlab("")+ylab("")+ 
  #geom_text(aes(label=Sig),vjust=0.75)+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=8),
        legend.position = "right")+
  geom_vline(xintercept=48.5)+
  geom_hline(yintercept = 12.5)+
  geom_vline(xintercept=9.5)+
  geom_hline(yintercept = 51.5)

setwd('D:/North Sea Ecosystem Model/1990 Start Time/Plots')
ggsave("MTI Difference.png",last_plot(),height=10,width=12,units = "in")

library(ggpubr)
ggarrange(one_mti,diff_mti,ncol=1,labels=c("A","B"))
ggsave("MTI All.png",last_plot(),height=16,width=12,units = "in")


mti_one <- all_dat[,,1,1]
for (a in 1:dim(mti_one)[1]){
  for (b in 1:dim(mti_one)[2]){
    mti_one[b,a] <- mean(c(na.omit(all_dat[a,b,1,])))
  }
}
write.csv(mti_one,"First Year MTI.csv")


trend<-matrix(0,nrow=n_spec+n_fisher,ncol=n_spec+n_fisher)
avg_mti<-array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year))

for (a in 1:(n_spec+n_fisher)){
  for (b in 1:(n_spec+n_fisher)){
    for (c in 1:n_year){
      avg_mti[b,a,c]<-mean(c(na.omit(all_dat[a,b,c,c(1:dim(all_dat)[4])])))
    }
  }
}

#*P values already imported#
for (a in 1:(n_spec+n_fisher)){
  for (b in 1:(n_spec+n_fisher)){
    if (p[a,b] <= 0.05){
      if (avg_mti[a,b,n_year] > avg_mti[a,b,1]){
        trend[a,b]<-"1" #Strong change
      }
      if (avg_mti[a,b,n_year] < avg_mti[a,b,1]){
        trend[a,b]<-"-1" #Weak change
      }
    }
  }
}

write.csv(trend,"Trends.csv")
