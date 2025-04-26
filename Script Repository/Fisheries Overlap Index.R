
#### To do: ####
#* Input predation and biomass csvs
#* Multiply P/B by biomass to get production to a predator (biomass consumed)
#* Subset for each predator pair
#* Run piankabio() function from pgirmess package
rm(list=ls())

## Load Packages ##
packages = c('pgirmess','dplyr')

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

outdir <-"D:/North Sea Ecosystem Model/1990 Start Time/Plots/FOI Results" #*N files created = N iterations:: Best to make a separate directory

species <- read.csv("Species.csv")
fleets <- read.csv("fleets.csv")

#species<-as.matrix(read.csv("FILE NAME.csv",header=F)) #List of functional groups in the model
#species_adj<-as.matrix(read.csv("FILE NAME.csv",header=F)) #Functional groups spelled as R will input them (i.e., special characters become periods)

## Global Parameters ##
n_spec<-length(species[,2]) #Number of functional groups
n_fisher<- 12 #Number of fisheries in the model
year_start<- 1990 #Model Start Year
year_end<- 2014  #Model End Year
n_year<- 25 #Number of years of the simulation
n_year_concern <- 25  #Could be different/implemented if you include a spin up to the years of concern

setwd(indir)
files <- list.files()

#set up parallel 
no_cores <- detectCores()  #determine number of cores on computer
cl<-makeCluster(no_cores)  #setup the size of your parallel cluster based on number of cores
registerDoSNOW(cl)         #register cluster to get it ready for parallel processing


FOI <- array(0,dim=c((n_spec+n_fisher),(n_spec+n_fisher),n_year,n_distinct(files)))

#set up text progress bar....this is personal code just to have a progress bar letting your know % completion
prog <- txtProgressBar(max = n_distinct(files), style = 3)
progress <- function(n) setTxtProgressBar(prog, n)
opts <- list(progress = progress)

ls=foreach(file_count=1:n_distinct(files),.options.snow = opts,.combine='rbind',.packages =c('PBSmodelling')) %dopar% {
  
  library(pgirmess)
  library(dplyr)
  
  if (files[file_count] != "mc_input"){
    setwd(paste(indir,"/",files[file_count],sep=""))

    biomass <- read.csv("biomass_annual.csv")
    biomass <- biomass[,-1]
    
    colnames(biomass) <- species$Species
    b_consumed <- array(0,dim=c(n_spec+n_fisher,n_spec+n_fisher,n_year_concern))
    
    
    for (a in 1:length(species[,2])){
      filename<-paste("predation_",species[a,2],"_annual.csv",sep="")
      if (file.exists(filename)){
        pb<-read.csv(filename,header=T) #Necessary data always starts on row 10
        pred_taxa<-colnames(pb) #Collected predator items of a prey
        for (elim in 1:length(pred_taxa)){
          if (pred_taxa[elim]=="X"){
            pred_taxa <-pred_taxa[-elim]
          }
        }
        
        for (b in 1:length(pb[,1])){
          for (c in 1:length(species[,2])){
            for (d in 2:length(pred_taxa)){
              if (species[c,3]==pred_taxa[d]){
                b_consumed[c,a,b]<-pb[b,d]*biomass[b,d] #You are creating a P/B matrix by skipping all non-preys
              }
            }
          }
        }
      }
    }
    
    filename<-"catch-fleet-group_annual.csv" #Fishery Catches
    #** If you have alot of fisheries or a long model, create a subset to reduce run time **
    if (file.exists(filename)){
      fishery<-read.csv(filename,header=T)
      for (y in year_start:year_end){ #Input years your model starts and ends
        for (x in 1:n_fisher){
          sub_catch <- subset(fishery,fishery$year==y & fishery$fleet==x)
          for (z in 1:length(sub_catch[,1])){
            for (w in 1:n_spec){
              if (sub_catch[z,1]==y && sub_catch[z,2]==x && sub_catch[z,3]==w){
                b_consumed[(n_spec+x),w,y-(year_start-1)]<-sub_catch[z,4]
              }
            }
          }
        }
      }
    }
  
    number <- dim(b_consumed)[1]*dim(b_consumed)[2]*dim(b_consumed)[3]
    cons_dataframe <- data.frame(Year <- rep("",number),
                                 Prey <- rep("",number),
                                 Predator <- rep("",number),
                                 Biomass <- rep(0,number))
    colnames(cons_dataframe) <- c("Year","Prey","Predator","Biomass")
    years <- year_start:year_end
    count <- 1
    
    
    for (pred in 1:dim(b_consumed)[1]){
      for (prey in 1:dim(b_consumed)[2]){
        if (b_consumed[pred,prey,1]>0){
          for (year in 1:n_year){
            if (year == 1 || year == n_year){
              cons_dataframe$Year[count] <- years[year]
              if (prey <= n_spec){
                cons_dataframe$Prey[count] <- species$Species[prey]
              } else {
                cons_dataframe$Prey[count] <- fleets$Fleet[prey-n_spec]
              }
              if (pred <= n_spec){
                cons_dataframe$Predator[count] <- species$Species[pred]
              } else {
                cons_dataframe$Predator[count] <- fleets$Fleet[pred-n_spec]
              }
              cons_dataframe$Biomass[count] <- b_consumed[pred,prey,year]
              count <- count + 1
            }
          }
        }
      }
    }
    
    cons_dataframe <- cons_dataframe[1:(count-1),]
    
    foi <- array(0,dim=dim(b_consumed))

    run <- matrix(0,ncol=dim(foi)[1],nrow=dim(foi)[1])
    
    for (pred1 in 1:dim(foi)[1]){
      for (pred2 in 1:dim(foi)[2]){
        if (run[pred1,pred2] == 0){
          run[pred1,pred2] <- run[pred2,pred1] <- 1
          for (year in 1:n_year){
            if (year == 1 || year == n_year){
              if (pred1 <= n_spec){
                obj1 <- cons_dataframe %>% filter(Predator %in% species$Species[pred1]) %>% filter(Year %in% years[year]) %>% dplyr::select(c("Prey","Biomass"))
              } else {
                obj1 <- cons_dataframe %>% filter(Predator %in% fleets$Fleet[pred1-n_spec]) %>% filter(Year %in% years[year]) %>% dplyr::select(c("Prey","Biomass"))
              }
              
              if (dim(obj1)[1]>0){    
                for (prey in 1:length(species[,2])){
                  check1 <-FALSE
                  for (first in 1:dim(obj1)[1]){
                    if (species[prey,2] == obj1$Prey[first]){
                      check1 <- TRUE
                    }
                  }
                  if (check1 == FALSE){
                    obj1[dim(obj1)[1]+1,] <- c(species$Species[prey],as.numeric(0))
                  }
                }
              }
            
            
              if (pred2 <=n_spec){
                obj2 <- cons_dataframe %>% filter(Predator %in% species$Species[pred2]) %>% filter(Year %in% years[year]) %>% dplyr::select(c("Prey","Biomass"))
                test<- subset(cons_dataframe,Predator==species$Species[pred2])
              } else {
                obj2 <- cons_dataframe %>% filter(Predator %in% fleets$Fleet[pred2-n_spec]) %>% filter(Year %in% years[year]) %>% dplyr::select(c("Prey","Biomass"))
                
              } 
              if (dim(obj2)[1]>0){
                for (prey in 1:length(species[,2])){
                  check2 <-FALSE
                  for (first in 1:dim(obj2)[1]){
                    if (species[prey,2] == obj2$Prey[first]){
                      check2 <- TRUE
                    }
                  }
                  if (check2 == FALSE){
                    obj2[dim(obj2)[1]+1,] <- c(species$Species[prey],as.numeric(0))
                  }
                }
              }
              
              if (dim(obj1)[1]>0 && dim(obj2)[1]>0){
                obj1$Biomass<-as.numeric(obj1$Biomass)
                obj2$Biomass<-as.numeric(obj2$Biomass)
                foi[pred1,pred2,year] <- foi[pred2,pred1,year] <- piankabio(obj1,obj2)
              }
            }
          }
        }
      }
    }
    
    FOI[,,,file_count] <- foi

    setwd(outdir)
    write.csv(FOI[,,1,file_count],paste(outdir,"/",files[file_count]," Year 1.csv",sep=""))
    write.csv(FOI[,,25,file_count],paste(outdir,"/",files[file_count]," Year 25.csv",sep=""))
    
  }
  setTxtProgressBar(prog,file_count)
}

# end parallel job
close(prog)
stopCluster(cl) #end the cluster for parallel processing
closeAllConnections() 
  
  
  ### Create FOI dataframe ####
  setwd("D:/North Sea Ecosystem Model/1990 Start Time/Plots/FOI Results")
  
  files <- list.files()
  
  year_one <- year_last <- data.frame()
  
  all_dat <- array(NA,dim=c((n_spec+n_fisher),(n_spec+n_fisher),2,n_distinct(files)))

  pb <- txtProgressBar(min=0,max=n_distinct(files),style=3)
  for (a in 1:n_distinct(files)){
    dat <- read.csv(files[a])
    dat <- dat[,-1]
    
    name <- unlist(strsplit(files[a],split = "mc_output_trial"))
    name2 <- unlist(strsplit(name,split = " Year "))
    name3 <- unlist(strsplit(name2,split = ".csv"))
    
    year <- as.numeric(name3)[2]
    iter <- as.numeric(name3)[1]
    
    if (year == 1){
      all_dat[,,1,a] <- as.matrix(dat)
    } else {
      all_dat[,,2,a] <- as.matrix(dat)
    }
    setTxtProgressBar(pb,a)
  }
  
  final <- data.frame(Species1 = rep("",(n_spec+n_fisher)*dim(all_dat)[4]),
                                      Species2 = rep("",(n_spec+n_fisher)*dim(all_dat)[4]),
                                      Year = rep(0,(n_spec+n_fisher)*dim(all_dat)[4]),
                                      FOI  = rep(0,(n_spec+n_fisher)*dim(all_dat)[4]))
  
  count <- 1
  for (a in 1:dim(all_dat)[1]){
    for (b in 1:dim(all_dat)[2]){
      for (c in 1:dim(all_dat)[3]){
        final$Species1[count] <- a
        final$Species2[count] <- b
        final$Year[count] <- c
        final$FOI[count] <- mean(c(na.omit(all_dat[a,b,c,])))
        count <- count + 1
      }
    }
  }
  
  year_one <- final %>% filter(Year %in% 1)
  year_last <- final %>% filter(Year %in% 2)
  library(ggplot2)
  
  diff <- year_last
  diff$FOI <-year_last$FOI-year_one$FOI
  
  year_one$Species1 <- as.factor(year_one$Species1)
  year_one$Species2 <- as.factor(year_one$Species2)
  length(year_one$Species2)
  
  ggplot(year_one,aes(x=reorder(Species2,seq(1:3600)),y=reorder(Species1,-seq(1:3600)),fill=FOI))+geom_tile()+
  scale_fill_gradientn(colours=c("white","black"),limits=c(0,1),labels=c("No Overlap","More Overlap"),breaks=c(0,1))+scale_x_discrete(position="top")+
  xlab("")+ylab("")+ 
  #geom_text(aes(label=Sig),vjust=0.75)+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=8),
        legend.position = "right")+
  geom_vline(xintercept=48.5)+
  geom_hline(yintercept = 12.5)+
  geom_vline(xintercept=9.5)+
  geom_hline(yintercept = 51.5)+
  geom_abline(intercept=61,slope=-1,lwd=1.5)


setwd('D:/North Sea Ecosystem Model/1990 Start Time/Plots')
ggsave("FOI Year One.png",last_plot(),height=10,width=12,units = "in")

ggplot(diff,aes(x=reorder(Species2,seq(1:3600)),y=reorder(Species1,-seq(1:3600)),fill=FOI))+geom_tile()+
  scale_fill_gradientn(colours=c("red","white","blue"),limits=c(-1,1),labels=c("Less Overlap","More Overlap"),breaks=c(-1,1))+scale_x_discrete(position="top")+
  xlab("")+ylab("")+ 
  #geom_text(aes(label=Sig),vjust=0.75)+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=8),
        legend.position = "right")+
  geom_vline(xintercept=48.5)+
  geom_hline(yintercept = 12.5)+
  geom_vline(xintercept=9.5)+
  geom_hline(yintercept = 51.5)+
  geom_abline(intercept=61,slope=-1,lwd=1.5)


setwd('D:/North Sea Ecosystem Model/1990 Start Time/Plots')
ggsave("FOI Difference.png",last_plot(),height=10,width=12,units = "in")


head(year_one)
length(year_one$FOI[year_one$FOI>0.3])
dim(year_one)
class(year_one$Species1)
year_last$Species1 <- as.numeric(year_last$Species1)
year_last$Species2 <- as.numeric(year_last$Species2)

sub <- year_last %>% filter(Species1 <10)
sub <- sub %>% filter(Species2 > 48)

sub[sub$FOI > 0.3,]
length(sub$FOI[sub$FOI>0.3])


year_one$Species1 <- as.numeric(year_one$Species1)
year_one$Species2 <- as.numeric(year_one$Species2)

sub2 <- year_one %>% filter(Species1 ==9)
sub2 <- sub2 %>% filter(Species2 > 48)

sub2[sub2$FOI > 0.3,]


diff$Species1 <- as.numeric(diff$Species1)
diff$Species2 <- as.numeric(diff$Species2)

sub3 <- diff %>% filter(Species1 == 9)
sub3 <- sub3 %>% filter(Species2 > 48)

sub3[sub3$FOI > 0.1,]

sub2[90:110,]


169999*1.2
3428*1.15
15826*2

203998/37553
0.0035*5
