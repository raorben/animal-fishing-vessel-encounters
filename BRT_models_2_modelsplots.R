library(ggplot2)
library(dplyr)
library(mgcv)
library(gbm)
library(dismo)
library(raster)
library(PresenceAbsence)

library(grid)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
library(pdp)
library(ape)
library(cowplot)
library(ggplotify)
library(wesanderson)


#RACHAEL
if(Sys.info()[7]=="rachaelorben") {
  userdir<-'/Users/rachaelorben/Dropbox/Research/GlobalFishingWatch'}
if(Sys.info()[7]=="torresle") {
  userdir<-'C:/Users/leigh.torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch'
  setwd('C:\\Users\\leigh.torres\\Dropbox (HMSC - OSU)\\seabirds\\GlobalFishingWatch\\Analysis\\Models\\EncTOint')
}
##change to match NEW COMPUTER
if(Sys.info()[7]=="Leigh Torres") { #if at home
  userdir<-'C:/Users/Leigh Torres/Dropbox (HMSC - OSU)/seabirds/GlobalFishingWatch'
  setwd('C:\\Users\\torres_l\\Dropbox (HMSC - OSU)\\seabirds\\GlobalFishingWatch\\Analysis\\Models\\EncTOint')
} 

LAAL_enc<-readRDS(paste0(userdir,"/Analysis/compileddata/LAAL_enc_2020-08-24.rds"))
STAL_enc<-readRDS(paste0(userdir,"/Analysis/compileddata/STAL_enc_2hr_2020-08-24.rds"))
STAL_enc_int<-readRDS(paste0(userdir,"/Analysis/compileddata/STAL_enc_int_2hr_2020-08-24.rds"))
STALevents<-readRDS(paste0(userdir,"/Analysis/compileddata/STALevents.rda"))  

LAAL_enc$log_num_mmsi<-log(LAAL_enc$num_mmsi)
STAL_enc$log_num_mmsi<-log(STAL_enc$num_mmsi)
STAL_enc_int$log_num_mmsi<-log(STAL_enc_int$num_mmsi)
LAAL_enc$perTrip_0<-LAAL_enc$perTrip*100

STAL_enc$RST_Per24rest_0<-STAL_enc$RST_Per24rest*100
STAL_enc$RST_Per24transit_0<-STAL_enc$RST_Per24transit*100
STAL_enc$RST_Per24actfor_0<-STAL_enc$RST_Per24actfor*100

STAL_enc_int$RST_Per24rest_0<-STAL_enc_int$RST_Per24rest*100
STAL_enc_int$RST_Per24transit_0<-STAL_enc_int$RST_Per24transit*100
STAL_enc_int$RST_Per24actfor_0<-STAL_enc_int$RST_Per24actfor*100


LAAL_enc%>%filter(perTrip>.4)%>%
  group_by(FishManage)%>%summarise(n=n())

STAL_enc<-left_join(STAL_enc,STALevents%>%
                      dplyr::select(eventID,InteractionTimeDiff,EncounterTimeDiff),
                    by=c("eventID"="eventID"))

STAL_enc$eventDiff<-STAL_enc$EncounterTimeDiff
idx<-which(is.na(STAL_enc$InteractionTimeDiff)==FALSE)
STAL_enc$eventDiff[is.na(STAL_enc$InteractionTimeDiff)==FALSE]<-STAL_enc$InteractionTimeDiff[idx]

STAL_enc_int<-left_join(STAL_enc_int,STALevents%>%
                      dplyr::select(eventID,InteractionTimeDiff,EncounterTimeDiff),
                    by=c("eventID"="eventID"))


# LAAL_enc ----------------------------------------------------------------
length(unique(LAAL_enc$birdID))
LAAL_enc$CV.Fold<-LAAL_enc$birdID

colnames(LAAL_enc)[28]
colnames(LAAL_enc)[51]
colnames(LAAL_enc)[20]
colnames(LAAL_enc)[78]
colnames(LAAL_enc)[80]
colnames(LAAL_enc)[81]
colnames(LAAL_enc)[82]
#make BRT model
lr<-0.01 ##set starting learning rate (usually between 0.01 and 0.001)

target.trees <- 0      #resets the target trees , make sure set condition to lower than 1000

while (target.trees < 1000) {
  cat("\n","fitting model with learning rate of ",lr,"\n")
  
  model.l <- gbm.step(LAAL_enc,
                    gbm.x = c(28,82,20,78,80,81), #your predictors; 
                    gbm.y = 53, #your response varaible: for enc models use 53(int); 61 (log_time3_min) for enc to int models
                    family = "bernoulli", #bernoulli for PA, gaussian for continuous
                    #site.weights = dat$wt,
                    n.trees = 50,
                    bag.fraction = 0.9, #0.75 is typical for larger dataset, using 0.9 for LAAL dataset
                    plot.main = TRUE,    
                    learning.rate = lr, 
                    tree.complexity = 2) #, #change interactions
  #fold.vector = dat$CVfold, # THIS SETS THE INDIVIDUALS INTO DIFFERENT 
  #CV FOLDS to account for non-independence; you have to make this vector to group your data.
  #n.folds = 10) # change the number of folds as needed
  
  #keep.fold.fit = T, #cv fold data; I think this is for offset models at end 
  #keep.fold.vector = T) #cv fold data; I think this is for offset models at end 
  
  if (object.size(model.l) > 0) {    #>0 = good, check if hasn't crashed
    target.trees <- model.l$gbm.call$best.trees
  }
  else {
    target.trees <- 0 #check if model size is zero -
  }
  lr <- lr / 2
}
quartz()
gbm.plot(model.l) #dismo pkg: this plots the fitted functions. Look at data distribution along rug-plot; all other predictors held at average
#"Note hat fitted functions are centered by subtracting their mean."
saveRDS(model.l,paste0(userdir,"/Analysis/compileddata/model_LAAL_enc_2020-09-17.rds"))
model.l<-readRDS(paste0(userdir,"/Analysis/compileddata/model_LAAL_enc_2020-09-17.rds"))

a<-plot(model.l, 'fishingden_60km_log', return.grid=TRUE)
ggplot()+
  geom_line(data=a,aes(x=fishingden_60km_log,y=y),color="green")
b<-plot(model.l, 'perTrip', return.grid=TRUE)
ggplot()+
  geom_line(data=b,aes(x=perTrip,y=y),color="green")
c<-plot(model.l, 'length', return.grid=TRUE)
ggplot()+
  geom_line(data=c,aes(x=length,y=y),color="green")
summary(model.l)
plot.gbm(model.l,i.var = 5) #gbm
gbm.plot.fits(model.l)#dismo
#if encounter error with plotting
#Error in seq.default(min(x$var.levels[[i.var[i]]]), max(x$var.levels[[i.var[i]]]), : 'from' must be a finite number
# then is likely becasue you have inf values in predictor variables, possibly from transformations. If log transforming, add 1 to all values. 

# to calculate % deviance explained: (mean total dev (Null) - estimated cv deviance (unexplained))/mean total dev (Null) 
lr       #learning rate  		
target.trees	# number of trees used
null.deviance <- model.l$self.statistics$mean.null
fitted.deviance <- model.l$cv.statistics$deviance.mean
pc.deviance <- (null.deviance - fitted.deviance)/null.deviance
round(pc.deviance,3)  	#cv.dev 
round(model.l$cv.statistics$discrimination.mean,3) #PA AUC
round(model.l$cv.statistics$correlation.mean,3) #cv.cor


# PLOT: LAAL_enc ----------------------------------------------------------
(A<-model.l$contributions)
#x axis labels to match
B<-c("log[Fishing Effort]","Trip (%)",
     "Fisheries Managment Area","RST Dominate State",
     "log[Fishing Density]","Vessel Length (m)") #specify the "pretty" plot titles here
info<-data.frame(A,B)
info<-info%>%arrange(desc(rel.inf)) #sorts by model contribution
info$var<-as.character(info$var)
info$Con_Fac<-c("C","C","F","F","C","C") #specify continious var or factor, used in plotting loops
info$type<-c("F","B","F","B","F","F") #specify variable "type": fisheries, bird, environment, time
info
info%>%group_by(type)%>%summarise(sumT=sum(rel.inf)) #sum inf of each variable type

PLOTS<-vector(mode = "list", length = nrow(info))
for (i in 1:nrow(info)){
  var.name=info$var[i]
  if(info$Con_Fac[i]=="F") next #skips factors as they cause an error
  p <- model.l %>%
    partial(pred.var = var.name,
            train=LAAL_enc, 
            n.trees = model.l$n.trees, prob = TRUE) %>% #PROB= TRUE presents true probabilities rather than logit scale (default)
    autoplot(size = .75, 
             rug = TRUE, 
             train = LAAL_enc) +
    geom_smooth(method = 'loess', color="#78B7C5", se=FALSE, size = .5, linetype = 5) +
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    #geom_hline(yintercept = .3,color="purple",linetype = 2)+
    ylim(.1,.6)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank())
  PLOTS[[i]]<-p
}

# #try to make centered version
# PLOTS<-vector(mode = "list", length = nrow(info))
# for (i in 1:nrow(info)){
#   var.name=info$var[i]
#   if(info$Con_Fac[i]=="F") next
#   dat <- model.l %>%
#     partial(pred.var = var.name,
#             train=LAAL_enc, 
#             n.trees = model.l$n.trees,prob = TRUE)# 
#   uyhat<-mean(dat$yhat)
#   dat$yhat_c<-(dat$yhat-uyhat)
#   colnames(dat)<-c("xvar","yhat","yhat_c")
#   p<-ggplot()+
#     geom_line(data=dat,aes(x=xvar, y=yhat_c))+
#     geom_smooth(data=dat,aes(x=xvar, y=yhat_c),method = 'loess', color="blue", se=FALSE, size = .5, linetype = 5) +
#     xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
#     geom_hline(yintercept = 0,color="purple",linetype = 2)+
#     #ylim(-.3,.1)+
#     theme_classic()+
#     theme(axis.title.x =element_text(size=8),
#           axis.title.y =element_blank())
#   PLOTS[[i]]<-p
# }
for (i in 1:nrow(info)){
  var.name=info$var[i]
  if(info$Con_Fac[i]=="C") next
    dat <- model.l %>%
    partial(pred.var = var.name,
            train=LAAL_enc, 
            n.trees = model.l$n.trees,prob = TRUE,cats =c("FishManage","RST_domstate_2hr"))
    uyhat<-mean(dat$yhat)
    dat$yhat_c<-dat$yhat-uyhat
    colnames(dat)<-c("xvar","yhat","yhat_c")
    p<-ggplot()+
      geom_boxplot(data=dat,aes(x=xvar,y=yhat))+ #change to yhat_c to center
      #geom_hline(yintercept = 0,color="purple",linetype = 2)+
      xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
      #geom_hline(yintercept = .3,color="purple",linetype = 2)+
      ylim(.1,.6)+
      theme_classic()+
      theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank())
  PLOTS[[i]]<-p
}

#RST
i=4
(var.name=info$var[i])
if(info$Con_Fac[i]=="C") next
dat <- model.l %>%
  partial(pred.var = var.name,
          train=LAAL_enc, 
          n.trees = model.l$n.trees,prob = TRUE)
colnames(dat)<-c("xvar","yhat")
dat$Mo<-c("Forage","Rest","Transit")
p<-ggplot()+
  geom_boxplot(data=dat,aes(x=xvar,y=yhat))+
  xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
  scale_x_discrete(labels=dat$Mo)+
  ylim(.05,.6)+
  theme_classic()+
  theme(axis.title.x =element_text(size=10),
        axis.title.y =element_blank(),
        axis.text.x =element_text(size=8),
        plot.background = element_blank())
PLOTS[[i]]<-p

nrow(info)
quartz(width=7.75,height=4)
plot<-plot_grid(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],
                PLOTS[[5]],PLOTS[[6]],nrow = 2,align = "hv")
y.grob <- textGrob("Probability", 
                   gp=gpar(fontsize=12), rot=90)
g<-grid.arrange(arrangeGrob(plot, left = y.grob))
cowplot::ggdraw(g) + 
  theme(plot.background = element_rect(fill="white", color = NA))
quartz.save(paste0(userdir,"/Analysis/PLOTS/LAAL enc_interaction model_FishED.png"), dpi=200)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Figure2_LAAL enc_interaction model_FishED.png"),dpi=1000)

# PLOTS.gbm<-vector(mode = "list", length = nrow(info))
# for (i in 1:nrow(info)){
#   gbm.plot(model.l, 
#               variable.no=i, 
#               smooth=TRUE, 
#               rug=TRUE, 
#               return.grid=TRUE,
#               n.plots=length(pred.names), ylim=c(-1.4,1.4),
#          common.scale=FALSE, write.title=FALSE, y.label="",cex.lab=1,cex.axis=.75,
#          x.label=(paste0(info$var[i],"\n",round(info$rel.inf[i],1),"%")),  
#          show.contrib=TRUE, plot.layout=c(1, 1))
#   abline(a=0,b=0, col="blue")
#   p
#   }
# 
# gg_partial_plot(model.l, info$var)
# dat<-partial_dependence.train(model.l, info$var)
# 
# response_matrix <- gbm::plot.gbm(model.l,
#                                  i.var = 1,
#                                  n.trees = model.l$n.trees,
#                                  return.grid = TRUE)
# 
# df_box <- list("vector", length(info$var))
# for (i in 1:length(info$var)){
#   df_box[[i]] <- partial_dependence(model.l, info$var[i])
# }
# df <- dplyr::bind_rows(df_box)
# head(df)
# 
# df_mean <-
#   df %>%
#   dplyr::group_by(variable) %>%
#   dplyr::summarise(mean = mean(fitted_function))
# 
# ggplot()+
#   geom_line(data = df,aes(x = value,y = fitted_function)) +
#   facet_wrap(~variable,nrow = 2,scales = "free_x") +
#   geom_hline(data = df_mean,aes(yintercept = mean),
#                       colour = "red",
#                       linetype = "dashed",
#                       alpha = 0.75) +
#   labs(x = "Variable Values",y = "Model Predicted Values")+
#   theme_classic()
# # 

# STAL_enc ----------------------------------------------------------------
length(unique(STAL_enc$birdID))
str(STAL_enc$birdID)

colnames(STAL_enc)[25]#"RST_Per24rest"
colnames(STAL_enc)[26]#"RST_Per24transit"
colnames(STAL_enc)[27]# "RST_Per24actfor"
colnames(STAL_enc)[40]
colnames(STAL_enc)[42]
colnames(STAL_enc)[60]
colnames(STAL_enc)[80]
colnames(STAL_enc)[81]#"log_num_mmsi"
colnames(STAL_enc)[82]
colnames(STAL_enc)[83]
colnames(STAL_enc)[84]
colnames(STAL_enc)[85]
colnames(STAL_enc)[86]
colnames(STAL_enc)[87] #"eventDiff"
hist(STAL_enc$eventDiff)

#make BRT model
lr<-0.01 ##set starting learning rate (usually between 0.01 and 0.001)

target.trees <- 0      #resets the target trees , make sure set condition to lower than 1000

while (target.trees < 1000) {
  cat("\n","fitting model with learning rate of ",lr,"\n")
  
  
  model.si <- gbm.step(STAL_enc,
                    gbm.x = c(82,83,84,40,42,60,80,81,87), #your predictors; 
                    gbm.y = 53, #your response varaible: for enc models use 53(int); 61 (log_time3_min) for enc to int models
                    family = "bernoulli", #bernoulli for PA, gaussian for continuous
                    #site.weights = dat$wt,
                    n.trees = 50,
                    bag.fraction = 0.75, #0.75 is typical for larger dataset, using 0.9 for LAAL dataset
                    plot.main = TRUE,    
                    learning.rate = lr, 
                    tree.complexity = 4)#,#change interactions
                    #fold.vector = STAL_enc$birdID,
                    #n.folds = 16) #, 
  #fold.vector = dat$CVfold, # THIS SETS THE INDIVIDUALS INTO DIFFERENT 
  #CV FOLDS to account for non-independence; you have to make this vector to group your data.
  #n.folds = 10) # change the number of folds as needed
  
  #keep.fold.fit = T, #cv fold data; I think this is for offset models at end 
  #keep.fold.vector = T) #cv fold data; I think this is for offset models at end 
  
  if (object.size(model.si) > 0) {    #>0 = good, check if hasn't crashed
    target.trees <- model.si$gbm.call$best.trees
  }
  else {
    target.trees <- 0 #check if model size is zero -
  }
  lr <- lr / 2
}

gbm.plot(model.si) #this plots the fitted functions. Look at data distribution along rug-plot; all other predictors held at average
summary(model.si)

#saveRDS(model.si,paste0(userdir,"/Analysis/compileddata/model_STAL_enc_2020-09-17.rds"))
saveRDS(model.si,paste0(userdir,"/Analysis/compileddata/model_STAL_enc_2020-11-23_eventdiff.rds"))
#model.si<-readRDS(paste0(userdir,"/Analysis/compileddata/model_STAL_enc_2020-08-25.rds"))
model.si<-readRDS(paste0(userdir,"/Analysis/compileddata/model_STAL_enc_2020-11-23_eventdiff.rds"))


#if encounter error with plotting
#Error in seq.default(min(x$var.levels[[i.var[i]]]), max(x$var.levels[[i.var[i]]]), : 'from' must be a finite number
# then is likely becasue you have inf values in predictor variables, possibly from transformations. If log transforming, add 1 to all values. 

# to calculate % deviance explained: (mean total dev (Null) - estimated cv deviance (unexplained))/mean total dev (Null) 
lr       #learning rate  		
target.trees	# number of trees used
null.deviance <- model.si$self.statistics$mean.null
fitted.deviance <- model.si$cv.statistics$deviance.mean
pc.deviance <- (null.deviance - fitted.deviance)/null.deviance
round(pc.deviance,3)  	#cv.dev 
round(model.si$cv.statistics$discrimination.mean,3) #PA AUC
round(model.si$cv.statistics$correlation.mean,3) #cv.cor

model.si<-readRDS(paste0(userdir,"/Analysis/compileddata/model_STAL_enc_2020-09-17.rds"))

# PLOT: STAL_enc ----------------------------------------------------------
(A<-model.si$contributions)
#x axis labels to match
B<-c("log[Fishing Effort]","log[Fishing Density]","Time of Day",
     "log[Depth(m)]","Month",
     "Small-scale Search (%)","Large-scale Search (%)","GPS Time Gap (min)",
     "Transit (%)")#     "Longhurst Region"
info<-data.frame(A,B)
info<-info%>%arrange(desc(rel.inf)) #sorts by model contribution
info$var<-as.character(info$var)
info$Con_Fac<-c("C","C","F","C","F","C","C","C","C")
info$type<-c("F","F","T","E","T","B","B","X","B")
#info$Con_Fac<-c("F","C","F","C","C","C","C","F")
#info$type<-c("T","F","T","E","B","B","B","E")
info

info%>%group_by(type)%>%summarise(sumT=sum(rel.inf))

PLOTS<-vector(mode = "list", length = nrow(info))
for (i in 1:nrow(info)){
  if(info$Con_Fac[i]=="F") next
  var.name=info$var[i]
  p <- model.si %>%
    partial(pred.var = var.name,
            train=STAL_enc, 
            n.trees = model.si$n.trees, prob = TRUE) %>%
    autoplot(color = "black", 
             size = .5, 
             rug = TRUE, 
             train = STAL_enc) +
    geom_smooth(method = 'loess', color="#78B7C5", se=FALSE, size = .5, linetype = 5) +
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    #ylim(.05,.8)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank(),
          axis.text.x =element_text(size=8),
          plot.background = element_blank())
  PLOTS[[i]]<-p
}

#DayNightDusk
  i=3
  var.name=info$var[i]
  if(info$Con_Fac[i]=="C") next
  dat <- model.si %>%
    partial(pred.var = var.name,
            train=STAL_enc, 
            n.trees = model.si$n.trees,prob = TRUE)
  colnames(dat)<-c("xvar","yhat")
  dat$Mo<-c("Day","Dusk","Night")
  p<-ggplot()+
    geom_boxplot(data=dat,aes(x=xvar,y=yhat))+
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    scale_x_discrete(labels=dat$Mo)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank(),
          axis.text.x =element_text(size=8),
          plot.background = element_blank())
  PLOTS[[i]]<-p

#Month (seperated to allow for re-ordering of factors)
  i=5
  var.name=info$var[i]
  if(info$Con_Fac[i]=="C") next
  dat <- model.si %>%
    partial(pred.var = var.name,
            train=STAL_enc, 
            n.trees = model.si$n.trees,prob = TRUE)
  colnames(dat)<-c("xvar","yhat")
  dat$Mo<-c("J","F","M","A","M","J","J","A","S","O","N","D")
  dat$xvar<-factor(dat$xvar, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
  p<-ggplot()+
    geom_boxplot(data=dat,aes(x=xvar,y=yhat))+
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    scale_x_discrete(labels=dat$Mo)+
    ylim(.05,.6)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank(),
          axis.text.x =element_text(size=8),
          plot.background = element_blank())
  PLOTS[[i]]<-p
  

# #Longhurst - x-axis label tilt
# var.name=info$var[8]
# dat <- model.si %>%
#   partial(pred.var = var.name,
#           train=STAL_enc, 
#           n.trees = model.si$n.trees, prob = TRUE) 
# colnames(dat)<-c("xvar","yhat")
# p<-ggplot()+
#   geom_boxplot(data=dat,aes(x=xvar,y=yhat))+
#   xlab(paste0(info$B[8],"\n",round(info$rel.inf[8],1),"%")) + 
#   ylim(.05,.6)+
#   theme_classic()+
#   theme(axis.title.x =element_text(size=10),
#         axis.title.y =element_blank(),
#         axis.text.x =element_text(size=7,angle = 30))
# PLOTS[[8]]<-p

nrow(info)
quartz(width=8.72,height=6.1)
plot<-plot_grid(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],
                PLOTS[[5]],PLOTS[[6]],PLOTS[[7]],PLOTS[[8]],
                PLOTS[[9]],nrow = 3,align = "hv")
y.grob <- textGrob("Probability", 
                   gp=gpar(fontsize=12), rot=90)
t.grob <- textGrob(" ", gp=gpar(fontsize=12),x = 0, hjust = 0)
g<-grid.arrange(arrangeGrob(plot, left = y.grob, top=t.grob))
cowplot::ggdraw(g) + 
  theme(plot.background = element_rect(fill="white", color = NA))

#quartz.save(paste0(userdir,"/Analysis/PLOTS/STAL enc_interaction model_FishED.png"), dpi=200)
#quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/Figure3A_STAL enc_interaction model_FishED.png"),dpi=1000)

quartz.save(paste0(userdir,"/Analysis/PLOTS/STAL enc_interaction model_FishED_GPSt.png"), dpi=200)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/Figure3A_STAL enc_interaction model_FishED_GPSt.png"),dpi=1000)


PLOTS<-vector(mode = "list", length = nrow(info))
for (i in 1:nrow(info)){
  if(info$Con_Fac[i]=="F") next
  if(info$type[i]!="F") next
  var.name=info$var[i]
  p <- model.si %>%
    partial(pred.var = var.name,
            train=STAL_enc, 
            n.trees = model.si$n.trees, prob = TRUE) %>%
    autoplot(color = "#fb6567", 
             size = 1, 
             rug = TRUE, 
             train = STAL_enc) +
    #geom_smooth(method = 'loess', color="#b6c58a", se=FALSE, size = .5, linetype = 5) +
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    #ylim(.05,.8)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank(),
          axis.text.x =element_text(size=8),
          rect = element_rect(fill = "transparent"),
          panel.background = element_blank(),
          plot.background = element_blank())
  PLOTS[[i]]<-p
}

for (i in 1:nrow(info)){
  if(info$Con_Fac[i]=="F") next
  if(info$type[i]!="B") next
  var.name=info$var[i]
  p <- model.si %>%
    partial(pred.var = var.name,
            train=STAL_enc, 
            n.trees = model.si$n.trees, prob = TRUE) %>%
    autoplot(color = "#15a6e4", 
             size = 1, 
             rug = TRUE, 
             train = STAL_enc) +
    #geom_smooth(method = 'loess', color="#b6c58a", se=FALSE, size = .5, linetype = 5) +
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    #ylim(.05,.8)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank(),
          axis.text.x =element_text(size=8),
          rect = element_rect(fill = "transparent"),
          panel.background = element_blank(),
          plot.background = element_blank())
  PLOTS[[i]]<-p
}

for (i in 1:nrow(info)){
  if(info$Con_Fac[i]=="F") next
  if(info$type[i]!="E") next
  var.name=info$var[i]
  p <- model.si %>%
    partial(pred.var = var.name,
            train=STAL_enc, 
            n.trees = model.si$n.trees, prob = TRUE) %>%
    autoplot(color = "#00ff00", 
             size = 1, 
             rug = TRUE, 
             train = STAL_enc) +
    #geom_smooth(method = 'loess', color="#b6c58a", se=FALSE, size = .5, linetype = 5) +
    xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
    #ylim(.05,.8)+
    theme_classic()+
    theme(axis.title.x =element_text(size=10),
          axis.title.y =element_blank(),
          axis.text.x =element_text(size=8),
          rect = element_rect(fill = "transparent"),
          panel.background = element_blank(),
          plot.background = element_blank())
  PLOTS[[i]]<-p
}

#DayNightDusk
i=3
var.name=info$var[i]
dat <- model.si %>%
  partial(pred.var = var.name,
          train=STAL_enc, 
          n.trees = model.si$n.trees,prob = TRUE)
colnames(dat)<-c("xvar","yhat")
dat$Mo<-c("Day","Dusk","Night")
p<-ggplot()+
  geom_boxplot(data=dat,aes(x=xvar,y=yhat), color="#00ff00")+
  xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
  scale_x_discrete(labels=dat$Mo)+
  theme_classic()+
  theme(axis.title.x =element_text(size=10),
        axis.title.y =element_blank(),
        axis.text.x =element_text(size=8),
        rect = element_rect(fill = "transparent"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = "none")
PLOTS[[i]]<-p

#Month (seperated to allow for re-ordering of factors)
i=5
var.name=info$var[i]
if(info$Con_Fac[i]=="C") next
dat <- model.si %>%
  partial(pred.var = var.name,
          train=STAL_enc, 
          n.trees = model.si$n.trees,prob = TRUE)
colnames(dat)<-c("xvar","yhat")
dat$Mo<-c("J","F","M","A","M","J","J","A","S","O","N","D")
dat$xvar<-factor(dat$xvar, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
p<-ggplot()+
  geom_boxplot(data=dat,aes(x=xvar,y=yhat),color="#00ff00")+
  xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
  scale_x_discrete(labels=dat$Mo)+
  ylim(.05,.6)+
  theme_classic()+
  theme(axis.title.x =element_text(size=10),
        axis.title.y =element_blank(),
        axis.text.x =element_text(size=8),
        rect = element_rect(fill = "transparent"),
        panel.background = element_blank(),
        plot.background = element_blank())
PLOTS[[i]]<-p

nrow(info)
quartz(width=8.72,height=4.1)
plot<-plot_grid(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],
                PLOTS[[5]],PLOTS[[6]],PLOTS[[7]],PLOTS[[8]],nrow = 2,align = "hv")
y.grob <- textGrob("Probability of an Interaction", 
                   gp=gpar(fontsize=12), rot=90)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_April2020/plots/GraphicalAbstract_Figure3A_STAL enc_interaction model_FishED.tiff"),dpi=300)



# STAL interaction model -------------------------------------
colnames(STAL_enc_int)[25]#"RST_Per24rest"
colnames(STAL_enc_int)[52]
colnames(STAL_enc_int)[82]
colnames(STAL_enc_int)[85]
colnames(STAL_enc_int)[86]

#make BRT model
lr<-0.01 ##set starting learning rate (usually between 0.01 and 0.001)

target.trees <- 0      #resets the target trees , make sure set condition to lower than 1000

while (target.trees < 1000) {
  cat("\n","fitting model with learning rate of ",lr,"\n")
  
  #
  model <- gbm.step(STAL_enc_int,
                    gbm.x = c(82,52,56,65,60,57,42,80,85), #your predictors; 
                    gbm.y = 61, #your response varaible: for enc models use 53(int); 61 (log_time3_min) for enc to int models
                    family = "gaussian", #bernoulli for PA, gaussian for continuous
                    #site.weights = dat$wt,
                    n.trees = 50,
                    bag.fraction = 0.75, #0.75 is typical for larger dataset, using 0.9 for LAAL dataset
                    plot.main = TRUE,    
                    learning.rate = lr, 
                    tree.complexity = 4) #, #change interactions
  #fold.vector = dat$CVfold, # THIS SETS THE INDIVIDUALS INTO DIFFERENT 
  #CV FOLDS to account for non-independence; you have to make this vector to group your data.
  #n.folds = 10) # change the number of folds as needed
  
  #keep.fold.fit = T, #cv fold data; I think this is for offset models at end 
  #keep.fold.vector = T) #cv fold data; I think this is for offset models at end 
  
  if (object.size(model) > 0) {    #>0 = good, check if hasn't crashed
    target.trees <- model$gbm.call$best.trees
  }
  else {
    target.trees <- 0 #check if model size is zero -
  }
  lr <- lr / 2
}

quartz()
gbm.plot(model) #this plots the fitted functions. Look at data distribution along rug-plot; all other predictors held at average
summary(model)

saveRDS(model,paste0(userdir,"/Analysis/compileddata/model_STAL_interDur_2020-09-17.rds"))
saveRDS(model,paste0(userdir,"/Analysis/compileddata/model_STAL_interDur_2020-11-25_GPSint.rds"))
model<-readRDS(paste0(userdir,"/Analysis/compileddata/model_STAL_interDur_2020-09-17.rds"))

# to calculate % deviance explained: (mean total dev (Null) - estimated cv deviance (unexplained))/mean total dev (Null) 
lr       #learning rate  		
target.trees	# number of trees used
null.deviance <- model$self.statistics$mean.null
fitted.deviance <- model$cv.statistics$deviance.mean
pc.deviance <- (null.deviance - fitted.deviance)/null.deviance
round(pc.deviance,3)  	#cv.dev 
round(model$cv.statistics$discrimination.mean,3) #PA AUC
round(model$cv.statistics$correlation.mean,3) #cv.cor


# PLOT: STAL intr dur -----------------------------------------------------
(A<-model$contributions)
#x axis labels to match
B<-c("log[Fishing Effort]","log[Depth]","Month",
     "Wind Speed (m/sec)","SST (C)","log[Chla]",
     "Small-scale Search (%)",
     "Age (days)")
info<-data.frame(A,B)
info<-info%>%arrange(desc(rel.inf)) #sorts by model contribution
info$var<-as.character(info$var)
info$Con_Fac<-c("C","C","F","C","C","C","C","C")
info$type<-c("F","E","T","E","E","E","Be","Bi")
info

B<-c("log[Fishing Effort]","log[Depth]","Month",
     "Wind Speed (m/sec)","SST (C)","log[Chla]","GPS Gap at Assocation (min)",
     "Small-scale Search (%)",
     "Age (days)")
info<-data.frame(A,B)
info<-info%>%arrange(desc(rel.inf)) #sorts by model contribution
info$var<-as.character(info$var)
info$Con_Fac<-c("C","C","F","C","C","C","C","C","C")
info$type<-c("F","E","T","E","E","E","X","Be","Bi")
info


info%>%group_by(type)%>%summarise(sumT=sum(rel.inf))


PLOTS<-vector(mode = "list", length = nrow(info))
for (i in 1:nrow(info)){
var.name=info$var[i]
if(info$Con_Fac[i]=="F") next
p <- model %>%
  partial(pred.var = var.name,
          train=STAL_enc_int, 
          n.trees = model$n.trees,
          recursive = FALSE,
          inv.link = exp) %>%
  autoplot(color = "black", 
           size = .5, 
           rug = TRUE, 
           train = STAL_enc_int) +
  geom_smooth(method = 'loess', color="#78B7C5", se=FALSE, size = .5, linetype = 5) +
  xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
  #scale_y_continuous(breaks = seq(4, 5, by=.05),
  #                   labels = scales::number_format(accuracy = 0.01))+
  #ylim(60,150)+
  theme_classic()+
  theme(axis.title.x =element_text(size=10),
        axis.title.y =element_blank())
PLOTS[[i]]<-p
}

#Month
i=3
var.name=info$var[i]
if(info$Con_Fac[i]=="C") next
dat <- model %>%
  partial(pred.var = var.name,
          train=STAL_enc, 
          n.trees = model$n.trees,prob = TRUE,
          recursive = FALSE,
          inv.link = exp)
colnames(dat)<-c("xvar","yhat")
dat$Mo<-c("J","F","M","A","M","J","J","A","S","O","N","D")
dat$xvar<-factor(dat$xvar, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
p<-ggplot()+
  geom_boxplot(data=dat,aes(x=xvar,y=yhat))+
  xlab(paste0(info$B[i],"\n",round(info$rel.inf[i],1),"%")) + 
  scale_x_discrete(labels=dat$Mo)+
  #ylim(60,150)+
  theme_classic()+
  theme(axis.title.x =element_text(size=10),
        axis.title.y =element_blank())
PLOTS[[i]]<-p

nrow(info)
quartz(width=8.72,height=6.1)
plot<-plot_grid(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],
          PLOTS[[5]],PLOTS[[6]],PLOTS[[7]],PLOTS[[8]],
          PLOTS[[9]],
          nrow = 3,align = "hv")
y.grob <- textGrob("Association Duration (min)", 
                   gp=gpar(fontsize=12), rot=90)
t.grob <- textGrob(" ", gp=gpar(fontsize=12),x = 0, hjust = 0)
g<-grid.arrange(arrangeGrob(plot, left = y.grob, top=t.grob))
cowplot::ggdraw(g) + 
  theme(plot.background = element_rect(fill="white", color = NA))
quartz.save(paste0(userdir,"/Analysis/PLOTS/STAL interaction model_GPS.png"),dpi=200)
quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/Figure3B_STAL interaction model_GPS.png"),dpi=1000)

#quartz.save(paste0(userdir,"/Analysis/PLOTS/STAL interaction model.png"),dpi=200)
#quartz.save(paste0(userdir,"/manuscript/JAE_GFW_manuscript2020/revision_Oct2020/Figure3B_STAL interaction model.png"),dpi=1000)

