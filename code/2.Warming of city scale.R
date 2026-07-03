library(ggplot2)
library(foreach)
library(tigris)
library(sf)
rm(list = ls())

##############readme################################
##plot figure 2, 3, 5, s3, s6, s7, s12, extended data 1-3
###################################################


#################################################
####Global variables########
{
seasons= c("annual","spring","summer","fall","winter")
seasonss= c("(a) Annual","(b) Spring","(c) Summer","(d) Fall","(e) Winter")
boundary <- st_read(dsn="../input/raw_shapefile/", layer="boundary")
boundary <- boundary[-which(boundary$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
us_states <- states(cb = TRUE, resolution = "20m") %>%  shift_geometry()
us_states <- us_states[-which(us_states$NAME %in% c("Alaska","Hawaii","Puerto Rico")),]
center <- st_transform(boundary,crs=st_crs(us_states)) %>% st_centroid() %>% st_geometry() %>% st_coordinates()
center1 <- st_transform(boundary,crs=4326) %>% st_centroid() %>% st_geometry() %>% st_coordinates()
area=read.csv("../input/Parking_Lot_LULC_area_sum.csv")
area=area[-which(area$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
area_parking=area$parking
area_urban=area$urban
ratio=area$parking/area$boundary*100
}
#####input data from GEE####
{
###Parking_Lot_city_mean_2024 represent multiple-year mean
input=read.csv("../input/parking_project_gee/Parking_Lot_city_mean_2024_buffer_2.csv")
input[,2:16]=input[,2:16]-273.15
#input=read.csv("../input/parking_project_gee_v2/Parking_Lot_city_mean_2024_buffer_2.csv")
input = input[-which(input$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
citys=input[,1]
input_diff= data.frame(input$LST_annual_parking-input$LST_annual_open,
                       input$LST_annual_parking-input$LST_annual_urban,
                       input$LST_annual_urban-input$LST_annual_open,
                       input$LST_spring_parking-input$LST_spring_open,
                       input$LST_spring_parking-input$LST_spring_urban,
                       input$LST_spring_urban-input$LST_spring_open,
                       input$LST_summer_parking-input$LST_summer_open,
                       input$LST_summer_parking-input$LST_summer_urban,
                       input$LST_summer_urban-input$LST_summer_open,
                       input$LST_fall_parking-input$LST_fall_open,
                       input$LST_fall_parking-input$LST_fall_urban,
                       input$LST_fall_urban-input$LST_fall_open,
                       input$LST_winter_parking-input$LST_winter_open,
                       input$LST_winter_parking-input$LST_winter_urban,
                       input$LST_winter_urban-input$LST_winter_open,
                       input$ISA_parking-input$ISA_open,
                       input$ISA_urban-input$ISA_open)
names(input_diff)= c('LST_diff_annual_parking', 'LST_diff_annual_parking_urban', 'LST_diff_annual_urban', 
                     'LST_diff_spring_parking', 'LST_diff_spring_parking_urban', 'LST_diff_spring_urban',
                     'LST_diff_summer_parking', 'LST_diff_summer_parking_urban', 'LST_diff_summer_urban',
                     'LST_diff_fall_parking',   'LST_diff_fall_parking_urban',   'LST_diff_fall_urban', 
                     'LST_diff_winter_parking', 'LST_diff_winter_parking_urban', 'LST_diff_winter_urban',
                     'ISA_diff_parking','ISA_diff_urban')
input=cbind(input,input_diff,ratio)
}

#################################################
#######calculate parking UHI contribution##
{
con_mean=NA
con_sd=NA
uhi_mean=NA
uhi_sd=NA
uhi_sum=NULL
con_sum=NULL
con_accumulate=NA
con_area_accumulate=NA
con_accumulate_mean=NA
con_area_accumulate_mean=NA
con2_mean=NA
con2_sd=NA
outlier=NA
for (ii in 1:5) {
  lst_diff_parking=input[[paste0("LST_diff_",seasons[ii],"_parking")]]
  lst_diff_urban=input[[paste0("LST_diff_",seasons[ii],"_urban")]]

  uhi=(lst_diff_parking*area_parking+lst_diff_urban*area_urban)/(area_parking+area_urban)
  uhi_mean[ii]=mean(uhi,na.rm=TRUE)
  uhi_sd[ii]=sd(uhi,na.rm=TRUE)
  
  uhi_con=lst_diff_parking*area_parking/(lst_diff_parking*area_parking+lst_diff_urban*area_urban)*100
  outlier[ii]=length(which(lst_diff_parking*lst_diff_urban<0))
  uhi_con[which(lst_diff_parking*lst_diff_urban<0)]=NA 
  con_mean[ii]=mean(uhi_con,na.rm=TRUE)
  con_sd[ii]=sd(uhi_con,na.rm=TRUE)
  uhi_con1=lst_diff_parking*area_parking
  uhi_con2=lst_diff_parking*area_parking+lst_diff_urban*area_urban
  con_accumulate_mean[ii]=mean(uhi_con1,na.rm=TRUE)
  con_area_accumulate_mean[ii]=mean(uhi_con2,na.rm=TRUE)
  con_accumulate= cbind(con_accumulate,uhi_con1)
  con_area_accumulate= cbind(con_area_accumulate,uhi_con2)
  con2_mean[ii]=mean(uhi_con1,na.rm=TRUE)/mean(uhi_con2,na.rm=TRUE)*100
  
  uhi_sum=cbind(uhi_sum,uhi)
  con_sum=cbind(con_sum,uhi_con)
}
uhi_sum=data.frame(uhi_sum)
names(uhi_sum)=c(paste0("uhi_",seasons))
con_sum=data.frame(con_sum)
names(con_sum)=c(paste0("con_",seasons))

  uhi_sum1=data.frame(season=1:5,mean=uhi_mean,sd=uhi_sd)
  con_sum1=data.frame(season=1:5,mean=con_mean,sd=con_sd)
  con_sum2=data.frame(season=1:5,mean=con2_mean,sd=con_sd)
  
  plot_bar_uhi = ggplot(data=uhi_sum1, aes(x=as.factor(season), y=mean,fill=as.factor(season))) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin = mean - sd/sqrt(100), ymax = mean + sd/sqrt(100)), width = 0.25,size=0.3,position=position_dodge(.9))+
    ylab(expression(SUHI~'('*degree*C*')'))+ xlab("")+ labs(fill="")+
    scale_x_discrete(labels = c("Annual","Spring","Summer","Fall","Winter"))+
    scale_fill_manual(values=alpha(c("#5e4fa2","#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                      label=c("Annual","Spring","Summer","Fall","Winter"))+
    theme_bw()+theme(text=element_text(family="serif"),
                     axis.title.y=element_text(colour="black", size=10, face="plain"),
                     axis.title.x=element_blank(),
                     axis.text=element_text(colour="black", size=10, face="plain"),
                     panel.grid = element_blank(),
                     legend.position = "none",
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
  
  plot_bar_con = ggplot(data=con_sum1, aes(x=as.factor(season), y=mean,fill=as.factor(season))) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin = mean - sd/sqrt(100), ymax = mean + sd/sqrt(100)), width = 0.5,size=0.3,position=position_dodge(.9))+
    ylab("Contribution to SUHI (%)")+ xlab("")+ labs(fill="")+
    scale_x_discrete(labels = c("Annual","Spring","Summer","Fall","Winter"))+
    coord_cartesian(ylim = c(19, 26.2)) + 
    scale_y_continuous(breaks = seq(20, 26,by=2))+
    scale_fill_manual(values=alpha(c("#5e4fa2","#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                      label=c("Annual","Spring","Summer","Fall","Winter"))+
    theme_bw()+theme(text=element_text(family="serif"),
                     axis.title.y=element_text(colour="black", size=10, face="plain"),
                     axis.title.x=element_blank(),
                     axis.text=element_text(colour="black", size=10, face="plain"),
                     panel.grid = element_blank(),
                     legend.position = "none",
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
  
  input0=data.frame(citys,uhi_sum,con_sum,lon=center[,1],lat=center[,2])
  MAX=round(apply(input0[,2:11],2,max,na.rm=TRUE),1)+0.1
  MIN=round(apply(input0[,2:11],2,min,na.rm=TRUE),1)-0.1
  plot_map_uhi=list()
  plot_map_con=list()
  for (ii in 1:5) {
    season=seasons[ii]
    plot_map_uhi [[ii]] = ggplot()+
      geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("uhi_",season))),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
      labs(fill=expression(SUHI~'('*degree*C*')'))+
      ylab("")+ xlab("")+ 
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = c(0.92,0.22),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.5,vjust = 2),
                       legend.key.size = unit(0.2, "cm"),
                       legend.key.width = unit(0.2, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=8.3,hjust = 0.01,vjust=-9))+
      {if (ii==1) scale_fill_distiller(limits=c(MIN[1],MAX[1]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
      {if (ii==2) scale_fill_distiller(limits=c(MIN[2],MAX[2]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==3) scale_fill_distiller(limits=c(MIN[3],MAX[3]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==4) scale_fill_distiller(limits=c(MIN[4],MAX[4]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==5) scale_fill_distiller(limits=c(MIN[5],MAX[5]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
      {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                      axis.ticks = element_line(color = "black", linewidth =0.5))}
    plot_map_con[[ii]] = ggplot()+
      geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("con_",season))),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
      labs(fill="CP (%)")+
      ylab("")+ xlab("")+ 
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = c(0.925,0.22),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.5,vjust = 2),
                       legend.key.size = unit(0.2, "cm"),
                       legend.key.width = unit(0.2, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=8.3,hjust = 0.01,vjust=-9))+
      {if (ii==1) scale_fill_distiller(limits=c(MIN[6],MAX[6]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
      {if (ii==2) scale_fill_distiller(limits=c(MIN[7],MAX[7]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==3) scale_fill_distiller(limits=c(MIN[8],MAX[8]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==4) scale_fill_distiller(limits=c(MIN[9],MAX[9]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==5) scale_fill_distiller(limits=c(MIN[10],MAX[10]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
      {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                        axis.ticks = element_line(color = "black", linewidth =0.5))}
    }
  plot_sum_map=ggplot() +
      coord_equal(xlim = c(0, 165), ylim = c(0, 205), expand = FALSE) +
      annotation_custom(ggplotGrob(plot_map_uhi[[2]]), xmin = 5, xmax = 85, ymin = 150, ymax = 200) +
      annotation_custom(ggplotGrob(plot_map_con[[2]]), xmin = 85, xmax = 165, ymin = 150, ymax = 200) +  
      annotation_custom(ggplotGrob(plot_map_uhi[[3]]), xmin = 5, xmax = 85, ymin = 100, ymax = 150) +
      annotation_custom(ggplotGrob(plot_map_con[[3]]), xmin = 85, xmax = 165, ymin = 100, ymax = 150) +
      annotation_custom(ggplotGrob(plot_map_uhi[[4]]), xmin = 5, xmax = 85, ymin = 50, ymax = 100) +  
      annotation_custom(ggplotGrob(plot_map_con[[4]]), xmin = 85, xmax = 165, ymin = 50, ymax = 100) +
      annotation_custom(ggplotGrob(plot_map_uhi[[5]]), xmin = 5, xmax = 85, ymin = 0, ymax = 50) +
      annotation_custom(ggplotGrob(plot_map_con[[5]]), xmin = 85, xmax = 165,ymin = 0, ymax = 50) +  
      annotate("text", x =3, y =225, label = "Annual",angle=90,colour = "black",size=4,family="serif")+
      annotate("text", x =3, y =25, label = "Winter",angle=90,colour = "black",size=4,family="serif")+
      annotate("text", x =3, y =75, label = "Fall",angle=90,colour = "black",size=4,family="serif")+
      annotate("text", x =3, y =125, label = "Summer",angle=90,colour = "black",size=4,family="serif")+
      annotate("text", x =3, y =175, label = "Spring",angle=90,colour = "black",size=4,family="serif")+
      annotate("text", x =42.5, y =202.5, label = "SUHI",colour = "black",size=4,family="serif")+
      annotate("text", x =122.5, y =202.5, label = "Contribution",colour = "black",size=4,family="serif")+
      theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
      theme_void()
    tiff(paste0("../figure/Fig_sum_extend3.tif"),width=16.5,height=20.5, units = "cm", res = 300, compression = "lzw")
    plot(plot_sum_map)
    dev.off()
}

#################################################
########calculate cooling potential#####
#### ISA reduction scenario###
{
input_parking=rbind(read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_1_2024.csv")),
                    read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_2_2024.csv")),
                    read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_3_2024.csv")),
                    read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_4_2024.csv")))
input_parking[,4:8]=input_parking[,4:8]-273.15

isa_iqr=NA
for (ii in 1:nrow(input)) {
  isa=input_parking$ISA_parking[input_parking$city==input$city[ii]]
  isa_iqr[ii]=quantile(isa,na.rm = TRUE)[4]-quantile(isa,na.rm = TRUE)[2]
}

slope_sum=data.frame(nrow=nrow(input),ncol=5)
pvalue_sum=data.frame(nrow=nrow(input),ncol=5)
for (id in 1:nrow(input)) {
  input_parking_sel=input_parking[input_parking$city==input$city[id],]
  if ((max(input_parking_sel$ISA_parking,na.rm=TRUE)>0)) {
    for (ii in 1:5) {
      regression = lm(input_parking_sel[[paste0("LST_",seasons[ii],"_parking")]]~input_parking_sel[[paste0("ISA_parking")]])
      slope_sum[id,ii]=coef(regression)[2]
      pvalue_sum[id,ii]=round(summary(regression)$coefficients[2,4],2)
    }
  }
}
names(slope_sum)=c("slope_annual","slope_spring","slope_summer","slope_fall","slope_winter")
slope_sum[which(isa_iqr<5),]=NA

green0=c(60,55,50,45,40,35,30,25,20)
cool=foreach (green_id=1:length(green0),.combine=rbind) %do% {
  foreach (ii=1:5,.combine=rbind) %do% {
    cbind(green0[green_id],ii,
          mean((green0[green_id]-input[[paste0("ISA_parking")]])*slope_sum[,ii],na.rm=TRUE),
          sd((green0[green_id]-input[[paste0("ISA_parking")]])*slope_sum[,ii],na.rm=TRUE),
          mean((green0[green_id]-input[[paste0("ISA_parking")]])*slope_sum[,ii]*area_parking/(area_parking+area_urban),na.rm=TRUE),
          sd((green0[green_id]-input[[paste0("ISA_parking")]])*slope_sum[,ii]*area_parking/(area_parking+area_urban),na.rm=TRUE))
  }
}
cool=data.frame(cool)
names(cool)=c("isa","season","mean_cool","sd_cool","mean_uhi_delta","sd_uhi_delta")

plot_cool_sensitivity=ggplot()+
  geom_point(data= cool,aes(x=isa,y=mean_cool,color=as.factor(season)))+
  geom_smooth(data= cool,aes(x=isa,y=mean_cool,color=as.factor(season)),method="lm",size=0.5,fill=NA)+
  scale_color_manual(values=alpha(c("#5e4fa2","#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                      label=c("Annual","Spring","Summer","Fall","Winter"))+
  scale_y_continuous(limits = c(-3.6,0.1),breaks =seq(-3,0,by=1))+
  scale_x_reverse(breaks=seq(20,60,by=5))+
  guides(color = guide_legend(reverse=FALSE,nrow=3))+
  labs(color="")+ylab(expression(Delta~LST~'('*degree*C*')'))+xlab("ISA (%)")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title = element_text(colour="black", size=9, face="plain"),
                   axis.text  =  element_text(colour="black", size=9, face="plain"),
                   legend.position = c(0.3,0.15),
                   legend.direction = "horizontal",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain"),
                   legend.key.size = unit(0.05, "cm"),
                   legend.key.width = unit(0.15, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

plot_cool_isa_uhi_single_y= ggplot(cool, aes(x = isa,y = mean_uhi_delta, color = factor(season))) +
  geom_point(size = 2) + 
  scale_y_continuous(limits=c(-0.7,0),name = expression(Delta~SUHI~'('*degree*C*')'))+
  scale_x_reverse(breaks=seq(20,60,by=5))+
  scale_color_manual(values=alpha(c("#5e4fa2","#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                     label=c("Annual","Spring","Summer","Fall","Winter"))+
  guides(color = guide_legend(reverse=FALSE,nrow=3,order = 2))+
  labs(x = "ISA (%)", color = "", shape = "Variable") +  # Adding shape to legend
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title = element_text(colour="black", size=9, face="plain"),
                   axis.text  =  element_text(colour="black", size=9, face="plain"),
                   legend.position = c(0.2,0.2),
                   legend.direction = "vertical",
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain",vjust=-1),
                   legend.key.size = unit(0.05, "cm"),
                   legend.key.width = unit(0.2, "cm"),
                   legend.spacing.y = unit(-0.3, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

green=50 ##ISA threshold
uhi_delta_isa_sum=NULL
lst_delta_isa_sum=NULL
for (ii in 1:5) {
  cooling_parking=(green-input[[paste0("ISA_parking")]])*slope_sum[,ii]
  uhi_delta=cooling_parking*area_parking/(area_parking+area_urban)
  uhi_delta_isa_sum=cbind(uhi_delta_isa_sum,uhi_delta)
  lst_delta_isa_sum=cbind(lst_delta_isa_sum,cooling_parking)
  cat(quantile(uhi_delta,na.rm=TRUE),"\n")
}
input0=data.frame(citys,uhi_delta_isa_sum,lst_delta_isa_sum,lon=center[,1],lat=center[,2],lat1=center1[,2])
names(input0)[2:6]=c(paste0("uhi_delta_isa_",seasons))
names(input0)[7:11]=c(paste0("lst_delta_isa_",seasons))

MAX=round(apply(input0[,2:6],2,max,na.rm=TRUE),1)+0.1
MIN=round(apply(input0[,2:6],2,min,na.rm=TRUE),1)-0.1
MAX_lst=round(apply(input0[,7:11],2,max,na.rm=TRUE),1)+0.1
MIN_lst=round(apply(input0[,7:11],2,min,na.rm=TRUE),1)-0.1
plot_map_uhi_delta_isa=list()
plot_map_lst_delta_isa=list()
for (ii in 1:5) {
  season=seasons[ii]
  plot_map_uhi_delta_isa[[ii]] = ggplot()+
    geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("uhi_delta_isa_",season))),pch=21,size=2)+
    geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
    labs(fill=expression(Delta~SUHI~'('*degree*C*')'))+
    ylab("")+ xlab("")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.91,0.22),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.7),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     plot.title = element_text(face="plain",size=8.3,hjust = 0.01,vjust=-8))+
    {if (ii==1) scale_fill_gradient2(midpoint=0, limits=c(MIN[1],MAX[1]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==2) scale_fill_gradient2(midpoint=0, limits=c(MIN[2],MAX[2]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==3) scale_fill_gradient2(midpoint=0, limits=c(MIN[3],MAX[3]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==4) scale_fill_gradient2(midpoint=0, limits=c(MIN[4],MAX[4]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==5) scale_fill_gradient2(midpoint=0, limits=c(MIN[5],MAX[5]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                      axis.ticks = element_line(color = "black", linewidth =0.5))}
  plot_map_lst_delta_isa[[ii]] = ggplot()+
    geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("lst_delta_isa_",season))),pch=21,size=2)+
    geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
    labs(fill=expression(Delta~LST~'('*degree*C*')'))+
    ylab("")+ xlab("")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.91,0.22),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.7),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     plot.title = element_text(face="plain",size=8.3,hjust = 0.01,vjust=-8))+
    {if (ii==1) scale_fill_gradient2(midpoint=0, limits=c(MIN_lst[1],MAX_lst[1]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==2) scale_fill_gradient2(midpoint=0, limits=c(MIN_lst[2],MAX_lst[2]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==3) scale_fill_gradient2(midpoint=0, limits=c(MIN_lst[3],MAX_lst[3]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==4) scale_fill_gradient2(midpoint=0, limits=c(MIN_lst[4],MAX_lst[4]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==5) scale_fill_gradient2(midpoint=0, limits=c(MIN_lst[5],MAX_lst[5]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                      axis.ticks = element_line(color = "black", linewidth =0.5))}
}
cooling_isa_lat <- ggplot(input0, aes(x = lat1, y =uhi_delta_isa_annual)) +
  geom_point(size=0.5,color="#5e4fa2",alpha=0.5) + labs(y=expression(Delta~SUHI~'('*degree*C*')'),
                              x=expression(Latitude~'('*degree*')'))+
  scale_y_continuous(limits=c(MIN[1],MAX[1]),breaks = c(-1.5,0))+
  geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.line = element_line(colour = "black",size=0.2),
                   panel.border = element_blank(),
                   panel.grid = element_blank(),
                   plot.background = element_rect(fill='transparent', color=NA),
                   plot.margin = unit(c(0,0,-0.2,-0.1), "cm"),
                   axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                   axis.ticks.length=unit(.05, "cm"),
                   axis.text.y = element_text(face="plain",size=6,color="black",hjust = 10),
                   axis.text.x = element_text(face="plain",size=6,color="black",vjust = 3),
                   axis.title.x =  element_text(face="plain",size=6,color="black",vjust = 6),
                   axis.title.y =  element_text(face="plain",size=6,color="black",vjust = -2),
                   plot.title = element_blank())+  coord_flip()
}
#### parking areas reduction scenario###
{
green0=seq(0.1,0.5,by=0.05)
  cool_mean=foreach (green_id=1:length(green0),.combine=rbind) %do% {
    foreach (ii=1:5,.combine=rbind) %do% {
      lst_diff_parking=input[[paste0("LST_diff_",seasons[ii],"_parking")]]
      uhi_change_open=lst_diff_parking*area_parking*green0[green_id]/(area_parking+area_urban)
      cbind((1-green0[green_id]),ii,-mean(uhi_change_open,na.rm=TRUE),sd(uhi_change_open,na.rm=TRUE))
    }
  }
  cool_mean=as.data.frame(cool_mean)
  names(cool_mean)=c("scenario","season","uhi_delta_open","uhi_delta_open_std")
  plot_cool_area_uhi_single_y <- ggplot(cool_mean,aes(x=scenario*100,y=uhi_delta_open,color=as.factor(season)))+
    geom_point(size = 2) + 
    scale_y_continuous(limits=c(-0.7,0),name = expression(Delta~SUHI~'('*degree*C*')'))+
    scale_x_reverse()+
    scale_color_manual(values=alpha(c("#5e4fa2","#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                       label=c("Annual","Spring","Summer","Fall","Winter"))+
    scale_shape_manual(values=c(16,2),
                       label=c(expression(Delta~LST),expression(Delta~UHI)))+
    guides(color = guide_legend(reverse=FALSE,nrow=3,order = 2))+
    labs(x = "Coverage (%)", color = "Season", shape = "Variable") +  # Adding shape to legend
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title = element_text(colour="black", size=9, face="plain"),
                     axis.text  =  element_text(colour="black", size=9, face="plain"),
                     legend.position = "none",
                     legend.direction = "vertical",
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain",vjust=-1),
                     legend.key.size = unit(0.05, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     legend.spacing.y = unit(-0.3, "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
  
green_id=which(green0==0.5)
uhi_delta_area_sum0=foreach (ii=1:5,.combine=cbind) %do% {
    lst_diff_parking=input[[paste0("LST_diff_",seasons[ii],"_parking")]]
    lst_diff_urban=input[[paste0("LST_diff_",seasons[ii],"_urban")]]
    uhi_change_urban=-(lst_diff_parking*area_parking*green0[green_id]-lst_diff_urban*area_parking*green0[green_id])/(area_parking+area_urban)
    uhi_change_open=-lst_diff_parking*area_parking*green0[green_id]/(area_parking+area_urban)
    cbind(uhi_change_open)
}
uhi_delta_area_sum=data.frame(uhi_delta_area_sum0)
names(uhi_delta_area_sum)=paste0("uhi_delta_area_",seasons)
input0=data.frame(citys,uhi_delta_area_sum,lon=center[,1],lat=center[,2],lat1=center1[,2])

cooling_area_lat <- ggplot(input0, aes(x = lat1, y =uhi_delta_area_annual))+
  geom_point(size=0.5,color="#5e4fa2",alpha=0.5 ) + labs(y=expression(Delta~SUHI~'('*degree*C*')'),
                              x=expression(Latitude~'('*degree*')'))+
  scale_y_continuous(limits=c(MIN[1],MAX[1]),breaks = c(-1.5,0))+
  geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.line = element_line(colour = "black",size=0.2),
                   panel.border = element_blank(),
                   panel.grid = element_blank(),
                   plot.background = element_rect(fill='transparent', color=NA),
                   plot.margin = unit(c(0,0,-0.2,-0.1), "cm"),
                   axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                   axis.ticks.length=unit(.05, "cm"),
                   axis.text.y = element_text(face="plain",size=6,color="black",hjust = 10),
                   axis.text.x = element_text(face="plain",size=6,color="black",vjust = 3),
                   axis.title.x =  element_text(face="plain",size=6,color="black",vjust = 6),
                   axis.title.y =  element_text(face="plain",size=6,color="black",vjust = -2),
                   plot.title = element_blank())+  coord_flip()

plot_map_uhi_delta_area=list()
for (ii in 1:5) {
  season=seasons[ii]
  plot_map_uhi_delta_area[[ii]] = ggplot()+
    geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("uhi_delta_area_",season))),pch=21,size=2)+
    geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
    labs(fill=expression(Delta~SUHI~'('*degree*C*')'))+
    ylab("")+ xlab("")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.91,0.22),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.7),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     plot.title = element_text(face="plain",size=8.3,hjust = 0.01,vjust=-8))+
    {if (ii==1) scale_fill_gradient2(midpoint=0, limits=c(MIN[1],MAX[1]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==2) scale_fill_gradient2(midpoint=0, limits=c(MIN[2],MAX[2]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==3) scale_fill_gradient2(midpoint=0, limits=c(MIN[3],MAX[3]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==4) scale_fill_gradient2(midpoint=0, limits=c(MIN[4],MAX[4]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==5) scale_fill_gradient2(midpoint=0, limits=c(MIN[5],MAX[5]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                      axis.ticks = element_line(color = "black", linewidth =0.5))}
}
}

#################################################
######sum_up to include cooling potential######
{
  input=cbind(input,uhi_sum,con_sum,slope_sum,lat1=center1[,2])
}

#################################################
###########bar plot#########################
{
  input_mean=data.frame(t(apply(input[,-1],2,mean,na.rm=TRUE)))
  input_sd=data.frame(t(apply(input[,-1],2,sd,na.rm=TRUE)))
  
  input_lst_season=data.frame(rbind(cbind(1,1,input_mean$LST_annual_parking,input_sd$LST_annual_parking),
                                    cbind(2,1,input_mean$LST_annual_urban,input_sd$LST_annual_urban),
                                    cbind(3,1,input_mean$LST_annual_open,input_sd$LST_annual_open),
                                    cbind(1,2,input_mean$LST_spring_parking,input_sd$LST_spring_parking),
                                    cbind(2,2,input_mean$LST_spring_urban,input_sd$LST_spring_urban),
                                    cbind(3,2,input_mean$LST_spring_open,input_sd$LST_spring_open),
                                    cbind(1,3,input_mean$LST_summer_parking,input_sd$LST_summer_parking),
                                    cbind(2,3,input_mean$LST_summer_urban,input_sd$LST_summer_urban),
                                    cbind(3,3,input_mean$LST_summer_open,input_sd$LST_summer_open),
                                    cbind(1,4,input_mean$LST_fall_parking,input_sd$LST_fall_parking),
                                    cbind(2,4,input_mean$LST_fall_urban,input_sd$LST_fall_urban),
                                    cbind(3,4,input_mean$LST_fall_open,input_sd$LST_fall_open),
                                    cbind(1,5,input_mean$LST_winter_parking,input_sd$LST_winter_parking),
                                    cbind(2,5,input_mean$LST_winter_urban,input_sd$LST_winter_urban),
                                    cbind(3,5,input_mean$LST_winter_open,input_sd$LST_winter_open)))
  names(input_lst_season)=c("position","season","mean","sd")
  
  input_lst_diff_season=data.frame(rbind(cbind(1,1,input_mean$LST_diff_annual_parking,input_sd$LST_diff_annual_parking),
                                         cbind(2,1,input_mean$LST_diff_annual_urban,input_sd$LST_diff_annual_urban),
                                         cbind(1,2,input_mean$LST_diff_spring_parking,input_sd$LST_diff_spring_parking),
                                         cbind(2,2,input_mean$LST_diff_spring_urban,input_sd$LST_diff_spring_urban),
                                         cbind(1,3,input_mean$LST_diff_summer_parking,input_sd$LST_diff_summer_parking),
                                         cbind(2,3,input_mean$LST_diff_summer_urban,input_sd$LST_diff_summer_urban),
                                         cbind(1,4,input_mean$LST_diff_fall_parking,input_sd$LST_diff_fall_parking),
                                         cbind(2,4,input_mean$LST_diff_fall_urban,input_sd$LST_diff_fall_urban),
                                         cbind(1,5,input_mean$LST_diff_winter_parking,input_sd$LST_diff_winter_parking),
                                         cbind(2,5,input_mean$LST_diff_winter_urban,input_sd$LST_diff_winter_urban)))
  names(input_lst_diff_season)=c("position","season","mean","sd")
  input_lst_diff1_season=data.frame(rbind(cbind(1,input_mean$LST_diff_annual_parking_urban,input_sd$LST_diff_annual_parking_urban),
                                          cbind(2,input_mean$LST_diff_spring_parking_urban,input_sd$LST_diff_spring_parking_urban),
                                          cbind(3,input_mean$LST_diff_summer_parking_urban,input_sd$LST_diff_summer_parking_urban),
                                          cbind(4,input_mean$LST_diff_fall_parking_urban,input_sd$LST_diff_fall_parking_urban),
                                          cbind(5,input_mean$LST_diff_winter_parking_urban,input_sd$LST_diff_winter_parking_urban)))
  names(input_lst_diff1_season)=c("season","mean","sd")
  
 plot_bar_lst = ggplot(data=input_lst_season, aes(x=as.factor(season), y=mean,fill=as.factor(position))) +
  geom_bar(stat="identity",position=position_dodge())+
  geom_errorbar(aes(ymin = mean - sd/sqrt(100), ymax = mean + sd/sqrt(100)), width = 0.25,size=0.3,position=position_dodge(.9))+
  ylab(expression(LST~'('*degree*C*')'))+ xlab("")+ labs(fill="")+
  scale_fill_manual(values=c("gray","orangered4","forestgreen"),
                    label=c("Parking lot","Other built-up","Open space"))+
  scale_x_discrete(labels = c("Annual","Spring","Summer","Fall","Winter"))+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   axis.text=element_text(colour="black", size=10, face="plain"),
                   panel.grid = element_blank(),
                   legend.position = c(0.85,0.9),
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain",margin = margin(b = 1)),
                   legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = -0.1),hjust = 0.3),
                   legend.key.size = unit(0.4, "cm"),
                   legend.key.width = unit(0.4, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))

plot_bar_lst_diff = ggplot(data=input_lst_diff_season, aes(x=as.factor(season), y=mean,fill=as.factor(position))) +
  geom_bar(stat="identity",position=position_dodge(),width=0.55)+
  geom_errorbar(aes(ymin = mean - sd/sqrt(100), ymax = mean + sd/sqrt(100)),  width = 0.2,size=0.3,position=position_dodge(.5))+
  ylab(expression(Delta~LST~'('*degree*C*')'))+ xlab("")+ labs(fill="")+
  scale_fill_manual(values=c("gray","orangered4"),
                    label=c("Parking lot","Other built-up"))+
  scale_x_discrete(labels = c("Annual","Spring","Summer","Fall","Winter"))+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.title.y=element_text(colour="black", size=10, face="plain"),
                   axis.title.x=element_blank(),
                   axis.text=element_text(colour="black", size=10, face="plain"),
                   panel.grid = element_blank(),
                   legend.position = c(0.85,0.92),
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain",margin = margin(b = 1)),
                   legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = -0.1),hjust = 0.3),
                   legend.key.size = unit(0.4, "cm"),
                   legend.key.width = unit(0.4, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
}

#################################################
##########scatter plot######################
##LST_parking lots vs LST_green spaces
{
pvalue=NA
slope=NA
data_bin=foreach (ii=1:5,.combine=rbind) %do% {
  input_sel=data.frame(season=ii,
                       open=input[[paste0("LST_",seasons[ii],"_open")]],
                       park=input[[paste0("LST_",seasons[ii],"_parking")]])
  regression = lm(input_sel$park~input_sel$open)
  slope[ii] = paste0("Slope=",sprintf("%.2f",coef(regression)[2]))
  pvalue0= summary(regression)$coefficients[4]
  if (pvalue0<0.01) {pvalue1="P<0.01"} else if (pvalue0>0.01 & pvalue0<0.05) {pvalue1="P<0.05"} else {pvalue1=paste0("P=",sprintf("%.2f",pvalue0))}
  pvalue[ii]= pvalue1
  input_sel
}
input_park_annual=data_bin[data_bin$season==1,]
input_park_season=data_bin[data_bin$season %in% 2:5,]

plot_scatter_season=ggplot(data= input_park_season,aes(x=open,y=park,color=as.factor(season)))+
  geom_point(size=1)+
  geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA)+
  geom_abline (slope=1, linetype = "dashed", color="black")+
  scale_color_manual(values=alpha(c("#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                     label=c("Spring","Summer","Fall","Winter"))+
  scale_y_continuous(limits = c(-10,60),breaks = seq(0,60,by=20))+
  scale_x_continuous(limits = c(-10,60),breaks = seq(0,60,by=20))+
  ylab(expression(LST[ParkingLot]~'('*degree*C*')'))+
  xlab(expression(LST[OpenSpace]~'('*degree*C*')'))+labs(color="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text = element_text(face="plain",size=10,color="black"),
                   axis.title = element_text(face="plain",size=10,color="black"),
                   legend.position = c(0.12,0.865),
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=9, face="plain"),
                   legend.title= element_text(colour="black", size=9, face="plain",margin = margin(b = -0.5)),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.2, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
plot_scatter_annual=ggplot(data= input_park_annual,aes(x=open,y=park,color=as.factor(season)))+
  geom_point(size=1)+
  geom_smooth(method="lm",formula=y~x,size=1,fill=NA)+
  geom_abline (slope=1, linetype = "dashed", color="black")+
  scale_color_manual(values=alpha(c("#5e4fa2"),.5),label=c("Annual"))+
  scale_y_continuous(limits = c(15,45),breaks = seq(10,50,by=10))+
  scale_x_continuous(limits = c(15,45),breaks = seq(10,50,by=10))+
  ylab(expression(LST[ParkingLot]~'('*degree*C*')'))+
  xlab(expression(LST[OpenSpace]~'('*degree*C*')'))+labs(color="")+
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.text = element_text(face="plain",size=10,color="black"),
                   axis.title = element_text(face="plain",size=10,color="black"),
                   legend.position = c(0.11,0.928),
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=9, face="plain"),
                   legend.title= element_text(colour="black", size=9, face="plain",margin = margin(b = -0.5)),
                   legend.key.size = unit(0.2, "cm"),
                   legend.key.width = unit(0.2, "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
plot_scatter_sum=ggplot() +
  coord_equal(xlim = c(0, 200), ylim = c(0, 100), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_scatter_annual), xmin = 0, xmax = 100, ymin = 0, ymax = 100) +  
  annotation_custom(ggplotGrob(plot_scatter_season), xmin = 100, xmax = 200, ymin = 0, ymax = 100) +  
  annotate("text", x =18, y =95, label = "(a)",size=4,family="serif")+
  annotate("text", x =118, y =95, label = "(b)",size=4,family="serif")+
  annotate("text", x =39, y =90, label = paste0(slope[1]),size=3,family="serif")+
  annotate("text", x =53, y =90, label = paste0(pvalue[1]),size=3,family="serif")+
  annotate("text", x =141, y =90, label = paste0(slope[2]),size=3,family="serif")+
  annotate("text", x =155, y =90, label = paste0(pvalue[2]),size=3,family="serif")+
  annotate("text", x =141, y =86.5, label = paste0(slope[3]),size=3,family="serif")+
  annotate("text", x =155, y =86.5, label = paste0(pvalue[3]),size=3,family="serif")+
  annotate("text", x =141, y =83, label = paste0(slope[4]),size=3,family="serif")+
  annotate("text", x =155, y =83, label = paste0(pvalue[4]),size=3,family="serif")+
  annotate("text", x =141, y =79.5, label = paste0(slope[5]),size=3,family="serif")+
  annotate("text", x =155, y =79.5, label = paste0(pvalue[5]),size=3,family="serif")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_s6.tif"),width=18,height=9, units = "cm", res = 300, compression = "lzw")
plot(plot_scatter_sum)
dev.off()
}
##slope(lst-isa) vs background lst###
{
  pvalue_diff=NA
  slope_diff=NA
  data_sum=foreach (ii=1:5,.combine=rbind) %do% {
    input_sel=data.frame(season=ii,
                         open=input[,paste0("LST_",seasons[ii],"_open")],
                         diff=input[,paste0("LST_diff_",seasons[ii],"_parking")])
    regression = lm(input_sel$diff~input_sel$open)
    slope_diff[ii] = paste0("Slope=",sprintf("%.2f",coef(regression)[2]))
    pvalue0= summary(regression)$coefficients[2,4]
    if (pvalue0<0.01) {pvalue1="P<0.01"} else if (pvalue0>0.01 & pvalue0<0.05) {pvalue1="P<0.05"} else {pvalue1=paste0("P=",sprintf("%.2f",pvalue0))}
    pvalue_diff[ii]= pvalue1
    input_sel
  }
  input_diff_annual=data_sum[data_sum$season==1,]
  input_diff_season=data_sum[data_sum$season %in% 2:5,]
  
  plot_scatter_season=ggplot(data= input_diff_season,aes(x=open,y=diff,color=as.factor(season)))+
    geom_point(size=1.5)+
    geom_smooth(method="lm",size=0.5,fill=NA)+
    scale_color_manual(values=alpha(c("#abdda4","#9e0142","#fdae61","#3288bd"),.5),
                       label=c("Spring","Summer","Fall","Winter"))+
    ylab(expression(Delta~LST~'('*degree*C*')'))+
    xlab(expression(LST[OpenSpace]~'('*degree*C*')'))+labs(color="")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.text = element_text(face="plain",size=10,color="black"),
                     axis.title = element_text(face="plain",size=10,color="black"),
                     legend.position = c(0.12,0.84),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=9, face="plain"),
                     legend.title= element_text(colour="black", size=9, face="plain",margin = margin(b = -0.5)),
                     legend.key.size = unit(0.4, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
  plot_scatter_annual=ggplot(data= input_diff_annual,aes(x=open,y=diff,color=as.factor(season)))+
    geom_point(size=1.5)+
    geom_smooth(method="lm",size=1,fill=NA)+
    scale_color_manual(values=alpha(c("#5e4fa2"),.5),label=c("Annual"))+
    ylab(expression(Delta~LST~'('*degree*C*')'))+
    xlab(expression(LST[OpenSpace]~'('*degree*C*')'))+labs(color="")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.text = element_text(face="plain",size=10,color="black"),
                     axis.title = element_text(face="plain",size=10,color="black"),
                     legend.position = "none",
                     #legend.position = c(0.12,0.915),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=9, face="plain"),
                     legend.title= element_text(colour="black", size=9, face="plain",margin = margin(b = -0.5)),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
}

#################################################
######### Map plotting######################
#########Parking lots#####
{
input0=data.frame(citys,input,lon=center[,1],lat=center[,2],ratio_con=input$con_annual/input$ratio)
MAX=round(apply(input0[,grepl("LST", names(input0))],2,max,na.rm=TRUE),1)+0.1
MIN=round(apply(input0[,grepl("LST", names(input0))],2,min,na.rm=TRUE),1)-0.1

plot_map_parking=list()
plot_map_diff_parking=list()
for (ii in 1:5) {
  season=seasons[ii]
  plot_map_parking [[ii]] = ggplot()+
    geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("LST_",season,"_parking"))),pch=21,size=2)+
    geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
    labs(fill=expression(LST~'('*degree*C*')'))+
    ylab("")+ xlab("")+ 
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.915,0.22),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.5,vjust = 2),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))+
    {if (ii==1) scale_fill_distiller(limits=c(MIN[1],MAX[1]),breaks = seq(20, 40,by=10),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
    {if (ii==2) scale_fill_distiller(limits=c(MIN[4],MAX[4]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
    {if (ii==3) scale_fill_distiller(limits=c(MIN[7],MAX[7]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
    {if (ii==4) scale_fill_distiller(limits=c(MIN[10],MAX[10]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
    {if (ii==5) scale_fill_distiller(limits=c(MIN[13],MAX[13]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
    {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                      axis.ticks = element_line(color = "black", linewidth =0.5))}

  plot_map_diff_parking[[ii]] = ggplot()+
    geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("LST_diff_",season,"_parking"))),pch=21,size=2)+
    geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
    labs(fill=expression(Delta~LST~'('*degree*C*')'))+
    ylab("")+ xlab("")+ 
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.915,0.22),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.7,vjust = 2),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))+
    {if (ii==1) scale_fill_gradient2(midpoint=0, limits=c(MIN[16],MAX[16]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==2) scale_fill_gradient2(midpoint=0, limits=c(MIN[19],MAX[19]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==3) scale_fill_gradient2(midpoint=0, limits=c(MIN[22],MAX[22]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==4) scale_fill_gradient2(midpoint=0, limits=c(MIN[25],MAX[25]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==5) scale_fill_gradient2(midpoint=0, limits=c(MIN[28],MAX[28]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
    {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                      axis.ticks = element_line(color = "black", linewidth =0.5))}
}
}
########Other built-up####
{
  plot_map_urban=list()
  plot_map_diff_urban=list()
  for (ii in 1:5) {
    season=seasons[ii]
    plot_map_urban [[ii]] = ggplot()+
      geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("LST_",season,"_urban"))),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
      labs(fill=expression(LST~'('*degree*C*')'))+
      ylab("")+ xlab("")+ 
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = c(0.915,0.22),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.5,vjust = 2),
                       legend.key.size = unit(0.2, "cm"),
                       legend.key.width = unit(0.2, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))+
      {if (ii==1) scale_fill_distiller(limits=c(MIN[2],MAX[2]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
      {if (ii==2) scale_fill_distiller(limits=c(MIN[5],MAX[5]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==3) scale_fill_distiller(limits=c(MIN[8],MAX[8]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==4) scale_fill_distiller(limits=c(MIN[11],MAX[11]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
      {if (ii==5) scale_fill_distiller(limits=c(MIN[14],MAX[14]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}
    
    plot_map_diff_urban[[ii]] = ggplot()+
      geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("LST_diff_",season,"_urban"))),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
      scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA)+
      labs(fill=expression(Delta~LST~'('*degree*C*')'))+
      ylab("")+ xlab("")+ 
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = c(0.915,0.22),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.5),hjust = 0.7,vjust = 2),
                       legend.key.size = unit(0.2, "cm"),
                       legend.key.width = unit(0.2, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))+
      {if (ii==1) scale_fill_gradient2(midpoint=0, limits=c(MIN[18],MAX[18]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
      {if (ii==2) scale_fill_gradient2(midpoint=0, limits=c(MIN[21],MAX[21]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
      {if (ii==3) scale_fill_gradient2(midpoint=0, limits=c(MIN[24],MAX[24]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
      {if (ii==4) scale_fill_gradient2(midpoint=0, limits=c(MIN[27],MAX[27]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}+
      {if (ii==5) scale_fill_gradient2(midpoint=0, limits=c(MIN[30],MAX[30]),low="blue",mid="white",high="red", space ="Lab",na.value = NA)}
  }
}
#########Green spaces#####
{
plot_map_open=list()
for (ii in 1:5) {
  season=seasons[ii]
  plot_map_open[[ii]] = ggplot()+
    geom_point(data=input0,aes(x=lon, y=lat, fill=!!sym(paste0("LST_",season,"_open"))),pch=21,size=2)+
    geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
    labs(fill=expression(LST~'('*degree*C*')'))+
    ylab("")+ xlab("")+ 
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     axis.title=element_blank(),
                     axis.text=element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.915,0.22),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.3),hjust = 0.5,vjust = 2),
                     legend.key.size = unit(0.2, "cm"),
                     legend.key.width = unit(0.2, "cm"),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))+
    {if (ii==1) scale_fill_distiller(limits=c(MIN[3],MAX[3]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+
    {if (ii==2) scale_fill_distiller(limits=c(MIN[6],MAX[6]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
    {if (ii==3) scale_fill_distiller(limits=c(MIN[9],MAX[9]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
    {if (ii==4) scale_fill_distiller(limits=c(MIN[12],MAX[12]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}+ 
    {if (ii==5) scale_fill_distiller(limits=c(MIN[15],MAX[15]),type = "div", palette = "RdYlBu",direction = -1,na.value=NA)}
  }
}
#################################################
###########plot latitude change ##############
{
matrix=data.frame(lat=input$lat1,
                  lst=input$LST_diff_annual_parking,
                  uhi=input$uhi_annual,
                  con=input$con_annual)
lst_lat <- ggplot(matrix, aes(x = lat, y =lst)) +
  geom_point(size=0.5,color="#5e4fa2",alpha=0.5) + labs(y=expression(Delta~LST~'('*degree*C*')'),
                              x=expression(Latitude~'('*degree*')'))+
  geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.line = element_line(colour = "black",size=0.2),
                   panel.border = element_blank(),
                   panel.grid = element_blank(),
                   plot.background = element_rect(fill='transparent', color=NA),
                   plot.margin = unit(c(0,0,-0.2,-0.1), "cm"),
                   axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                   axis.ticks.length=unit(.05, "cm"),
                   axis.text.y = element_text(face="plain",size=6,color="black",hjust = 10),
                   axis.text.x = element_text(face="plain",size=6,color="black",vjust = 2),
                   axis.title.x =  element_text(face="plain",size=6,color="black",vjust = 5),
                   axis.title.y =  element_text(face="plain",size=6,color="black",vjust = -2),
                   plot.title = element_blank())+  coord_flip()

uhi_lat <- ggplot(matrix, aes(x = lat, y =uhi)) +
  geom_point(size=0.5,color="#5e4fa2",alpha=0.5) + labs(y=expression(Delta~SUHI~'('*degree*C*')'),
                              x=expression(Latitude~'('*degree*')'))+
  geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.line = element_line(colour = "black",size=0.2),
                   panel.border = element_blank(),
                   panel.grid = element_blank(),
                   plot.background = element_rect(fill='transparent', color=NA),
                   plot.margin = unit(c(0,0,-0.2,-0.1), "cm"),
                   axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                   axis.ticks.length=unit(.05, "cm"),
                   axis.text.y = element_text(face="plain",size=6,color="black",hjust = 10),
                   axis.text.x = element_text(face="plain",size=6,color="black",vjust = 2),
                   axis.title.x =  element_text(face="plain",size=6,color="black",vjust = 5),
                   axis.title.y =  element_text(face="plain",size=6,color="black",vjust = -2),
                   plot.title = element_blank())+  coord_flip()

con_lat <- ggplot(matrix, aes(x = lat, y =con)) +
  geom_point(size=0.5,color="#5e4fa2",alpha=0.5) + labs(y=expression(CP~'(%)'),
                              x=expression(Latitude~'('*degree*')'))+
  geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
  theme_bw()+theme(text=element_text(family="serif"),
                   axis.line = element_line(colour = "black",size=0.2),
                   panel.border = element_blank(),
                   panel.grid = element_blank(),
                   plot.background = element_rect(fill='transparent', color=NA),
                   plot.margin = unit(c(0,0,-0.2,-0.1), "cm"),
                   axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                   axis.ticks.length=unit(.05, "cm"),
                   axis.text.y = element_text(face="plain",size=6,color="black",hjust = 10),
                   axis.text.x = element_text(face="plain",size=6,color="black",vjust = 2),
                   axis.title.x =  element_text(face="plain",size=6,color="black",vjust = 5),
                   axis.title.y =  element_text(face="plain",size=6,color="black",vjust = -2),
                   plot.title = element_blank())+  coord_flip()
}

#################################################
#####summarize & export figures########
{
plot_sum_sum=ggplot() +
  coord_equal(xlim = c(0, 240), ylim = c(0, 160), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_bar_lst), xmin = 0, xmax = 120, ymin = 80, ymax = 160) +  
  annotation_custom(ggplotGrob(plot_bar_lst_diff), xmin = 120, xmax = 240, ymin = 80, ymax = 160) +  
  annotation_custom(ggplotGrob(plot_bar_uhi), xmin = 1, xmax = 120, ymin = 0, ymax = 80) +  
  annotation_custom(ggplotGrob(plot_bar_con), xmin = 120, xmax = 240, ymin = 0, ymax = 80) +
  annotate("text", x =19.5, y =155, label = "(a)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =139, y =155, label = "(b)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =19, y =75, label = "(c)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =139, y =75, label = "(d)",colour = "black",size=4,family="serif",fontface ="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_2.tif"),width=18,height=12, units = "cm", res = 300, compression = "lzw")
plot(plot_sum_sum)
dev.off()
pdf( file = "../figure/Fig_sum_2.pdf",
     width = 18/2.54,
     height = 12 /2.54,
     onefile = FALSE,
     paper = "special",
     colormodel = "srgb",
     useDingbats = FALSE)
plot(plot_sum_sum)
dev.off()
plot_scatter_sum=ggplot() +
  coord_equal(xlim = c(0, 180), ylim = c(0, 120), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_map_diff_parking[[1]]), xmin = 2, xmax = 88, ymin = 60, ymax = 120) +  
  annotation_custom(ggplotGrob(lst_lat), xmin = 10, xmax = 25, ymin = 67.5, ymax = 84) +  
  annotation_custom(ggplotGrob(plot_scatter_annual), xmin = 90, xmax = 180, ymin = 56, ymax = 119) +
  annotation_custom(ggplotGrob(plot_map_uhi[[1]]), xmin = 2, xmax = 88, ymin = 0, ymax = 60) +  
  annotation_custom(ggplotGrob(uhi_lat), xmin = 10, xmax = 25, ymin = 7.5, ymax = 24) +  
  annotation_custom(ggplotGrob(plot_map_con[[1]]), xmin = 92, xmax = 178, ymin = 0, ymax = 60) +
  annotation_custom(ggplotGrob(con_lat), xmin = 100, xmax = 115, ymin = 7.4, ymax = 24) +  
  annotate("text", x =14, y =115, label = "(a)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =104, y =115, label = "(b)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =14, y =55, label = "(c)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =104, y =55, label = "(d)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =156, y =115, label = paste0(slope_diff[1]),size=3.2,family="serif")+
  annotate("text", x =170, y =115, label = paste0(pvalue_diff[1]),size=3.2,family="serif")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_3.tif"),width=18,height=12, units = "cm", res = 300, compression = "lzw")
plot(plot_scatter_sum)
dev.off()
pdf( file = "../figure/Fig_sum_3.pdf",
     width = 18/2.54,
     height = 12 /2.54,
     onefile = FALSE,
     paper = "special",
     colormodel = "srgb",
     useDingbats = FALSE)
plot(plot_scatter_sum)
dev.off()
plot_sum_sum=ggplot() +
  coord_equal(xlim = c(0, 240), ylim = c(0, 140), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_cool_isa_uhi_single_y), xmin = 0, xmax = 120, ymin = 70, ymax = 140) +  
  annotation_custom(ggplotGrob(plot_cool_area_uhi_single_y), xmin = 120, xmax = 240, ymin = 70, ymax = 140) +  
  annotation_custom(ggplotGrob(plot_map_uhi_delta_isa[[1]]), xmin = 4, xmax = 120, ymin = 0, ymax = 70) +  
  annotation_custom(ggplotGrob(plot_map_uhi_delta_area[[1]]), xmin = 124, xmax = 240, ymin = 0, ymax = 70) +
  annotation_custom(ggplotGrob(cooling_isa_lat), xmin = 15, xmax = 35, ymin = 4.5, ymax = 28) +  
  annotation_custom(ggplotGrob(cooling_area_lat), xmin = 135, xmax = 155, ymin = 4.5, ymax = 28) +
  annotate("text", x =113, y =135, label = "(a)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =233, y =135, label = "(c)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =113, y =67, label = "(b)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =233, y =67, label = "(d)",colour = "black",size=4,family="serif",fontface ="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_5.tif"),width=20,height=12.2, units = "cm", res = 300, compression = "lzw")
plot(plot_sum_sum)
dev.off()
pdf( file = "../figure/Fig_sum_5.pdf",
     width = 20/2.54,
     height = 12.2 /2.54,
     onefile = FALSE,
     paper = "special",
     colormodel = "srgb",
     useDingbats = FALSE)
plot(plot_sum_sum)
dev.off()
plot_scatter_sum=ggplot() +
  coord_equal(xlim = c(0, 80), ylim = c(0, 70), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_scatter_season), xmin = 0, xmax = 80, ymin = 0, ymax = 70) +  
  annotate("text", x =31, y =62, label = paste0(slope_diff[2]),size=3.2,family="serif")+
  annotate("text", x =43, y =62, label = paste0(pvalue_diff[2]),size=3.2,family="serif")+
  annotate("text", x =31, y =59, label = paste0(slope_diff[3]),size=3.2,family="serif")+
  annotate("text", x =43, y =59, label = paste0(pvalue_diff[3]),size=3.2,family="serif")+
  annotate("text", x =31, y =56, label = paste0(slope_diff[4]),size=3.2,family="serif")+
  annotate("text", x =43, y =56, label = paste0(pvalue_diff[4]),size=3.2,family="serif")+
  annotate("text", x =31, y =53, label = paste0(slope_diff[5]),size=3.2,family="serif")+
  annotate("text", x =43, y =53, label = paste0(pvalue_diff[5]),size=3.2,family="serif")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_s7.tif"),width=10,height=8.75, units = "cm", res = 300, compression = "lzw")
plot(plot_scatter_sum)
dev.off()

plot_sum_sum=ggplot() +
  coord_equal(xlim = c(0, 160), ylim = c(0, 70), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_cool_sensitivity), xmin = 0, xmax = 60, ymin = 0, ymax = 70) +  
  annotation_custom(ggplotGrob(plot_map_lst_delta_isa[[1]]), xmin = 60, xmax = 160, ymin = 2, ymax = 72) +  
  annotate("text", x =14, y =66, label = "(a)",colour = "black",size=4,family="serif",fontface ="bold")+
  annotate("text", x =72, y =66, label = "(b)",colour = "black",size=4,family="serif",fontface ="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_s12.tif"),width=16,height=7, units = "cm", res = 300, compression = "lzw")
plot(plot_sum_sum)
dev.off()

plot_sum_map=ggplot() +
  coord_equal(xlim = c(0, 245), ylim = c(0, 205), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_map_parking[[2]]), xmin = 5, xmax = 85, ymin = 150, ymax = 200) +
  annotation_custom(ggplotGrob(plot_map_urban[[2]]), xmin = 85, xmax = 165, ymin = 150, ymax = 200) +
  annotation_custom(ggplotGrob(plot_map_open[[2]]), xmin = 165, xmax = 245, ymin = 150, ymax = 200) +
  annotation_custom(ggplotGrob(plot_map_parking[[3]]), xmin = 5, xmax = 85, ymin = 100, ymax = 150) +
  annotation_custom(ggplotGrob(plot_map_urban[[3]]), xmin = 85, xmax = 165, ymin = 100, ymax = 150) +
  annotation_custom(ggplotGrob(plot_map_open[[3]]), xmin = 165, xmax = 245, ymin = 100, ymax = 150) +
  annotation_custom(ggplotGrob(plot_map_parking[[4]]), xmin = 5, xmax = 85, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_map_urban[[4]]), xmin = 85, xmax = 165, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_map_open[[4]]), xmin = 165, xmax = 245, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_map_parking[[5]]), xmin = 5, xmax = 85,ymin = 0, ymax = 50) +
  annotation_custom(ggplotGrob(plot_map_urban[[5]]), xmin = 85, xmax = 165, ymin = 0, ymax = 50) +
  annotation_custom(ggplotGrob(plot_map_open[[5]]), xmin = 165, xmax = 245, ymin = 0, ymax = 50) +
  annotate("text", x =3, y =25, label = "Winter",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =75, label = "Fall",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =125, label = "Summer",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =175, label = "Spring",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =42.5, y =202.5, label = expression(LST[ParkingLot]),colour = "black",size=4,family="serif")+
  annotate("text", x =122.5, y =202.5, label = expression(LST[OtherBuilt-up]),colour = "black",size=4,family="serif")+
  annotate("text", x =202.5, y =202.5, label = expression(LST[OpenSpace]),colour = "black",size=4,family="serif")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_extend1.tif"),width=21.5,height=18, units = "cm", res = 300, compression = "lzw")
plot(plot_sum_map)
dev.off()

plot_sum_map=ggplot() +
  coord_equal(xlim = c(0, 165), ylim = c(0, 205), expand = FALSE) +
  annotation_custom(ggplotGrob(plot_map_diff_parking[[2]]), xmin = 5, xmax = 85, ymin = 150, ymax = 200) +  
  annotation_custom(ggplotGrob(plot_map_diff_urban[[2]]), xmin = 85, xmax = 165, ymin = 150, ymax = 200) +
  annotation_custom(ggplotGrob(plot_map_diff_parking[[3]]), xmin = 5, xmax = 85, ymin = 100, ymax = 150) +  
  annotation_custom(ggplotGrob(plot_map_diff_urban[[3]]), xmin = 85, xmax = 165, ymin = 100, ymax = 150) +
  annotation_custom(ggplotGrob(plot_map_diff_parking[[4]]), xmin = 5, xmax = 85, ymin = 50, ymax = 100) +  
  annotation_custom(ggplotGrob(plot_map_diff_urban[[4]]), xmin = 85, xmax = 165, ymin = 50, ymax = 100) +
  annotation_custom(ggplotGrob(plot_map_diff_parking[[5]]), xmin = 5, xmax = 85,ymin = 0, ymax = 50) +  
  annotation_custom(ggplotGrob(plot_map_diff_urban[[5]]), xmin = 85, xmax = 165, ymin = 0, ymax = 50) +
  annotate("text", x =3, y =225, label = "Annual",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =25, label = "Winter",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =75, label = "Fall",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =125, label = "Summer",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =3, y =175, label = "Spring",angle=90,colour = "black",size=4,family="serif")+
  annotate("text", x =42.5, y =202.5, label = expression(Delta~LST[ParkingLot]),colour = "black",size=4,family="serif")+
  annotate("text", x =122.5, y =202.5, label = expression(Delta~LST[OtherBuilt-up]),colour = "black",size=4,family="serif")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_extend2.tif"),width=16,height=20, units = "cm", res = 300, compression = "lzw")
plot(plot_sum_map)
dev.off()

#########################
##change the input to 10m data: line 34 "parking_project_gee_v2"
# plot_sum_sum = ggplot() +
#   coord_equal(xlim = c(0, 240), ylim = c(0, 160), expand = FALSE) +
#   annotation_custom(ggplotGrob(plot_bar_lst), xmin = 0, xmax = 120, ymin = 80, ymax = 160) +
#   annotation_custom(ggplotGrob(plot_bar_lst_diff), xmin = 120, xmax = 240, ymin = 80, ymax = 160) +
#   annotation_custom(ggplotGrob(plot_map_diff_parking[[1]]), xmin = 5, xmax = 118, ymin = 5, ymax = 80) +
#   annotation_custom(ggplotGrob(lst_lat), xmin = 15, xmax = 35, ymin = 13, ymax = 35) +
#   annotation_custom(ggplotGrob(plot_scatter_annual), xmin = 120, xmax = 240, ymin = -1, ymax = 80) +
#   annotate("text", x =20, y =155, label = "(a)",colour = "black",size=4,family="serif",fontface ="bold")+
#   annotate("text", x =140, y =155, label = "(b)",colour = "black",size=4,family="serif",fontface ="bold")+
#   annotate("text", x =20, y =75, label = "(c)",colour = "black",size=4,family="serif",fontface ="bold")+
#   annotate("text", x =140, y =75, label = "(d)",colour = "black",size=4,family="serif",fontface ="bold")+
#   theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
#   theme_void()
# tiff(paste0("../figure/Fig_sum_s3.tif"),width=20,height=12, units = "cm", res = 300, compression = "lzw")
# plot(plot_sum_sum)
# dev.off()
}