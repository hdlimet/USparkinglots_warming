library(ggplot2)
library(terra)
library(maptiles)
library(tidyterra)
library(ggExtra)
library(tigris)
library(sf)
library(stars)
rm(list = ls())

##############readme################################
##plot figure s8, s9 for four cities with largest parking areas ratio 
###################################################


#################################################
####Global variables########
{
seasons= c("annual","spring","summer","fall","winter")
seasons1= c("spring","summer","fall","winter")
us_states <- states(cb = TRUE, resolution = "20m") %>%  shift_geometry()
parking=st_transform(st_read(dsn="../input/raw_shapefile",layer="parkinglots"),crs=st_crs(us_states))
parking=parking[-which(parking$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
boundary=st_transform(st_read(dsn="../input/raw_shapefile",layer="boundary"),crs=st_crs(us_states))
boundary=boundary[-which(boundary$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
citys=c("san-bernardino-ca","arlington-tx","lexington-ky","columbia-sc",
        "boston-ma","washington-dc","san-francisco-ca","new-york-ny")
cityss=c("San Bernardino, CA","Arlington, TX","Lexington, KY","Columbia, SC",
         "boston-ma","washington-dc","san-francisco-ca","new-york-ny")
}

#################################################
###plot maps of LST and ISA
{
##adjust the extent of boundary for map ploting
xdy0=NA
xdy0[2:8]=1.3
xdy0[1]=1.25
xadjust=rep(0,8)
yadjust=rep(0,8)
for (id in 1:length(citys)) {
  shp_boundary_sel=boundary[boundary$city %in% citys[id],]
  xrange=st_bbox(shp_boundary_sel)$xmax-st_bbox(shp_boundary_sel)$xmin
  yrange=st_bbox(shp_boundary_sel)$ymax-st_bbox(shp_boundary_sel)$ymin
  if (xrange/yrange<xdy0[id]) {
   xadjust[id]=(xdy0[id]*yrange-xrange)/2
  } else {
   yadjust[id]=(xrange/xdy0[id]-yrange)/2
  }
}

LST_map=list()
ISA_map=list()
for (id in 1:4) {
  shp_park_sel=parking[parking$city==citys[id],]
  shp_boundary_sel=boundary[boundary$city==citys[id],]
  grid_box <-st_as_sfc(st_bbox(c(st_bbox(shp_boundary_sel)$xmin-xadjust[id], 
                                 st_bbox(shp_boundary_sel)$xmax+xadjust[id],
                                 st_bbox(shp_boundary_sel)$ymin-yadjust[id],
                                 st_bbox(shp_boundary_sel)$ymax+yadjust[id]), 
                        crs = st_crs(shp_boundary_sel)))
  dc<- get_tiles(grid_box, provider = "Esri.WorldStreetMap", zoom = 16)
  
  LST=rast(paste0("../input/parking_project_gee/LST_annual_",citys[id],".tif"))-273.15
  LST_parking <- mask(project(LST,st_crs(boundary)$proj4string),shp_park_sel)
  LST_parking_1=as.data.frame(LST_parking, xy=TRUE)
  names(LST_parking_1)=c("lon","lat","value")
    
  ISA=rast(paste0("../input/parking_project_gee/ISA_",citys[id],".tif"))
  ISA_parking <- mask(project(ISA,st_crs(boundary)$proj4string),shp_park_sel)
  ISA_parking_1=as.data.frame(ISA_parking, xy=TRUE)
  names(ISA_parking_1)=c("lon","lat","value")
  
  LST_map[[id]] = ggplot()+
    geom_spatraster_rgb(data = dc, r = 1, g = 1, b = 1) +
    geom_sf(data=shp_boundary_sel,color = "black", fill = "gray",size=0.1,alpha=0.5)+
    geom_raster(data=LST_parking_1, aes(lon, lat, fill=value))+
    geom_sf(data=shp_park_sel,color = "black", fill = "NA",linewidth =0.3, alpha=0.1)+
    coord_sf(crs = st_crs(boundary),expand=TRUE,
             xlim=c((st_bbox(shp_boundary_sel)$xmin-xadjust[id]),
                    (st_bbox(shp_boundary_sel)$xmax+xadjust[id])),
             ylim=c((st_bbox(shp_boundary_sel)$ymin-yadjust[id]),
                    (st_bbox(shp_boundary_sel)$ymax+yadjust[id])))+
    ylab("") + xlab("")+ labs(fill=expression(LST~'('*degree*C*')'))+
    theme_bw()+theme(text=element_text(family="serif"),
                     axis.title =element_blank(),
                     axis.text=element_blank(),
                     axis.ticks =element_blank(),
                     panel.grid = element_blank(),
                     panel.border = element_blank(),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     legend.position = c(0.88,0.85),
                     legend.background = element_rect(fill="transparent", linetype="blank"),
                     legend.text = element_text(colour="black", size=7, face="plain"),
                     legend.title= element_text(colour="black", size=7, face="plain",vjust=-1.5,hjust=0.5),
                     legend.key.size = unit(0.15, "cm"),
                     legend.key.width = unit(0.15, "cm"),
                     plot.title =  element_blank()) +
    {if (id==2) scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                                    breaks =seq(33,37,by=2))}+
    {if (id==4) scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                                     breaks =seq(28,32,by=2))}+
    {if (id==3) scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                                    breaks =seq(27,31,by=2))}+
    {if (id==1) scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                                     breaks =seq(36,42,by=3))}
    ISA_map[[id]] = ggplot()+
    geom_spatraster_rgb(data = dc, r = 1, g = 1, b = 1)+
    geom_sf(data=shp_boundary_sel,color = "black", fill = "gray",size=0.1,alpha=0.5)+
    geom_raster(data=ISA_parking_1, aes(lon, lat, fill=value))+
    geom_sf(data=shp_park_sel,color = "black", fill = "NA",linewidth =0.3, alpha=0.1)+
    scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                         limits = c(0,100),breaks =seq(30,90,by=30))+ 
    coord_sf(crs = st_crs(boundary),expand=TRUE,
             xlim=c((st_bbox(shp_boundary_sel)$xmin-xadjust[id]),
                    (st_bbox(shp_boundary_sel)$xmax+xadjust[id])),
             ylim=c((st_bbox(shp_boundary_sel)$ymin-yadjust[id]),
                    (st_bbox(shp_boundary_sel)$ymax+yadjust[id])))+
    ylab("") + xlab("")+ labs(fill="ISA (%)")+
    theme_bw()+theme(text=element_text(family="serif"),
                     axis.title =element_blank(),
                     axis.text=element_blank(),
                     axis.ticks =element_blank(),
                     panel.grid = element_blank(),
                     panel.border = element_blank(),
                     plot.margin = unit(c(0,0,0,0), "cm"),
                     legend.position = c(0.88,0.85),
                     legend.background = element_rect(fill="transparent", linetype="blank"),
                     legend.text = element_text(colour="black", size=7, face="plain"),
                     legend.title= element_text(colour="black", size=7, face="plain",vjust=-1,hjust=0.5),
                     legend.key.size = unit(0.15, "cm"),
                     legend.key.width = unit(0.15, "cm"),
                     plot.title =  element_blank())
}
}

#################################################
####scatter between LST and ISA 
{
####input####
{
input0=rbind(read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_1_2024.csv")),
              read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_2_2024.csv")),
              read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_3_2024.csv")),
              read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_4_2024.csv")))

  input0 = input0[-which(input0$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
  input0[,4:8]=input0[,4:8]-273.15
  ID=c(84,6,47,22)  ##four cities
}
####seasonal####
{
  plot_scatter_isa_sum=list()
  for (id in 1:length(ID)) {
    cat (id,"\n")
    input=input0[input0$city_id==ID[id],]
    plot_scatter_isa=list()
    for (ii in 1:4) {
      regression = lm(input[,paste0("LST_",seasons1[ii],"_parking")]~input$ISA_parking)
      slope = paste0("Slope=",sprintf("%.2f",coef(regression)[2]))
      pvalue0= summary(regression)$coefficients[2,4]
      if (pvalue0<0.01) {pvalue1="P<0.01"} else if (pvalue0>0.01 & pvalue0<0.05) {pvalue1="P<0.05"} else {pvalue1=paste0("P=",sprintf("%.2f",pvalue0))}
      plot_scatter_isa[[ii]] = ggplot(data= input,
                                      aes(x=ISA_parking,
                                          y=!!sym(paste0("LST_",seasons1[ii],"_parking"))))+
        geom_point(size=1,shape = 16)+
        geom_smooth(method="lm",formula=y~x,size=1,fill=NA,color="red")+
        ylab(expression(LST[ParkingLot]~'('*degree*C*')'))+
        xlab(expression(ISA[ParkingLot]~'(%)'))+
        geom_text(label=paste0(slope),aes(x= -Inf,y = Inf), hjust = -0.08,vjust =1.2,
                  colour = "black",fontface = "plain",size=3,family="serif")+
        geom_text(label=paste0(pvalue1),aes(x= -Inf,y = Inf), hjust = -0.15,vjust =2.6,
                  colour = "black",fontface = "plain",size=3,family="serif")+
        theme_bw()+theme(text=element_text(family="serif"),
                         panel.grid = element_blank(),
                         axis.title = element_blank(),
                         axis.text = element_text(face="plain",size=10,color="black"),
                         legend.position = "none")+
        {if (ii==1) theme(axis.title.y = element_text(face="plain",size=10))}+
        {if (id==4) theme(axis.title.x = element_text(face="plain",size=10))}
    }
    if (id<4) {    
      plot_scatter_isa_sum[[id]]=ggplot() +
        coord_equal(xlim = c(0, 146), ylim = c(0, 45), expand = FALSE) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[1]]), xmin = 0, xmax = 41, ymin = 0, ymax = 45) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[2]]), xmin = 41, xmax = 76, ymin = 0, ymax = 45) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[3]]), xmin = 76, xmax = 111, ymin = 0, ymax = 45) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[4]]), xmin = 111, xmax = 146,ymin = 0, ymax = 45) +
        theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
        theme_void()
    } else {
      plot_scatter_isa_sum[[id]]=ggplot() +
        coord_equal(xlim = c(0, 146), ylim = c(0, 50), expand = FALSE) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[1]]), xmin = 0, xmax = 41, ymin = 0, ymax = 50) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[2]]), xmin = 41, xmax = 76, ymin = 0, ymax = 50) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[3]]), xmin = 76, xmax = 111, ymin = 0, ymax = 50) +
        annotation_custom(ggplotGrob(plot_scatter_isa[[4]]), xmin = 111, xmax = 146,ymin = 0, ymax = 50) +
        theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
        theme_void()
    }
  }
  plot_sum=ggplot() +
    coord_equal(xlim = c(0, 151), ylim = c(0, 190), expand = FALSE) +
    annotation_custom(ggplotGrob(plot_scatter_isa_sum[[1]]), xmin = 5, xmax = 151, ymin = 140, ymax = 185) +
    annotation_custom(ggplotGrob(plot_scatter_isa_sum[[2]]), xmin = 5, xmax = 151, ymin = 95, ymax = 140) +
    annotation_custom(ggplotGrob(plot_scatter_isa_sum[[3]]), xmin = 5, xmax = 151, ymin = 50, ymax = 95) +
    annotation_custom(ggplotGrob(plot_scatter_isa_sum[[4]]), xmin = 5, xmax = 151, ymin = 0, ymax = 50) +
    annotate("text", x =32, y =187, label = "Spring",size=4,family="serif")+
    annotate("text", x =65, y =187, label = "Summer",size=4,family="serif")+
    annotate("text", x =102, y =187, label = "Fall",size=4,family="serif")+
    annotate("text", x =135, y =187, label = "Winter",size=4,family="serif")+
    annotate("text", x =2, y =165, label = paste0(cityss[1]),angle=90,size=4,family="serif")+
    annotate("text", x =2, y =120, label = paste0(cityss[2]),angle=90,size=4,family="serif")+
    annotate("text", x =2, y =75, label = paste0(cityss[3]),angle=90,size=4,family="serif")+
    annotate("text", x =2, y =30, label = paste0(cityss[4]),angle=90,size=4,family="serif")+
    theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
    theme_void()
  tiff(paste0("../figure/Fig_sum_s9.tif"),width=15,height=19, units = "cm", res = 300, compression = "lzw")
  plot(plot_sum)
  dev.off()
}  
####annual####
{
plot_scatter_isa1=list()
  for (id in 1:length(ID)) {
  cat (id,"\n")
  input=input0[input0$city_id==ID[id],]
  ii=1
    regression = lm(input[,paste0("LST_",seasons[ii],"_parking")]~input$ISA_parking)
    slope = paste0("Slope=",sprintf("%.2f",coef(regression)[2]))
    pvalue0= summary(regression)$coefficients[2,4]
    if (pvalue0<0.01) {pvalue1="P<0.01"} else if (pvalue0>0.01 & pvalue0<0.05) {pvalue1="P<0.05"} else {pvalue1=paste0("P=",sprintf("%.2f",pvalue0))}
    plot_scatter_isa1[[id]] = ggplot(data= input,
                                    aes(x=ISA_parking,
                                        y=!!sym(paste0("LST_",seasons[ii],"_parking"))))+
      geom_point(size=1,shape = 16)+
      geom_smooth(method="lm",formula=y~x,size=1,fill=NA,color="red")+
      ylab(expression(LST~'('*degree*C*')'))+
      xlab(expression(ISA~'(%)'))+
      geom_text(label=paste0(slope),aes(x= -Inf,y = Inf), hjust = -0.08,vjust =1.2,
                colour = "black",fontface = "plain",size=3,family="serif")+
      geom_text(label=paste0(pvalue1),aes(x= -Inf,y = Inf), hjust = -0.15,vjust =2.6,
                colour = "black",fontface = "plain",size=3,family="serif")+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.text = element_text(face="plain",size=10,color="black"),
                       axis.title = element_text(face="plain",size=10,color="black"),
                       legend.position = "none")
  }
}
}

#################################################
##combine figures
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 400), ylim = c(0, 266), expand = FALSE) +
  annotation_custom(ggplotGrob(LST_map[[1]]), xmin = 0, xmax = 100, ymin = 180, ymax = 260) +  
  annotation_custom(ggplotGrob(LST_map[[2]]), xmin = 100, xmax = 200, ymin = 180, ymax = 260) +  
  annotation_custom(ggplotGrob(LST_map[[3]]), xmin = 200, xmax = 300, ymin = 180, ymax = 260) +  
  annotation_custom(ggplotGrob(LST_map[[4]]), xmin = 300, xmax = 400, ymin = 180, ymax = 260) +  
  annotation_custom(ggplotGrob(ISA_map[[1]]), xmin = 0, xmax = 100, ymin = 100, ymax = 180) +
  annotation_custom(ggplotGrob(ISA_map[[2]]), xmin = 100, xmax = 200, ymin = 100, ymax = 180) +
  annotation_custom(ggplotGrob(ISA_map[[3]]), xmin = 200, xmax = 300, ymin = 100, ymax = 180) +
  annotation_custom(ggplotGrob(ISA_map[[4]]), xmin = 300, xmax = 400, ymin = 100, ymax = 180) +
  annotation_custom(ggplotGrob(plot_scatter_isa1[[1]]), xmin = 0, xmax = 100, ymin = 0, ymax = 100) +  
  annotation_custom(ggplotGrob(plot_scatter_isa1[[2]]), xmin = 100, xmax = 200, ymin = 0, ymax = 100) +  
  annotation_custom(ggplotGrob(plot_scatter_isa1[[3]]), xmin = 200, xmax = 300, ymin = 0, ymax = 100) +  
  annotation_custom(ggplotGrob(plot_scatter_isa1[[4]]), xmin = 300, xmax = 400, ymin = 0, ymax = 100) +  
  annotate("rect", xmin = 2, xmax = 398, ymin = 181, ymax = 259, color="black",fill=NA,size=0.3)+
  annotate("rect", xmin = 2, xmax = 398, ymin = 101, ymax = 179, color="black",fill=NA,size=0.3)+
  annotate("rect", xmin = 2, xmax = 398, ymin = 1, ymax = 99, color="black",fill=NA,size=0.3)+
  annotate("text", x =7, y =255, label = "(a)",size=4,family="serif")+
  annotate("text", x =7, y =175, label = "(b)",size=4,family="serif")+
  annotate("text", x =7, y =95, label = "(c)",size=4,family="serif")+
  annotate("text", x =50,  y =263.5, label = paste0(cityss[1]),size=4,family="serif")+
  annotate("text", x =150, y =263.5, label = paste0(cityss[2]),size=4,family="serif")+
  annotate("text", x =250, y =263.5, label = paste0(cityss[3]),size=4,family="serif")+
  annotate("text", x =350, y =263.5, label = paste0(cityss[4]),size=4,family="serif")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_s8.tif"),width=20,height=13.3, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()
