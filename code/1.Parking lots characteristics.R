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
##plot figure 1: mean ISA and area ratio across U.S.
###plot s1,s2,s5
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
}

#################################################
#input data from GEE

{
    area=read.csv("../input/Parking_Lot_LULC_area_sum.csv")
    area=area[-which(area$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
    ratio=area$parking/area$boundary*100

    input0=read.csv("../input/parking_project_gee/Parking_Lot_city_mean_2024_buffer_2.csv")
    input0[,2:16]=input0[,2:16]-273.15
    input0 = input0[-which(input0$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
    citys=boundary$city
    
    us_states <- us_states[-which(us_states$NAME %in% c("Alaska","Hawaii","Puerto Rico")),]
    center <- st_transform(boundary,crs=st_crs(us_states)) %>% st_centroid() %>% st_geometry() %>% st_coordinates()
    center1 <- st_transform(boundary,crs=4326) %>% st_centroid() %>% st_geometry() %>% st_coordinates()
    input1=cbind(input0,ratio,lon=center[,1],lat=center[,2],id=1:nrow(input0),lat1=center1[,2])
    
    input1 %>% arrange(desc(ratio))
    input1[c((input1 %>% arrange(desc(ratio)))$id[1:5]),]$ratio
    input1[c((input1 %>% arrange(desc(ratio)))$id[1:5]),]$city
    input1[c((input1 %>% arrange(desc(ratio)))$id[95:100]),]$ratio
    input1[c((input1 %>% arrange(desc(ratio)))$id[95:100]),]$city
  }

#figure: parking areas/percent
{
library(dplyr)
library(stringr)
library(ggplot2)

# Convert city-state formatting: remove dash, capitalize both words
area <- area %>%
  mutate(
    # separate city and state based on the final "-ST"
    city_part  = sub("-[A-Za-z]{2}$", "", city),   # everything before -ST
    state_part = sub(".*-", "", city),             # last two letters after -
    
    # force full city to lower case first
    city_part  = tolower(city_part),
    
    # capitalize ONLY the first character of the entire city string
    city_part  = paste0(
      toupper(substr(city_part, 1, 1)),
      substr(city_part, 2, nchar(city_part))
    ),
    
    # state abbreviation: always two uppercase letters
    state_part = toupper(state_part),
    
    # final label "City name, ST"
    city_clean = paste0(city_part, ", ", state_part),
    
    # parking percentage
    parking_pct = (parking / boundary) * 100
  )

# Order factor by area for clearer visual hierarchy
area$city_clean <- factor(area$city_clean, levels = area$city_clean[order(area$parking)])

plot_area <- ggplot(area, aes(x = parking/ 1e6, y = city_clean)) +
  geom_bar(stat = "identity", fill = "steelblue",alpha=0.7) +
  labs( x = "Parking Area (km²)",
    y = "City Names") + theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    axis.line = element_line(colour = "black",size=0.1),
    panel.border = element_blank(),
    axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(colour="black", size=7, face="plain"),
    axis.title = element_text(colour="black", size=10, face="plain"))

#ggsave("../figure/fig_sum_s1.", plot = plot_area, width = 7, height = 10, dpi = 400)
tiff(paste0("../figure/Fig_sum_s1.tif"),width=12,height=20, units = "cm", res = 300, compression = "lzw")
plot(plot_area)
dev.off()




area$city_clean <- factor(area$city_clean, levels = area$city_clean[order(area$parking_pct)])

plot_pct <- ggplot(area, aes(x = parking_pct, y = city_clean)) +
  geom_bar(stat = "identity", fill = "steelblue",alpha=0.7) +
  labs(
    x = "Parking Percentage (%)",
    y = "City Names") + theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    axis.line = element_line(colour = "black",size=0.1),
    panel.border = element_blank(),
    axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(colour="black", size=7, face="plain"),
    axis.title = element_text(colour="black", size=10, face="plain"))

#ggsave("../figure/fig_sum_s2.png", plot = plot_pct, width = 7, height = 10, dpi = 400)
tiff(paste0("../figure/Fig_sum_s2.tif"),width=12,height=20, units = "cm", res = 300, compression = "lzw")
plot(plot_pct)
dev.off()
}

#################################################
###### latitude change
{
    matrix=data.frame(lat=input1$lat1,
                      isa=input1$ISA_parking,
                      coverage=input1$ratio)
    ratio_lat <- ggplot(matrix, aes(x = lat, y = coverage)) +
      geom_point(size=1,color=alpha(c("#5e4fa2"),.5)) + labs(y="Coverage (%)", x=expression(Latitude~'('*degree*')'))+
      geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.text = element_text(face="plain",size=10,color="black"),
                       axis.title =  element_text(face="plain",size=10,color="black"),    
                       plot.title = element_blank())+  coord_flip()
    ratio_lat1=ggMarginal(ratio_lat, type = "densigram",
                          margins="x",size =3,
                          xparams = list(binwidth =1, fill = "gray", color="black",size=0.2))
    isa_lat <- ggplot(matrix, aes(x = lat, y =isa )) +
      geom_point(size=1,color=alpha(c("#5e4fa2"),.5)) + labs(y="ISA (%)", x=expression(Latitude~'('*degree*')'))+
      geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.text = element_text(face="plain",size=10,color="black"),
                       axis.title =  element_text(face="plain",size=10,color="black"),
                       plot.title = element_blank())+  coord_flip()
    isa_lat1=ggMarginal(isa_lat, type = "densigram",
                        margins="x",size =3,
                        xparams = list(binwidth =1, fill = "gray", color="black",size=0.2))
  }

#################################################
###### maps across U.S.
{
    ratio_map= ggplot()+
      geom_point(data=input1,aes(x=lon, y=lat, fill=ratio),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",linewidth=0.1, alpha=0.1)+
      scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA)+ 
      labs(fill="Coverage(%)")+ ylab("")+ xlab("")+ 
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_text(colour="black", size=8, face="plain"),
                       axis.ticks = element_line(color = "black", linewidth =0.5),
                       legend.position = c(0.91,0.20),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = 3),hjust = 0.5),
                       legend.key.size = unit(0.3, "cm"),
                       legend.key.width = unit(0.2, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
    
    isa_map= ggplot()+
      geom_point(data=input1,aes(x=lon, y=lat, fill=ISA_parking),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
      scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                           breaks = seq(75,95,by=10))+
      labs(fill="ISA(%)")+ ylab("")+ xlab("")+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_text(colour="black", size=8, face="plain"),
                       axis.ticks = element_line(color = "black", linewidth =0.5),
                       legend.position = c(0.9,0.20),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = 3),hjust = 0.5),
                       legend.key.size = unit(0.3, "cm"),
                       legend.key.width = unit(0.2, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0)) 
  }

##### bar plot of ISA 
{
    input1[,1]=1:nrow(input1)
    input_mean=data.frame(t(apply(input1,2,mean,na.rm=TRUE)))
    input_sd=data.frame(t(apply(input1,2,sd,na.rm=TRUE)))
    input_isa_bar=data.frame(rbind(cbind(1,input_mean$ISA_parking,input_sd$ISA_parking),
                                   cbind(2,input_mean$ISA_urban,input_sd$ISA_urban),
                                   cbind(3,input_mean$ISA_open,input_sd$ISA_open)))
    names(input_isa_bar)=c("position","mean","sd")
    plot_bar_isa = ggplot(data=input_isa_bar, aes(x=as.factor(position), y=mean,fill=as.factor(position))) +
      geom_bar(stat="identity",position=position_dodge(),width=0.7)+
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25,size=0.3,position=position_dodge(.9))+
      ylab("ISA (%)")+ xlab("")+ labs(fill="")+
      scale_fill_manual(values=c("gray","orangered4","forestgreen"),
                        label=c("Parking","Others","Open"))+
      scale_x_discrete(label=c("Parking","Others","Open"))+
      theme_bw()+theme(text=element_text(family="serif"),
                       axis.line = element_line(colour = "black",size=0.2),
                       panel.border = element_blank(),
                       panel.background = element_rect(fill='transparent'),
                       plot.background = element_rect(fill='transparent', color=NA),
                       axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                       axis.ticks.length=unit(.05, "cm"),
                       axis.title.x=element_text(colour="black", size=8, face="plain"),
                       axis.title.y=element_text(colour="black", size=8, face="plain",vjust =-1),
                       axis.text.y=element_text(colour="black", size=8, face="plain",hjust =1.1),
                       axis.text.x=element_text(colour="black", size=7.5, face="plain",vjust=2),
                       panel.grid = element_blank(),
                       legend.position ="none",
                       plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))
  }
  
#################################################
#####combine figures
plot_sum=ggplot() +
  coord_equal(xlim = c(0, 140), ylim = c(0, 120), expand = FALSE) +
  annotation_custom(ggplotGrob(ratio_map), xmin = 0, xmax = 100, ymin = 60, ymax = 120) +  
  annotation_custom(ratio_lat1, xmin = 100, xmax = 140, ymin = 60, ymax = 120) +  
  annotation_custom(ggplotGrob(isa_map), xmin = 0, xmax = 100, ymin = 0, ymax = 60) +  
  annotation_custom(isa_lat1, xmin = 100, xmax = 140, ymin = 0, ymax =60) +  
  annotation_custom(ggplotGrob(plot_bar_isa), xmin = 8, xmax = 45, ymin = -1.5, ymax =23) +  
  annotate("text", x =5, y =118, label = "(a)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =105, y =118, label = "(b)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =5, y =58, label = "(c)",size=4,family="serif",fontface ="bold")+
  annotate("text", x =105, y =58, label = "(d)",size=4,family="serif",fontface ="bold")+
  theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_void()
tiff(paste0("../figure/Fig_sum_1.tif"),width=14,height=12, units = "cm", res = 300, compression = "lzw")
plot(plot_sum)
dev.off()

# Vector PDF for publication
pdf( file = "../figure/Fig_sum_1.pdf",
     width = 14/2.54,
     height = 12 /2.54,
     onefile = FALSE,
     paper = "special",
     colormodel = "srgb",
     useDingbats = FALSE)
plot(plot_sum)
dev.off()

#evi,tcc spaital map
p=list()
p[[1]] = ggplot()+
  geom_point(data=input1,aes(x=lon, y=lat, fill=EVI_open),pch=21,size=2)+
  geom_sf(data=us_states,color = "black", fill = "wheat1",linewidth=0.1, alpha=0.1)+
  scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                       limits = c(0, 0.5),breaks = seq(0, 0.5, by = 0.25))+ 
  labs(fill="EVI")+ ylab("")+ xlab("")+ 
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title=element_blank(),
                   axis.text=element_text(colour="black", size=8, face="plain"),
                   axis.ticks = element_line(color = "black", linewidth =0.5),
                   legend.position = c(0.9,0.20),
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = 3),hjust = 0.5,vjust = 2),
                   legend.key.size = unit(0.3, "cm"),
                   legend.key.width = unit(0.2, "cm"),
                   plot.margin = unit(c(0,0,0,0), "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

p[[2]] = ggplot()+
  geom_point(data=input1,aes(x=lon, y=lat, fill=TCC_open),pch=21,size=2)+
  geom_sf(data=us_states,color = "black", fill = "wheat1",linewidth=0.1, alpha=0.1)+
  scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                       limits = c(0, 61),breaks = seq(0, 61, by = 20))+ 
  labs(fill="TCC (%)")+ ylab("")+ xlab("")+ 
  theme_bw()+theme(text=element_text(family="serif"),
                   panel.grid = element_blank(),
                   axis.title=element_blank(),
                   axis.text=element_text(colour="black", size=8, face="plain"),
                   axis.ticks = element_line(color = "black", linewidth =0.5),
                   legend.position = c(0.9,0.20),
                   legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                   legend.text = element_text(colour="black", size=8, face="plain"),
                   legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = 3),hjust = 0.5,vjust = 2),
                   legend.key.size = unit(0.3, "cm"),
                   legend.key.width = unit(0.2, "cm"),
                   plot.margin = unit(c(0,0,0,0), "cm"),
                   plot.title = element_text(face="plain",size=10,hjust = 0.5,vjust=0))

library(cowplot)
fig <- plot_grid( p[[1]], p[[2]], ncol = 1,nrow=2, align = "hv" )
tiff(paste0("../figure/fig_sum_s5.tif"),width=12,height=14, units = "cm", res = 300, compression = "lzw")
print(fig)
dev.off()
