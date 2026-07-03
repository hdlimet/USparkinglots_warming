library(ggplot2)
library(tigris)
library(sf)
rm(list = ls())

##############readme################################
##plot figure 4, s4, s10, s11
###################################################


#################################################
####Global variables########
{
seasons= c("annual","spring","summer","fall","winter")
seasonss= c("(a) Annual","(b) Spring","(c) Summer","(d) Fall","(e) Winter")
seasonsss= c("","(a) Spring","(b) Summer","(c) Fall","(d) Winter")
boundary <- st_read(dsn="../input/raw_shapefile/", layer="boundary")
boundary <- boundary[-which(boundary$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]

us_states <- states(cb = TRUE, resolution = "20m") %>%  shift_geometry()
us_states <- us_states[-which(us_states$NAME %in% c("Alaska","Hawaii","Puerto Rico")),]
center <- st_transform(boundary,crs=st_crs(us_states)) %>% st_centroid() %>% st_geometry() %>% st_coordinates()
center1 <- st_transform(boundary,crs=4326) %>% st_centroid() %>% st_geometry() %>% st_coordinates()
}

#####input data from GEE####
{
input0=rbind(read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_1_2024.csv")),
             read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_2_2024.csv")),
             read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_3_2024.csv")),
             read.csv(paste0("../input/parking_project_gee/Parking_Lot_parking_mean_4_2024.csv")))
input0 = input0[-which(input0$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
input0[,4:8]=input0[,4:8]-273.15

input_city0=read.csv("../input/parking_project_gee/Parking_Lot_city_mean_2024_buffer_2.csv")
input_city0 = input_city0[-which(input_city0$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
citys=input_city0[,1]
input_city0[,2:16]=input_city0[,2:16]-273.15
input_city0_diff= data.frame(input_city0$LST_annual_parking-input_city0$LST_annual_open,
                       input_city0$LST_annual_urban-input_city0$LST_annual_open,
                       input_city0$LST_spring_parking-input_city0$LST_spring_open,
                       input_city0$LST_spring_urban-input_city0$LST_spring_open,
                       input_city0$LST_summer_parking-input_city0$LST_summer_open,
                       input_city0$LST_summer_urban-input_city0$LST_summer_open,
                       input_city0$LST_fall_parking-input_city0$LST_fall_open,
                       input_city0$LST_fall_urban-input_city0$LST_fall_open,
                       input_city0$LST_winter_parking-input_city0$LST_winter_open,
                       input_city0$LST_winter_urban-input_city0$LST_winter_open,
                       input_city0$ISA_parking-input_city0$ISA_open,
                       input_city0$ISA_urban-input_city0$ISA_open)
names(input_city0_diff)= c('LST_diff_annual_parking', 'LST_diff_annual_urban', 
                           'LST_diff_spring_parking','LST_diff_spring_urban',
                           'LST_diff_summer_parking','LST_diff_summer_urban',
                           'LST_diff_fall_parking', 'LST_diff_fall_urban', 
                           'LST_diff_winter_parking','LST_diff_winter_urban',
                           'ISA_diff_parking','ISA_diff_urban')
input_city0=cbind(input_city0,input_city0_diff)
isa_iqr=NA
for (ii in 1:nrow(input_city0)) {
  isa=input0$ISA_parking[input0$city==input_city0$city[ii]]
  isa_iqr[ii]=quantile(isa,na.rm = TRUE)[4]-quantile(isa,na.rm = TRUE)[2]
}
}

#################################################
####calculate LST-ISA slope###
{
  slope_sum=data.frame(nrow=nrow(center),ncol=5)
  pvalue_sum=data.frame(nrow=nrow(center),ncol=5)
  ##raw data
  for (id in 1:nrow(center)) {
    input_sel=input0[input0$city==input_city0$city[id],]
    if ((max(input_sel$ISA_parking,na.rm=TRUE)>0)) {
      for (ii in 1:5) {
        cat (id,ii,"\n")
        season=seasons[ii]
        regression = lm(input_sel[[paste0("LST_",season,"_parking")]]~input_sel[[paste0("ISA_parking")]])
        slope = coef(regression)[2]
        rr= round(summary(regression)$r.squared,2)
        pvalue= round(summary(regression)$coefficients[2,4],2)
        slope_sum[id,ii]=slope
        pvalue_sum[id,ii]=pvalue
      }
    }
  }
  names(slope_sum)=paste0("slope_",seasons)
  names(pvalue_sum)=paste0("pvalue_",seasons)
}
slope_sum[which(isa_iqr<5),]=NA

#################################################
####plotting of LST-ISA slope###
{
  ##boxplot of slope####
  {
  library(reshape2)
  slope_sum1=melt(slope_sum)
  names(slope_sum1)=c("season","slope_isa")
  
  plot_box_slope_isa = ggplot(data=slope_sum1,aes(x=season,y=slope_isa,fill=season))+ 
    stat_boxplot(geom = "errorbar", width = 0.5,size=0.3)+  
    geom_boxplot(fatten = NULL,outlier.size = 0,outlier.shape = 16,width=0.7, size=0.1, alpha=0.6)+
    stat_summary(fun.min = match.fun(mean), fun = match.fun(mean), fun.max = match.fun(mean), geom="errorbar", 
                 width=0.5,size=0.5, linetype="dashed",position=position_dodge(width=.75))+
    scale_fill_manual(values=c("#5e4fa2","#abdda4","#9e0142","#fdae61","#3288bd"),label=seasons)+
    scale_y_continuous(limits = c(-0.05,0.2),breaks=c(0,0.10,0.2))+
    scale_x_discrete(label= c("Annual","Spring","Summer","Fall","Winter"))+
    ylab(expression(Delta*'LST/'*Delta*ISA~'('*degree*C*'/%'*')'))+xlab("")+
    theme_bw()+theme(text=element_text(family="serif"),
                     panel.grid = element_blank(),
                     legend.position = "none",
                     axis.title = element_text(colour="black", size=10, face="plain"),
                     axis.text=element_text(colour="black", size=10, face="plain"),
                     plot.title = element_blank())
  }
  
  ##map####
  {
  input01=data.frame(citys=boundary$city,slope_sum,lon=center[,1],lat=center[,2],lat1=center1[,2])
  slope_lat <- ggplot(input01, aes(x = lat1, y =slope_annual)) +
    geom_point(size=0.5,color="#5e4fa2",alpha=0.5) + labs(y=expression(Delta~LST*'/'*Delta~ISA~'('*degree*C*'/%)'),
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
    
  plot_map_isa_slope=list()
  isa_slope_lat=list()
  
  for (ii in 1:5) {
    isa_slope_lat[[ii]] <- ggplot(input01, aes(x = lat1, y =!!sym(paste0("slope_",seasons[ii])))) +
      geom_point(size=0.5,color="#5e4fa2",alpha=0.5) + labs(y=expression(Delta~LST*'/'*Delta~ISA~'('*degree*C*'/%)'),
                                  x=expression(Latitude~'('*degree*')'))+
      geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       plot.background = element_rect(fill='transparent', color=NA),
                       plot.margin = unit(c(0,0,-0.2,-0.1), "cm"),
                       axis.ticks=element_line(color = "black", linewidth =0.1, size=0.1),
                       axis.ticks.length=unit(.05, "cm"),
                       axis.text.y = element_text(face="plain",size=6,color="black"),
                       axis.text.x = element_text(face="plain",size=6,color="black",vjust = 2),
                       axis.title.x =  element_text(face="plain",size=6,color="black",vjust = 5),
                       axis.title.y =  element_text(face="plain",size=6,color="black",vjust = -1),
                       plot.title = element_blank())+  coord_flip()
    
    plot_map_isa_slope [[ii]] = ggplot()+
      geom_point(data=input01,aes(x=lon, y=lat, fill=!!sym(paste0("slope_",seasons[ii]))),pch=21,size=2)+
      geom_sf(data=us_states,color = "black", fill = "wheat1",size=0.1, alpha=0.1)+
      scale_fill_gradient2(midpoint=0,  #limits = c(-0.055,0.195),
                           low="blue",mid="white",high="red", space ="Lab",na.value = NA)+
      labs(fill=expression('('*degree*C*'/%'*')'))+
      ylab("")+ xlab("")+ ggtitle(paste0(seasonsss[ii]))+
      theme_bw()+theme(text=element_text(family="serif"),
                       panel.grid = element_blank(),
                       axis.title=element_blank(),
                       axis.text=element_blank(),
                       axis.ticks = element_blank(),
                       legend.position = c(0.925,0.20),
                       legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                       legend.text = element_text(colour="black", size=8, face="plain"),
                       legend.title= element_text(colour="black", size=8, face="plain", margin = margin(b = -0.5),vjust = 2),
                       legend.key.size = unit(0.2, "cm"),
                       legend.key.width = unit(0.15, "cm"),
                       plot.margin = unit(c(0,0,0,0), "cm"),
                       plot.title = element_text(face="plain",size=8.3,hjust = 0.5,vjust=-9))+
      {if (ii==1) theme(axis.text=element_text(colour="black", size=8, face="plain"),
                        axis.ticks = element_line(color = "black", linewidth =0.5))}+
      {if (ii==1) coord_sf(crs = st_crs(us_states),expand=TRUE,
                           xlim=c((st_bbox(us_states)$xmin-100000),st_bbox(us_states)$xmax),
                           ylim=c((st_bbox(us_states)$ymin+100000),(st_bbox(us_states)$ymax-100000)))}
    
  }
  plot_sum_map=ggplot() +
    coord_equal(xlim = c(0, 160), ylim = c(0, 100), expand = FALSE) +
    annotation_custom(ggplotGrob(plot_map_isa_slope[[2]]), xmin = 0, xmax = 80, ymin = 50, ymax = 100) +
    annotation_custom(ggplotGrob(plot_map_isa_slope[[3]]), xmin = 80, xmax = 160, ymin = 50, ymax = 100) +
    annotation_custom(ggplotGrob(plot_map_isa_slope[[4]]), xmin = 0, xmax = 80, ymin = 0, ymax = 50) +
    annotation_custom(ggplotGrob(plot_map_isa_slope[[5]]), xmin = 80, xmax = 160, ymin = 0, ymax = 50) +
    theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
    theme_void()
  tiff(paste0("../figure/Fig_sum_s10.tif"),width=16,height=10, units = "cm", res = 300, compression = "lzw")
  plot(plot_sum_map)
  dev.off()
  }
  
  ##scatter: relationship with background LST####
  {
  input02=data.frame(input_city0,slope_sum,pvalue_sum)
  plot_scatter_slope=list()
  for (ii in 1:5) {
    input_sel=data.frame(season=ii,
                         open=input02[,paste0("LST_",seasons[ii],"_open")],
                         diff=input02[,paste0("slope_",seasons[ii])],
                         pvalue=input02[,paste0("pvalue_",seasons[ii])],
                         isa=input02[,"ISA_parking"])
    regression = lm(input_sel$diff~input_sel$open)
    slope = paste0("Slope=",sprintf("%.4f",coef(regression)[2]))
    pvalue0= summary(regression)$coefficients[2,4]
    
    if (pvalue0<0.01) {pvalue1="P<0.01"} else if (pvalue0>0.01 & pvalue0<0.05) {pvalue1="P<0.05"} else {pvalue1=paste0("P=",sprintf("%.2f",pvalue0))}
    pvalue= pvalue1
    plot_scatter_slope[[ii]] = ggplot(data=input_sel,aes(x=open,y=diff,fill=isa))+
      geom_point(size=1.5,pch = 21,stroke = 0.1)+
      geom_smooth(method="lm",formula=y~x,size=0.5,fill=NA,color="black")+
      scale_fill_distiller(type = "div", palette = "RdYlBu",direction = -1,na.value=NA,
                            breaks = seq(70,90,by=10))+
      scale_y_continuous(limits = c(-0.05,0.195),breaks = c(0,0.1,0.2))+
      ylab(expression(Delta*'LST/'*Delta*ISA~'('*degree*C*'/%'*')'))+
      xlab(expression(LST[OpenSpace]~'('*degree*C*')'))+
      labs(fill="ISA (%)")+
      geom_text(label=paste0(seasonsss[ii]),aes(x= -Inf,y = Inf), hjust = -0.08,vjust =1.3,
                colour = "black",fontface = "plain",size=3.5,family="serif")+
      geom_text(label=paste0(slope),aes(x= -Inf,y = Inf), hjust = -0.08,vjust =3.5,
                colour = "black",fontface = "plain",size=3.5,family="serif")+
      geom_text(label=paste0(pvalue),aes(x= -Inf,y = Inf), hjust = -2.4,vjust =3.5,
                colour = "black",fontface = "plain",size=3.5,family="serif")+
      theme_bw()+theme(text=element_text(family="serif"),
                       plot.background = element_rect(fill='transparent', color=NA),
                       panel.background = element_rect(fill='transparent'),
                       panel.grid = element_blank(),
                       axis.text = element_text(face="plain",size=10,color = "black"),
                       axis.title = element_text(face="plain",size=10,color = "black"),
                       legend.position = "none",
                       plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))+
      {if (ii==5) theme(legend.position = c(1.15,1.2),
                        legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                        legend.text = element_text(colour="black", size=9, face="plain"),
                        legend.title= element_text(colour="black", size=9, face="plain", margin = margin(b = 0),hjust = 0.5,vjust = 2),
                        legend.key.size = unit(0.5, "cm"),
                        legend.key.width = unit(0.25, "cm"))}+
      {if (ii==1) theme(legend.position = c(0.91,0.63),
                        legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                        legend.text = element_text(colour="black", size=8, face="plain"),
                        legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = 0),hjust = 0.5,vjust = 2),
                        legend.key.size = unit(0.2, "cm"),
                        legend.key.width = unit(0.15, "cm"))}
  }
  plot_sum=ggplot() +
    coord_equal(xlim = c(0, 110), ylim = c(0, 100), expand = FALSE) +
    annotation_custom(ggplotGrob(plot_scatter_slope[[2]]), xmin = 0, xmax = 50, ymin = 50, ymax = 100) +  
    annotation_custom(ggplotGrob(plot_scatter_slope[[3]]), xmin = 50, xmax = 100, ymin = 50, ymax = 100) +
    annotation_custom(ggplotGrob(plot_scatter_slope[[4]]), xmin = 0, xmax = 50, ymin = 0, ymax = 50) +
    annotation_custom(ggplotGrob(plot_scatter_slope[[5]]), xmin = 50, xmax = 100, ymin = 0, ymax = 50) +
    theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
    theme_void()
  tiff(paste0("../figure/Fig_sum_s11.tif"),width=16.5,height=15, units = "cm", res = 300, compression = "lzw")
  plot(plot_sum)
  dev.off()
  }
  
  ##combine figures
  {
    plot_sum_map=ggplot() +
    coord_equal(xlim = c(0, 100), ylim = c(0, 105), expand = FALSE) +
    annotation_custom(ggplotGrob(plot_box_slope_isa), xmin = 0, xmax = 50, ymin = 54, ymax = 105) +
    annotation_custom(ggplotGrob(plot_map_isa_slope[[1]]), xmin = 0.5, xmax = 100.5,ymin = -1, ymax = 57) +
    annotation_custom(ggplotGrob(plot_scatter_slope[[1]]), xmin = 50, xmax = 100, ymin = 53.4, ymax = 105) +
    annotation_custom(ggplotGrob(slope_lat), xmin = 9, xmax = 27, ymin = 1.8, ymax = 19.8) +
    theme(text=element_text(family="serif"), plot.margin = unit(c(0,0,0,0), "cm"))+
    annotate("text", x =11, y =102, label = "(a)",size=4,family="serif",fontface ="bold")+
    annotate("text", x =11, y =52, label = "(b)",size=4,family="serif",fontface ="bold")+
    annotate("text", x =61, y =102, label = "(c)",size=4,family="serif",fontface ="bold")+
    annotate("text", x =91, y =18, label = (expression(Delta*'LST/'*Delta*ISA)),size=2.5,family="serif")+
    theme_void()
  tiff(paste0("../figure/Fig_sum_4.tif"),width=15,height=16, units = "cm", res = 300, compression = "lzw")
  plot(plot_sum_map)
  dev.off()
  pdf( file = "../figure/Fig_sum_4.pdf",
       width = 15/2.54,
       height = 16 /2.54,
       onefile = FALSE,
       paper = "special",
       colormodel = "srgb",
       useDingbats = FALSE)
  plot(plot_sum_map)
  dev.off()
  }
}

###################################
#900m large parking vs all parking
{
  library(dplyr)
  input0=read.csv("../input/parking_project_gee_v2/Parking_Lot_parking_mean.csv")
  input0 = input0[-which(input0$city %in% c("anchorage-ak","honolulu-hi","san-juan-pr")),]
  df_long <- 
    input0 %>%
    mutate(lot_type = "All parking lots") %>%
    bind_rows(
      input0 %>%
        filter(area > 900) %>%
        mutate(lot_type = "Large parking lots (area > 900m2)")) %>%
    pivot_longer(
      cols = c(LST_annual_parking, LST_spring_parking,
               LST_summer_parking, LST_fall_parking,
               LST_winter_parking),
      names_to = "season",
      values_to = "LST")
  
  # Clean season labels
  df_long <- df_long %>%
    mutate(
      season = gsub("LST_|_parking", "", season),
      season = factor(
        season,
        levels = c("annual", "spring", "summer", "fall", "winter"),
        labels = c("Annual", "Spring", "Summer", "Fall", "Winter") ))
  
  # 2. City-level mean LST for each season and lot type
  city_means <- df_long %>% group_by(city, lot_type, season) %>%
    summarise(city_mean_LST = mean(LST, na.rm = TRUE), .groups = "drop")
  
  # 3. Across-city mean and SD of city means
  summary_df <- city_means %>%
    group_by(lot_type, season) %>%
    summarise(
      mean_LST = mean(city_mean_LST, na.rm = TRUE),
      sd_LST   = sd(city_mean_LST, na.rm = TRUE),
      n_city   = n(),.groups = "drop")
  
  plot_large_parking <- ggplot(summary_df, aes(x = season, y = mean_LST, fill = lot_type)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(
      aes(ymin = mean_LST - sd_LST/10,
          ymax = mean_LST + sd_LST/10),
      position = position_dodge(width = 0.8),alpha=0.7,
      width = 0.2) + labs(
        x = NULL,fill = "",
        y = "LST (°C)") +
    theme_bw() +
    scale_fill_manual(values= c("skyblue","orange"),labels = c(
      "All parking lots",
      expression("Large parking lots (area > 900 m"^2*")")))+
    theme_bw()+theme(text=element_text(family="serif"),
                     axis.title.y=element_text(colour="black", size=10, face="plain"),
                     axis.title.x=element_blank(),
                     axis.text=element_text(colour="black", size=10, face="plain"),
                     panel.grid = element_blank(),
                     legend.position = c(0.82,0.92),
                     legend.background = element_rect(fill="transparent", size=2, linetype="blank", colour ="darkblue"),
                     legend.text = element_text(colour="black", size=8, face="plain"),
                     legend.title= element_text(colour="black", size=8, face="plain",margin = margin(b = -0.1),hjust = 0.3),
                     legend.key.size = unit(0.4, "cm"),
                     legend.key.width = unit(0.4, "cm"),
                     plot.title = element_text(face="plain",size=10,hjust = 0,vjust=0))
  tiff(paste0("../figure/fig_sum_s4.tif"),width=15,height=10, units = "cm", res = 300, compression = "lzw")
  plot_large_parking
  dev.off()
  
  ttest_results <- city_means %>%
    tidyr::pivot_wider(
      names_from = lot_type,
      values_from = city_mean_LST
    ) %>%
    group_by(season) %>%
    summarise(
      n_city = n(),
      t_test = list(
        t.test(`Large parking lots (area > 900m2)`,
               `All parking lots`,
               paired = TRUE)
      ),
      p_value = t_test[[1]]$p.value,
      mean_diff = mean(`Large parking lots (area > 900m2)` - `All parking lots`, na.rm = TRUE),
      df        = t_test[[1]]$parameter,      # degrees of freedom
      t_value   = t_test[[1]]$statistic,      # optional: APA reporting needs this
      .groups = "drop"
    ) %>%
    select(season, n_city, mean_diff,  t_value, df, p_value)
  
  ttest_results
}
