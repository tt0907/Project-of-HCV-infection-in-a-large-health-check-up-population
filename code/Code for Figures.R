rm (list = ls())
library (ggbreak)
library (ggplot2)
library (plyr)
library (sp)
library (maptools)
library (Cairo)
library (RColorBrewer)
library (openxlsx)
library ("readxl")
library (colorspace)
library (epitools)
library (tableone)
library (haven)
library (tidyverse)
library (data.table)
library (gtsummary)
library ("survminer")
library (dplyr)
library (lattice)
library (rgdal)
library (ggsci)
library (ggsignif)
library (scales)
library (grid)
library (ggpubr)
library (ggbreak)
library (ggh4x)
library (robustbase)
library (patchwork)
library (RImagePalette)
library (jpeg)
library (rms)
library (mgcv)
library (lme4)
library (ggridges)
library (gghalves)
library (grDevices)
library (ggbeeswarm)
library (forestploter)
library (flextable)
library (export)



##############********** Figure 1 ********################
##############*   Flow chart of the study population, draw by PPT according to the inclusion and exclusion criteria of this study
##############*   Details of the criteria are presented in the Method section of the manuscript


##############********** Figure 2 ********################
##############*   Prevalence
windowsFonts(myFont = windowsFont("Arial"))

bar <- read_excel ("E:\\HCV_data\\Clusterbarplot.xlsx",sheet = "Prevalence")
bar$Sex <- factor (bar$Sex, levels = c('Total','Male','Female'))

barplot <- ggplot (data = bar, aes (x = Sex, y = Prevalence, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", color = 'white', size = 0.5) +
  scale_fill_manual(values =  c ("#544F8F", "#668FCE","#C65A46")) +
  coord_cartesian (ylim = c(0,1.5)) +
  labs (x = '', y = expression("Prevalence of anti-HCV (%)")) +
  geom_errorbar(aes (x = Sex, ymin = LCI, ymax = UCI), width = 0.2, color = 'black', position = position_dodge(0.8), size = 0.4) +
  geom_text (aes (label = sprintf ("%0.2f", round (Prevalence, digits = 2))), position = position_dodge(0.8), vjust = -(4 + bar$UCI - bar$Prevalence), color = 'black', size = 4) +
  scale_y_continuous(breaks = c(0,0.5,1.0,1.5), expand = c(0,0)) +
  scale_x_discrete(labels = c ("Total", "Male", "Female")) +
  theme_classic() 
barplot

barplot1 <- barplot + theme (legend.position = "none",
                             text = element_text(size = 12, color = 'black'),
                             axis.text.x = element_text(size = 12, color = 'black'),
                             axis.text.y = element_text(size = 12, color = 'black'),
                             axis.title.x = element_text (size = 14, color = 'black'),
                             axis.title.y = element_text (size = 14, color = 'black'))  
barplot1


##############*   Incidence
windowsFonts(myFont = windowsFont("Arial"))

bar <- read_excel ("E:\\HCV_data\\Clusterbarplot.xlsx",sheet = "Incidence")
bar$Sex <- factor (bar$Sex, levels = c('Total','Male','Female'))

barplot <- ggplot (data = bar, aes (x = Sex, y = Prevalence, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", color = 'white', size = 0.5) +
  scale_fill_manual(values =  c ("#544F8F", "#668FCE","#C65A46")) +
  coord_cartesian (ylim = c(0,1.5)) +
  labs (x = '', y = 'Incidence rate of HCV seroconversion\n (per 1,000 person-years)') +
  geom_errorbar(aes (x = Sex, ymin = LCI, ymax = UCI), width = 0.2, color = 'black', position = position_dodge(0.8), size = 0.4) +
  geom_text (aes (label = sprintf ("%0.2f", round (Prevalence, digits = 2))), position = position_dodge(0.8), vjust = -(4 + bar$UCI - bar$Prevalence), color = 'black', size = 4) +
  scale_y_continuous(breaks = c(0,0.5,1.0,1.5), expand = c(0,0)) +
  scale_x_discrete(labels = c ("Total", "Male", "Female")) +
  theme_classic() 
barplot

barplot2 <- barplot + theme (legend.position = "none",
                             text = element_text(size = 12, color = 'black'),
                             axis.text.x = element_text(size = 12, color = 'black'),
                             axis.text.y = element_text(size = 12, color = 'black'),
                             axis.title.x = element_text (size = 14, color = 'black'),
                             axis.title.y = element_text (size = 14, color = 'black'))  
barplot2



##############********** Line Plot 
##############*  Prevalence
windowsFonts(myFont = windowsFont("Arial"))

line <- read_excel ("E:\\HCV_data\\Lineplot.xlsx",sheet = "Prevalence")
line$Age <- factor (line$Age, levels = c('18-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','≥70'))
line$Group <- factor (line$Group, levels = c('Total','Male', 'Female'))

lineplot <- ggplot(line, aes (x = Age, y = Prevalence, color = Group, group = Group, shape = Group)) +
  labs (x = 'Age (years)', y = 'Prevalence of anti-HCV (%)') +
  geom_line (position = position_dodge(0.1)) +
  geom_point (position = position_dodge(0.1)) +
  geom_errorbar(aes (x = Age, ymin = LCI, ymax = UCI), width = 0.5, position = position_dodge(0.1), size = 0.4) +
  scale_color_manual(values = c ("#544F8F", "#668FCE","#C65A46")) +
  scale_x_discrete(labels = c ("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69",expression("">="70"))) +
  scale_y_continuous(limits=c(0,3), breaks = c(0,0.5,1.0,1.5,2.0,2.5,3.0)) +
  theme_classic() +
  theme(legend.title = element_blank()) + 
  guides(colour = guide_legend (byrow = TRUE)) 
lineplot

lineplot1 <- lineplot +theme (legend.background = element_blank(),
                              legend.text = element_text(size = 12),
                              legend.key.size = unit (20, "pt"),
                              legend.box.background = element_rect (fill = NA, color = NA),
                              legend.position = c(0.20,0.80),
                              axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = 'black'),
                              axis.text.y = element_text(size = 12, color = 'black'),
                              axis.title.x = element_text (size = 14, color = 'black'),
                              axis.title.y = element_text (size = 14, color = 'black'),
                              text = element_text(size = 12, color = 'black')) 



lineplot1


##############*  Incidence
windowsFonts(myFont = windowsFont("Arial"))

line <- read_excel ("E:\\HCV_data\\Lineplot.xlsx",sheet = "Incidence")
line$Age <- factor (line$Age, levels = c('18-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','≥70'))
line$Group <- factor (line$Group, levels = c('Total','Male', 'Female'))

lineplot <- ggplot(line, aes (x = Age, y = Prevalence, color = Group, group = Group, shape = Group)) +
  labs (x = 'Age (years)', y = 'Incidence rate of HCV seroconversion\n (per 1,000 person-years)') +
  geom_line (position = position_dodge(0.1)) +
  #geom_smooth(position = position_dodge(0.1), span = 0.5, se = F) +
  geom_point (position = position_dodge(0.1)) +
  geom_errorbar(aes (x = Age, ymin = LCI, ymax = UCI), width = 0.5, position = position_dodge(0.1), size = 0.4) +
  scale_color_manual(values = c ("#544F8F", "#668FCE","#C65A46")) +
  scale_x_discrete(labels = c ("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69",expression("">="70"))) +
  scale_y_continuous(limits=c(0,5), breaks = c(0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)) +
  theme_classic() +
  theme(legend.title = element_blank()) + 
  guides(colour = guide_legend (byrow = TRUE)) 
lineplot

lineplot2 <- lineplot +theme (legend.background = element_blank(),
                              legend.text = element_text(size = 12),
                              legend.key.size = unit (20, "pt"),
                              legend.box.background = element_rect (fill = NA, color = NA),
                              legend.position = c(0.20,0.80),
                              axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = 'black'),
                              axis.text.y = element_text(size = 12, color = 'black'),
                              axis.title.x = element_text (size = 14, color = 'black'),
                              axis.title.y = element_text (size = 14, color = 'black'),
                              text = element_text(size = 12, color = 'black')) 



lineplot2



##############********** Combine 4 Plots 
patchwork <- (barplot1 + lineplot1 + barplot2 + lineplot2 + plot_layout(ncol = 2, nrow = 2, widths = c(0.36,0.64))) + plot_annotation(tag_levels = 'A')
patchwork







##############********** Figure 3 ********#################
bar <- read_excel ("E:\\HCV_data\\Clusterbarplot-vertical1.xlsx",sheet = "Prevalence-afterimputation") 


cairo_pdf("Cluster.pdf", width = 10, height = 11)
bar <- bar %>% data.table()
bar$Group <- factor (bar$Group, levels = c('Geographical and socioeconomic characterisitics',
                                           'Southern China','Northern China', 
                                           'East China', 'West China', 'Central China', 
                                           'Non-border regions','Border regions', 
                                           'Living in areas with high GDP per capita', 'Living in areas with low GDP per capita',
                                           'Abnormal liver function or liver fibrosis',
                                           'Normal ALT', 'Elevated ALT' ,
                                           'Normal AST', 'Elevated AST', 
                                           'Normal GGT', 'Elevated GGT',
                                           'Normal total bilirubin', 'Elevated total bilirubin',
                                           'Normal platelet count', 'Decreased platelet count',
                                           'Normal albumin', 'Decreased albumin',
                                           'Without liver fibrosis †', 'With significant liver fibrosis †', 'With advanced liver fibrosis or cirrhosis †', 
                                           'Abnormal thyroid function or antibodies',
                                           'Normal thyroid function', 'With subclinical hypothyroidism', 'With overt hypothyroidism', 'With subclinical hyperthyroidism', 'With overt hyperthyroidism',
                                           'TgAb negative', 'TgAb positive',
                                           'TPOAb negative', 'TPOAb positive',
                                           'Cardio-renal-metabolic components',
                                           'Without diabetes (WHO criteria)', 'With diabetes (WHO criteria)',
                                           'eGFR ≥ 60 ml/min/1.73 m2','eGFR < 60 ml/min/1.73 m2'))
bar [,Group:=fct_relevel(bar$Group,rev)]
levels(bar$Group)

barplot <- ggplot (data = bar, aes (x = Group, y = Prevalence, fill = Group, color = Group), stat = 'identity') +
  scale_fill_manual(values =  c("#c9d4f7","#c9d4f7",
                                "#c09f7e","#c09f7e",
                                "white",
                                "#d1e9f4","#d1e9f4",
                                "#c5a6c4","#c5a6c4",
                                "#fe98a7","#fe98a7","#fe98a7","#fe98a7", "#fe98a7",
                                "white",
                                "#e7d3ed","#e7d3ed","#e7d3ed",
                                "#b0d097","#b0d097",
                                "#e1f9e8","#e1f9e8",
                                "#ffc8d0","#ffc8d0",
                                "#fd8d6e","#fd8d6e",
                                "#f7e8c9","#f7e8c9",
                                "#faadac","#faadac",
                                "white",
                                "#fcc351","#fcc351",
                                "#dfde6c","#dfde6c",
                                "#eeeaeb","#eeeaeb","#eeeaeb",
                                "#fa86a9","#fa86a9",
                                "white" )) +
  labs (x = 'Subgroups', y = 'Prevalence of anti-HCV (%)') +
  theme_classic() +
  theme (text = element_text(size = 12, color = 'black'),
         axis.text.x = element_text (size = 12, color = 'black'),
         axis.text.y = element_text (size = 12, color = 'black'),
         axis.title.x = element_text (size = 14, color = 'black'),
         axis.title.y = element_text (size = 14, color = 'black'),
         legend.title = element_text (size = 12, color = 'black'),
         legend.text = element_text (size = 12, color = 'black')) +
  geom_col (position = 'dodge', width = 0.8, color = 'black', size = 0.5, alpha = 0.9) +
  geom_errorbar(aes (x = Group, ymin = LCI, ymax = UCI), width = 0.2, color = 'black', position = position_dodge(0.8), size = 0.4) +
  geom_text (aes (y = label_y, label = paste (format (sprintf ("%0.2f", round (Prevalence, digits = 2))),"%")), position = position_dodge(0.8), color = 'black', size = 4) +
  scale_y_continuous(limits = c(0, 5.9), expand = c(0,0), breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5)) +
  scale_y_break(c(4.4,5.5), space = 0, expand = c(0,0)) +
  scale_x_discrete() +
  coord_flip() +
  guides (fill = F) 
barplot


barplot1 <- barplot + 
  stat_signif(data=bar,show.legend = FALSE,xmin=1, xmax=2, annotations="***", y_position=2.22, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.22),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=3, xmax=4, annotations="***", y_position=1.66, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.13),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=6, xmax=7, annotations="**", y_position=1.64, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.15),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=8, xmax=9, annotations="**", y_position=1.50, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.12),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=10, xmax=14.2, annotations="**", y_position=4.20, textsize = 5, vjust = 0.3, tip_length = c(0.29, 0.62),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=13, xmax=13.8, annotations="***", y_position=1.62, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.145),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=12, xmax=14, annotations="***", y_position=3.78, textsize = 5, vjust = 0.3, tip_length = c(0.016, 0.535),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=16, xmax=18.1, annotations="***", y_position=5.81, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.84),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=17, xmax=17.9, annotations="**", y_position=4.08, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.52),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=19, xmax=20, annotations="**", y_position=2.04, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.2),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=21, xmax=22, annotations="***", y_position=4.24, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.59),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=23, xmax=24, annotations="***", y_position=1.41, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.08),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=25, xmax=26, annotations="***", y_position=1.76, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.11),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=27, xmax=28, annotations="***", y_position=4.17, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.57),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=29, xmax=30, annotations="***", y_position=1.83, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.14),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=32, xmax=33, annotations="***", y_position=1.63, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.14),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=34, xmax=35, annotations="***", y_position=1.52, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.12),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=36, xmax=38.1, annotations="***", y_position=1.82, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.2),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=37, xmax=37.9, annotations="***", y_position=1.05, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.06),color = "black",size = 0.4) +
  stat_signif(data=bar,show.legend = FALSE,xmin=39, xmax=40, annotations="***", y_position=1.55, textsize = 5, vjust = 0.3, tip_length = c(0.02, 0.16),color = "black",size = 0.4) 
barplot1
#output: 10*10 inch  
dev.off()







##############********** Figure 4 ********################
##############********** Prevalence
china_map <- readShapePoly("E:\\HCV_data\\bou\\bou2_4p.shp")
bianjing <- rgdal::readOGR("E:\\HCV_data\\SouthSea\\bou2_4l.shp")
l9 <- rgdal::readOGR("E:\\HCV_data\\SouthSea\\九段线.shp")
province_city <- read_excel ("E:\\HCV_data\\chinaprovincecity2.xlsx")
Heatmap_data <- read_excel("E:\\HCV_data\\Map.xlsm", sheet = "Prevalence")
quantile (Heatmap_data$Prevalence, c(0, 0.25, 0.50, 0.75, 1))
china_map@data$NAME <- as.character(((iconv(china_map@data$NAME,"GBK","UTF-8"))))
x <- china_map@data
xs <- data.frame (x, id = seq (0:924)-1)
china_map1 <- fortify(china_map)
china_map_data <- join (china_map1, xs, type = "full")

china_map2 <- fortify (bianjing)
china_map2 <- subset (china_map2, id == 1087|id == 1336|id == 1377| id == 1475|id ==1481| id ==1784| id==1769 | id==1764|id==1508| id==1488)
china_map_data <- plyr::join (china_map_data, china_map2, by = "id", type = "full")

china_data <- join (china_map_data, Heatmap_data,type = "full")
china_data1 <- merge (china_data, china_map2, by = c("order", "piece", "id", "group"), all = TRUE)
china_data1$long <- ifelse (is.na (china_data1$long.x)==TRUE, china_data1$long.y, china_data1$long.x)
china_data1$lat <- ifelse (is.na (china_data1$lat.x)==TRUE, china_data1$lat.y, china_data1$lat.x)
midops <- function(x) mean (range (x, na.rm = TRUE))

china_data1$Prevalence <- cut (china_data1$Prevalence, breaks = c(0.13, 0.4599634, 0.7048, 0.9337935, 2.43 ), 
                               labels = c("0.13 ~ 0.45", "0.46 ~ 0.70", "0.71 ~ 0.93", "0.94 ~ 2.43"),
                               right = FALSE, order = TRUE)
windowsFonts(myFont = windowsFont("Arial"))
rhg_cols1 <- c("#E7D4E8", "#C4ABCF","#AE8BBC", "#8A659A")

map1 <- ggplot (china_data1, aes (x = long, y = lat)) +
  geom_polygon(aes (group = group, fill = Prevalence), colour = "grey30", size = 0.05) +
  scale_fill_manual(values = rhg_cols1) +
  guides (fill = guide_legend(title = expression("Prevalence of anti-HCV (%)"))) +
  coord_map ("polyconic") +
  geom_text(size = 2.3, aes (x = jd, y = wd, label = province), hjust = 0.5, vjust = 0.5, data = province_city) +
  theme (panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         legend.position = c(0.14, 0.25),
         legend.text = element_text(size = 12),
         legend.title = element_text(size=12),
         legend.key.size = unit (14, "pt"),
         legend.title.align = 0.5)
map1


##############********** Incidence
china_map <- readShapePoly("E:\\HCV_data\\bou\\bou2_4p.shp")
bianjing <- rgdal::readOGR("E:\\HCV_data\\SouthSea\\bou2_4l.shp")
l9 <- rgdal::readOGR("E:\\HCV_data\\SouthSea\\九段线.shp")
province_city <- read_excel ("E:\\HCV_data\\chinaprovincecity2.xlsx")
Heatmap_data <- read_excel("E:\\HCV_data\\Map.xlsm", sheet = "Incidence")
quantile (Heatmap_data$Incidence, c(0, 0.25, 0.50, 0.75, 1))
china_map@data$NAME <- as.character(((iconv(china_map@data$NAME,"GBK","UTF-8"))))
x <- china_map@data
xs <- data.frame (x, id = seq (0:924)-1)
china_map1 <- fortify(china_map)
china_map_data <- join (china_map1, xs, type = "full")

china_map2 <- fortify (bianjing)
china_map2 <- subset (china_map2, id == 1087|id == 1336|id == 1377| id == 1475|id ==1481| id ==1784| id==1769 | id==1764|id==1508| id==1488)
china_map_data <- plyr::join (china_map_data, china_map2, by = "id", type = "full")

china_data <- join (china_map_data, Heatmap_data,type = "full")
china_data1 <- merge (china_data, china_map2, by = c("order", "piece", "id", "group"), all = TRUE)
china_data1$long <- ifelse (is.na (china_data1$long.x)==TRUE, china_data1$long.y, china_data1$long.x)
china_data1$lat <- ifelse (is.na (china_data1$lat.x)==TRUE, china_data1$lat.y, china_data1$lat.x)
midops <- function(x) mean (range (x, na.rm = TRUE))

china_data1$Incidence <- cut (china_data1$Incidence, breaks = c(0.0000000, 0.4992417, 0.7658928, 1.2138689, 7.09), 
                              labels = c("0.00 ~ 0.49", "0.50 ~ 0.76", "0.77 ~ 1.20", "1.21 ~ 7.08"),
                              right = FALSE, order = TRUE)
windowsFonts(myFont = windowsFont("Arial"))
rhg_cols1 <- c("#D9F0D3", "#A6DBA0", "#79BE7F", "#519D57")

map2 <- ggplot (china_data1, aes (x = long, y = lat)) +
  geom_polygon(aes (group = group, fill = Incidence), colour = "grey30", size = 0.05) +
  scale_fill_manual(values = rhg_cols1) +
  guides (fill = guide_legend(title = expression("Incidence rate of HCV seroconversion\n (per 1,000 person-years)"))) +
  coord_map ("polyconic") +
  geom_text(size = 2.3, aes (x = jd, y = wd, label = province), hjust = 0.5, vjust = 0.5, data = province_city) +
  theme (panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         legend.position = c(0.14, 0.25),
         legend.text = element_text(size = 12),
         legend.title = element_text(size=12),
         legend.key.size = unit (14, "pt"),
         legend.title.align = 0.5)
map2


##############********** Combine 2 Maps 
patchwork <- (map1 + map2 + plot_layout(ncol = 2, nrow = 1)) + plot_annotation(tag_levels = 'A')
patchwork







##############********** Figure 5 ********#################
##############*   Poisson regression
# Read provided sample example data
forest <- read_excel ("E:\\HCV_data\\Forestplot.xlsx",sheet = "Poisson-new")

cairo_pdf("Forest1.pdf", width = 16.2, height = 8)
# indent the subgroup if there is a number in the placebo column
forest$Populations <- ifelse (is.na (forest$`No. of participants (%)`),
                              forest$Populations,
                              paste0("   ", forest$Populations))

# NA to blank or NA will be transformed to charachter.
forest$`No. of participants (%)` <- ifelse(is.na(forest$`No. of participants (%)`), "", forest$`No. of participants (%)`)
forest$`Person-years` <- ifelse(is.na(forest$`Person-years`), "", forest$`Person-years`)
forest$Events <- ifelse(is.na(forest$Events), "", forest$Events)
forest$`per 1,000 person-years` <- ifelse(is.na(forest$`per 1,000 person-years`), "", forest$`per 1,000 person-years`)

# Add blank column for the forest plot to display CI.
# Adjust the column width with space.
forest$` ` <- paste(rep(" ", 20), collapse = " ")
forest$`  ` <- paste(rep(" ", 20), collapse = " ")


# Create confidence interval column to display
forest$`Crude IRR (95% CI) ‡` <- forest$F
forest$`Crude IRR (95% CI) ‡` <- ifelse(is.na(forest$`Crude IRR (95% CI) ‡`), "", forest$`Crude IRR (95% CI) ‡`)

forest$`Adjusted IRR (95% CI) §` <- forest$J
forest$`Adjusted IRR (95% CI) §` <- ifelse(is.na(forest$`Adjusted IRR (95% CI) §`), "", forest$`Adjusted IRR (95% CI) §`)


# Define theme
tm <- forest_theme (base_size = c (12,12),
                    legend_value = c("", ""), 
                    refline_lty = c ("longdash","longdash"),        # 参考线的线型
                    ci_pch = c(15,15), 
                    ci_col = c("black","black"),
                    footnote_col = "black")

p <- forest(forest[,c(1:5, 14,16,15,17)],
            est = list (forest$est,forest$est1),
            lower = list (forest$low,forest$low1),
            upper = list (forest$high,forest$high1),
            ci_column = c(6,8),
            ref_line = c(1,1),
            x_trans=c("log","log"),
            xlim = list (c(0.8, 6.5),c(0.8, 6.5)),
            ticks_at = list (c( 1,2, 3,4, 5,6),c(1,2, 3,4, 5,6)),
            arrow_lab = list (c("Decreased risk of \nHCV seroconversion", "Increased risk of \nHCV seroconversion"),c("Decreased risk of \nHCV seroconversion", "Increased risk of \nHCV seroconversion")),
            theme = tm)

# Print plot
plot(p)

f <- insert_text (p, text = "IR (95% CI) †,", col = 5, part = "header", gp = gpar (fontface = "bold"))
f <- add_border (f, part = "header", row = 1, where = "top", gp = gpar(lwd = 1.5))
f <- add_border (f, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1.5))

f <- edit_plot (f, row = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29), which = "background", gp = gpar(fill = "#ECF1F8"))

f

dev.off()



##############********** Supplementary Figure 1 ********#################
##############*   This figure is the distribution of the study centers included in the study


##############********** Supplementary Figure 2 ********#################
##############*  GDP
pal= pal_lancet("lanonc")(9)
show_col(pal)
col <- c ("#b0d097","#fa86a9", "#c09f7e", "#fd8d6e", "#fad354","#c5a6c4","#778ccc" )
windowsFonts(myFont = windowsFont("Arial"))
options (scipen = 999)

# City-LEVEL
bubble <- read_excel ("E:\\HCV_data\\Bubbleplot.xlsx",sheet = "GDP")

model <- lm(pre ~ GDP, data = bubble)
summary(model) # p < 0.001

bubbleplot <- ggplot(bubble, aes(x = GDP, y = pre,colour = region)) +
  xlab("GDP per capita (CNY)") +
  ylab("Prevalence of anti-HCV (%)") +
  theme_minimal(base_size = 12, base_family = "myFont") +
  geom_point(aes(size = N), alpha = 0.8) +
  annotate("text", x = 140000, y = 1.1, label = "y = -0.0000060x + 1.38, P < 0.001") +
  scale_size_area(guide = "none", max_size = 10) + scale_color_manual(values = col)+
  stat_smooth(method = "lm", formula = y ~x, size = 0.5, color = "black", fill = "grey90", linetype="longdash")+
  theme_classic()+
  theme(axis.title = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.line.x = element_line(linetype = 1, color = "black", size = 0.2),
        axis.line.y = element_line(linetype = 1, color = "black", size = 0.2),
        axis.ticks.x = element_line(color = "black", size = 0.2),
        axis.ticks.y = element_line(color = "black", size = 0.2),
        panel.grid = element_blank())+ 
  theme(legend.title = element_blank())+
  scale_y_continuous(limits=c(0,4),breaks=seq(0,4,0.5))
bubbleplot1 <- bubbleplot + theme (legend.position = c(0.63,0.98),
                                   legend.justification = c ("left", "top"),
                                   legend.background = element_blank(),legend.text = element_text(size = 12),
                                   legend.key.size = unit (25, "pt"),
                                   legend.box.background = element_rect (fill = NA, color = NA),
                                   axis.text.x = element_text(size = 12, color = 'black'),
                                   axis.text.y = element_text(size = 12, color = 'black'),
                                   axis.title.x = element_text (size = 14, color = 'black'),
                                   axis.title.y = element_text (size = 14, color = 'black'),
                                   text = element_text(size = 12, color = 'black'))  
bubbleplot1





##############********** Supplementary Figure 3 ********#################
##############*   Cox regression
# Read provided sample example data
forest <- read_excel ("E:\\HCV_data\\Forestplot.xlsx",sheet = "Cox-new")

cairo_pdf("Forest2.pdf", width = 16.2, height = 8)
# indent the subgroup if there is a number in the placebo column
forest$Populations <- ifelse (is.na (forest$`No. of participants (%)`),
                              forest$Populations,
                              paste0("   ", forest$Populations))

# NA to blank or NA will be transformed to charachter.
forest$`No. of participants (%)` <- ifelse(is.na(forest$`No. of participants (%)`), "", forest$`No. of participants (%)`)
forest$`Person-years` <- ifelse(is.na(forest$`Person-years`), "", forest$`Person-years`)
forest$Events <- ifelse(is.na(forest$Events), "", forest$Events)
forest$`per 1,000 person-years` <- ifelse(is.na(forest$`per 1,000 person-years`), "", forest$`per 1,000 person-years`)

# Add blank column for the forest plot to display CI.
# Adjust the column width with space.
forest$` ` <- paste(rep(" ", 20), collapse = " ")
forest$`  ` <- paste(rep(" ", 20), collapse = " ")


# Create confidence interval column to display
forest$`Crude RR (95% CI) ‡` <- forest$F
forest$`Crude RR (95% CI) ‡` <- ifelse(is.na(forest$`Crude RR (95% CI) ‡`), "", forest$`Crude RR (95% CI) ‡`)

forest$`Adjusted RR (95% CI) §` <- forest$J
forest$`Adjusted RR (95% CI) §` <- ifelse(is.na(forest$`Adjusted RR (95% CI) §`), "", forest$`Adjusted RR (95% CI) §`)


# Define theme
tm <- forest_theme (base_size = c (12,12),
                    legend_value = c("", ""), 
                    refline_lty = c ("longdash","longdash"),        # 参考线的线型
                    ci_pch = c(15,15), 
                    ci_col = c("black","black"),
                    footnote_col = "black")

p <- forest(forest[,c(1:5, 14,16,15,17)],
            est = list (forest$est,forest$est1),
            lower = list (forest$low,forest$low1),
            upper = list (forest$high,forest$high1),
            ci_column = c(6,8),
            ref_line = c(1,1),
            x_trans=c("log","log"),
            xlim = list (c(0.8, 6.5),c(0.8, 6.5)),
            ticks_at = list (c(1,2,3,4,5,6),c(1,2,3,4,5,6)),
            arrow_lab = list (c("Decreased risk of \nHCV seroconversion", "Increased risk of \nHCV seroconversion"),c("Decreased risk of \nHCV seroconversion", "Increased risk of \nHCV seroconversion")),
            theme = tm)

# Print plot
plot(p)

f <- insert_text (p, text = "IR (95% CI) †,", col = 5, part = "header", gp = gpar (fontface = "bold"))
f <- add_border (f, part = "header", row = 1, where = "top", gp = gpar(lwd = 1.5))
f <- add_border (f, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1.5))

f <- edit_plot (f, row = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29), which = "background", gp = gpar(fill = "#ECF1F8"))

f

dev.off()




##############********** Supplementary Figure 4 ********#################
##############* This figure was generated based on the raw data.
##############* Access to the health check‑up database is restricted and not available for public use.
##############* For transparency, the code for generating this figure is included below:

raw_data <- read_sas("E:\\HCV_data\\rcs.sas7bdat") %>% data.table()

cairo_pdf("Curve.pdf", width = 24, height = 6.5)

# Age
raw_data [Age >= 18 & Age <30 , agegroupnew := "18-29 years"]
raw_data [Age >= 30 & Age <40 , agegroupnew := "30-39 years"]
raw_data [Age >= 40 & Age <50 , agegroupnew := "40-49 years"]
raw_data [Age >= 50 & Age <60 , agegroupnew := "50-59 years"]
raw_data [Age >= 60 & Age <70 , agegroupnew := "60-69 years"]
raw_data [Age >= 70 , agegroupnew := "≥ 70 years"]
raw_data$agegroupnew <- ordered(raw_data$agegroupnew, levels = c("18-29 years","30-39 years","40-49 years","50-59 years","60-69 years","≥ 70 years"))
table (raw_data$agegroupnew)

m1 <- coxph(Surv(time, outcome)~ agegroupnew + sex + region3, data=raw_data, method = "breslow")
plot <- ggadjustedcurves (m1, data = raw_data, method = "marginal", variable = "agegroupnew",
                          reference = raw_data[raw_data$agegroupnew == "18-29 years",],conf.int = TRUE)

plotdata <- plot$data
plotdata<-plotdata %>% mutate(cumhazard=1-surv)
plotdata$variable <- factor(plotdata$variable,levels = c("18-29 years","30-39 years","40-49 years","50-59 years","60-69 years","≥ 70 years"))

p <- ggplot(data=plotdata, aes(x=time,y=cumhazard)) +geom_line(aes(colour=variable, group=variable), size = 1)+
  labs(x='Follow-up time (year)', y='\n \nCumulative hazard of \nincidence rate of HCV seroconversion\n', col='Age groups')
p

windowsFonts(RMN=windowsFont("Arial"))
p1 <- p + theme_classic()+scale_y_continuous(expand = c(0,0),breaks=c(0.0,0.005,0.010,0.015,0.020)) +
  scale_x_continuous(expand = c(0,0),breaks=c(0,1,2,3,4,5,6))+ scale_color_manual(values = c("#9adbc5","#a1dee0", "#dfde6c", "#fcc351","#fd8d6e", "#fa86a9")) +
  theme(legend.position = c(0.05,0.96),
        legend.justification = c ("left", "top"),
        axis.text.x = element_text(size=12,color='black'),  # 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=12,color='black'), 
        axis.title.x = element_text(size=14,color='black'), 
        axis.title.y = element_text(size=14,color='black'),
        legend.title = element_text(size=12,color='black'),
        legend.text = element_text(size=12,color='black'),
        plot.title = element_text(size=12,color='black')) 
p1

rt <- read_excel("E:\\HCV_data\\risktable.xlsx", sheet = "Age")
rt <- rt[, c("year0", "year1", "year2", "year3","year4","year5","year6")]
rt.p <- ggtexttable(rt, rows = c("18-29 years","30-39 years","40-49 years","50-59 years","60-69 years","≥ 70 years"), cols = NULL, theme = ttheme("blank", base_size = 12))
rt.p1 <- rt.p %>% tab_add_title(text = "Number at risk", face = "plain", size=12) 
# Arrange the plots on the same page
pp1 <- ggarrange(p1, rt.p1, ncol = 1, nrow = 2, heights = c(1, 0.4))
pp1




# Region Border 
raw_data$regionib <- ordered(raw_data$regionib, levels = c("Inland", "Border"))
table (raw_data$regionib)

m2 <- coxph(Surv(time, outcome)~ Age + sex + regionib, data=raw_data, method = "breslow")
plot <- ggadjustedcurves (m2, data = raw_data, method = "marginal", variable = "regionib",
                          reference = raw_data[raw_data$regionib == "Inland",],conf.int = TRUE)

plotdata <- plot$data
plotdata<-plotdata %>% mutate(cumhazard=1-surv)
plotdata$variable <- factor(plotdata$variable,levels = c("Inland", "Border"))

p <- ggplot(data=plotdata, aes(x=time,y=cumhazard)) +geom_line(aes(colour=variable, group=variable), size = 1)+
  labs(x='Follow-up time (year)', y='\n \nCumulative hazard of \nincidence rate of HCV seroconversion\n', col='Living in border regions')
p

windowsFonts(RMN=windowsFont("Arial"))
p2 <- p + theme_classic()+scale_y_continuous(expand = c(0,0),breaks=c(0.0,0.0025,0.0050,0.0075)) +
  scale_x_continuous(expand = c(0,0),breaks=c(0,1,2,3,4,5,6))+ scale_color_manual(values = c( "#acbfeb", "#f17172")) +
  theme(legend.position = c(0.05,0.96),
        legend.justification = c ("left", "top"),
        axis.text.x = element_text(size=12,color='black'),  # 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=12,color='black'), 
        axis.title.x = element_text(size=14,color='black'), 
        axis.title.y = element_text(size=14,color='black'),
        legend.title = element_text(size=12,color='black'),
        legend.text = element_text(size=12,color='black'),
        plot.title = element_text(size=12,color='black')) 
p2

rt <- read_excel("E:\\HCV_data\\risktable.xlsx", sheet = "Region")
rt <- rt[, c("year0", "year1", "year2", "year3","year4","year5","year6")]
rt.p <- ggtexttable(rt, rows = c("Yes","No"), cols = NULL, theme = ttheme("blank", base_size = 12))
rt.p2 <- rt.p %>% tab_add_title(text = "Number at risk", face = "plain", size=12) 
# Arrange the plots on the same page
pp2 <- ggarrange(p2, rt.p2, ncol = 1, nrow = 2, heights = c(1, 0.4))
pp2



# GDPcat
raw_data$GDPcat <- ordered(raw_data$GDPcat, levels = c( "High" , "Low"))
table (raw_data$GDPcat)

m3 <- coxph(Surv(time, outcome)~ Age + sex + region3 + GDPcat, data=raw_data, method = "breslow")
plot <- ggadjustedcurves (m3, data = raw_data, method = "marginal", variable = "GDPcat",
                          reference = raw_data[raw_data$GDPcat == "High",],conf.int = TRUE)

plotdata <- plot$data
plotdata<-plotdata %>% mutate(cumhazard=1-surv)
plotdata$variable <- factor(plotdata$variable,levels = c("High", "Low"))

p <- ggplot(data=plotdata, aes(x=time,y=cumhazard)) +geom_line(aes(colour=variable, group=variable), size = 1)+
  labs(x='Follow-up time (year)', y='\n \nCumulative hazard of \nincidence rate of HCV seroconversion\n', col='Level of GDP per capita of the\n living areas of the participants')
p

windowsFonts(RMN=windowsFont("Arial"))
p3 <- p + theme_classic()+scale_y_continuous(expand = c(0,0),breaks=c(0.0,0.003,0.006,0.009)) +
  scale_x_continuous(expand = c(0,0),breaks=c(0,1,2,3,4,5,6))+ scale_color_manual(values = c( "#fad354", "#8475c5")) +
  theme(legend.position = c(0.05,0.96),
        legend.justification = c ("left", "top"),
        axis.text.x = element_text(size=12,color='black'),  # 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=12,color='black'), 
        axis.title.x = element_text(size=14,color='black'), 
        axis.title.y = element_text(size=14,color='black'),
        legend.title = element_text(size=12,color='black'),
        legend.text = element_text(size=12,color='black'),
        plot.title = element_text(size=12,color='black')) 
p3

rt <- read_excel("E:\\HCV_data\\risktable.xlsx", sheet = "GDPcat")
rt <- rt[, c("year0", "year1", "year2", "year3","year4","year5","year6")]
rt.p <- ggtexttable(rt, rows = c("Low","High"), cols = NULL, theme = ttheme("blank", base_size = 12))
rt.p3 <- rt.p %>% tab_add_title(text = "Number at risk", face = "plain", size=12) 
# Arrange the plots on the same page
pp3 <- ggarrange(p3, rt.p3, ncol = 1, nrow = 2, heights = c(1, 0.4))
pp3


# Precat
raw_data$Precat <- ordered(raw_data$Precat, levels = c( "Low", "High"))
table (raw_data$Precat)

m4 <- coxph(Surv(time, outcome)~ Age + sex + region3 + Precat, data=raw_data, method = "breslow")
plot <- ggadjustedcurves (m4, data = raw_data, method = "marginal", variable = "Precat",
                          reference = raw_data[raw_data$Precat == "Low",],conf.int = TRUE)

plotdata <- plot$data
plotdata<-plotdata %>% mutate(cumhazard=1-surv)
plotdata$variable <- factor(plotdata$variable,levels = c("Low", "High"))

p <- ggplot(data=plotdata, aes(x=time,y=cumhazard)) +geom_line(aes(colour=variable, group=variable), size = 1)+
  labs(x='Follow-up time (year)', y='\n \nCumulative hazard of \nincidence rate of HCV seroconversion\n', col='Anti-HCV prevalence of the\n living areas of the participants')
p

windowsFonts(RMN=windowsFont("Arial"))
p4 <- p + theme_classic()+scale_y_continuous(expand = c(0,0),breaks=c(0.0,0.003,0.006,0.009,0.012)) +
  scale_x_continuous(expand = c(0,0),breaks=c(0,1,2,3,4,5,6))+ scale_color_manual(values = c( "#71bcec", "#d35b7e")) +
  theme(legend.position = c(0.05,0.96),
        legend.justification = c ("left", "top"),
        axis.text.x = element_text(size=12,color='black'),  # 设置x轴字体大小，以下同理
        axis.text.y = element_text(size=12,color='black'), 
        axis.title.x = element_text(size=14,color='black'), 
        axis.title.y = element_text(size=14,color='black'),
        legend.title = element_text(size=12,color='black'),
        legend.text = element_text(size=12,color='black'),
        plot.title = element_text(size=12,color='black')) 
p4

rt <- read_excel("E:\\HCV_data\\risktable.xlsx", sheet = "Precat1")
rt <- rt[, c("year0", "year1", "year2", "year3","year4","year5","year6")]
rt.p <- ggtexttable(rt, rows = c("Low","High"), cols = NULL, theme = ttheme("blank", base_size = 12))
rt.p4 <- rt.p %>% tab_add_title(text = "Number at risk", face = "plain", size=12) 
# Arrange the plots on the same page
pp4 <- ggarrange(p4, rt.p4, ncol = 1, nrow = 2, heights = c(1, 0.4))
pp4


##############********** Combine 4 Plots **********##############
patchwork <- (pp1 + pp2 + pp3 + pp4 + plot_layout(ncol = 4, nrow = 1, widths = c(0.25,0.25,0.25,0.25)))  + plot_annotation(tag_levels = 'A')
patchwork

dev.off()
