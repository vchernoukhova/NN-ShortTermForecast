require('sqldf')

unique(data_fin$Utility)
unique(data_fin$Zone[data_fin$Utility=='ConEd'])
unique(data_fin$Zone[data_fin$Utility=='NYSEG'])
unique(data_fin$Zone[data_fin$Utility=='NIMO'])
unique(data_fin$Zone[data_fin$Utility=='CentralHudson'])
unique(data_fin$Zone[data_fin$Utility=='RGE'])
unique(data_fin$Zone[data_fin$Utility=='OR'])

data_sub <- subset(data_fin, Entity !='Hudson' & 
                     (Zone=='J' | Zone=='A' |Zone== 'B'|Zone=='C'))
data_grouped <- sqldf('select 
                        FlowDateTime, Month, VolumeType, Zone,
                        sum(Volume) as Volume 
                        from data_sub 
                        group by FlowDateTime, Month, VolumeType, Zone')


#to change the order
data_grouped$Month <- factor(data_grouped$Month,
                         levels = c('June','July','August', 'September'))


zones <- as.data.frame(unique(data_grouped$Zone))
names(zones)[1] <- 'Zone'
for (i in 1:nrow(zones)){
    zones$range[i] <- max(data_grouped$Volume[data_grouped$Zone==zones$Zone[i]])-min(data_grouped$Volume[data_grouped$Zone==zones$Zone[i]])
}

head(zones)

data_grouped$ZoneSort <- factor(data_grouped$Zone,
                            levels = levels(zones[,1])[zones[order(-zones$range),1]])


mypath <- file.path('d:/Work/Presentation',
                    paste("Just Energy (big zones)",".png",sep =''))


png(file=mypath,width = 2560, height = 1024,units = 'px')
qplot(FlowDateTime, Volume, data=data_grouped, group=VolumeType,
      color=VolumeType, geom='line', xlab = 'Date', main = 'Just Energy: biggest zones') + 
        scale_colour_manual(values=colors) +
        facet_grid(ZoneSort~Month, scales = 'free') +
        theme_bw() +
    geom_line(size = 0.75)  +
  theme(axis.text.x  = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.text.y  = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12))
 # scale_colour_brewer(type='seq',palette='Paired')
   # scale_fill_brewer(palette="Paired")
    #scale_fill_brewer('Paired')
dev.off()
#######################



