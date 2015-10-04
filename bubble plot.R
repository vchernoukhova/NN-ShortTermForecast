library(plyr) # for round_any
require (ggplot2)

data_file <- 'U:/_Load Forecasting/Victoria/Short Term NN model/!presentation/rcecounts.csv'

data      <- read.csv (data_file, sep = ',', header = TRUE)
head(data)


data$LSE.Name <- as.character(data$LSE.Name)
data$LSE.Name[data$LSE.Name!='HUDSON'] <- 'Just Energy'
data$LSE.Name[data$LSE.Name=='HUDSON'] <- 'Hudson'

#mypath <- file.path('U:/_Load Forecasting/Victoria/Short Term NN model/Plots for Hanna',
#                   "plot.png")

names(data)[1] <- 'Book'
names(data)[2] <- 'Utility'

data$Zone <- substring(data$Zone, 6)

y_axis <- c(2:18)

#max_err <- round_any(max(data$RCECount[data$MAPE>9]),10)
#vector <- c(0:18)*2
#x_axis <- vector*max_err
x_axis <- c(0:13)*30000
#png(file=mypath,width = 2560, height = 1024,units = 'px')

data <- subset (data, Book == 'Hudson' )

op <- options()
options(scipen = 7)

qplot(ActualVolume, MAPE, data=data, 
       geom='point', xlab = 'Actual Volume', main = 'MAPE Analysis', color = Utility, size=RCECount, label = Zone) +
        geom_point(size=3) +  
     #   scale_colour_manual(values=c('blue','red')) +
      #  facet_grid(.~Utility, scales = 'free') +
        #geom_jitter() +
        theme_bw() +
    #scale_size_continuous(range = c(13, 25)) + 
    geom_text(size=5, colour = 'black') +
    scale_size_area(breaks=c(60,600, 1000, 2000,3000, 4000,6000, 8000, 10000, 30000),  max_size=70) +
scale_colour_brewer(palette="Pastel2")+

        scale_x_continuous(breaks=x_axis) +
         scale_y_continuous(breaks=y_axis) 
        #  theme(axis.text.x  = element_text(face = "bold", size = 12),
        #        axis.title.x = element_text(face = "bold", size = 12),
        #       axis.text.y  = element_text(face = "bold", size = 12),
        #       axis.title.y = element_text(face = "bold", size = 12),
        #        strip.text.x = element_text(face = "bold", size = 12))
#dev.off()
options(op)
#########################################

data_outliers <- subset (data, ActualVolume < 300000 ) 

qplot(ActualVolume, MAPE, data=data_outliers, 
      geom='point', xlab = 'Actual Volume', main = 'MAPEs versus RCECount', color = Utility, shape = Zone, size=RCECount) +
    geom_point(size=3) +  
    #   scale_colour_manual(values=c('blue','red')) +
    facet_grid(Utility~., scales = 'free') +
    #geom_jitter() +
    theme_bw() 





