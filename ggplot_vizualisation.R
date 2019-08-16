##########################################################################################
#### Generic vizualisation
#### author: Magdalena Halbgewachs
#### August 2019
##########################################################################################

##########################################################################################

#loading required packages
library(ggplot2)
library(plotly)
library(ggplot2)
library(rgdal)
library(raster)
library(rasterVis)
library(maptools)
library(corrplot)
library(RStoolbox)


setwd ("C:\\02_Studium\\02_Master\\02_Semester_2\\MET3_Python\\01_final_project\\GoogleEarthEngine-TimeSeriesAnalysis")

##########################################################################################
# 1.) RASTER STACK TIME SERIES
# simple display of raster time series, for example indices
##########################################################################################
#loading all NDWI images with the pattern TIF, stored as list
NDWI <- ".\\01_data\\NDWI"
all_NDWI <- list.files(NDWI, full.names = TRUE, pattern = ".tif$")
all_NDWI

#----------------------------------------------------------------------------------------
# Create a raster stack of the NDWI time series
NDWI_stack <- stack(all_NDWI)
# view names for each raster layer
names(NDWI_stack)
# gsub to modify label names
rasterNames  <- gsub("NDWI_","", names(NDWI_stack))

#----------------------------------------------------------------------------------------
#levelplot
cols <- colorRampPalette(brewer.pal(11,"Spectral"))    #  number of colors

levelplot(NDWI_stack,
          main="NDWI\nLake Poopó, Bolivia (1989 - 2018)",
          col.regions=cols,
          names.attr=rasterNames,                     # using new defined names
          scales=list(draw=FALSE ))                  # remove axes labels & ticks


##########################################################################################
# 2.) TIME SERIES PLOTS 
##########################################################################################
###precipitation and evapotranspiration April and July 1989 - 2018
df_data <- read.csv(".\\01_data\\prec_et_april_july.csv", header=T, sep=";")
if(names(df_data)[1]=="ï..YEAR"){             #replaces mysterious column name
  names(df_data)[1]<- "YEAR"
}
df_data[[1]] <- as.Date(df_data[[1]], origin="01-04-1989")   #read as date
names(df_data) <- c ("YEAR", "PREC_APRIL", "PREC_JULY", "ET_APRIL", "ET_JULY")

#---------------------------------------------------------------------------------------

et_plot <- ggplot(data=df_data, aes(x=YEAR, y=ET_APRIL, group=1, color=legend)) +
  #data
  geom_bar(data=df_data, aes(x=YEAR, y=ET_APRIL, color="April"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
  geom_bar(data=df_data, aes(x=YEAR, y=ET_JULY, color="July"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
  #linear regression
  geom_smooth(data=df_data, aes(x=YEAR, y=ET_APRIL), method='lm',formula=y~x, color="#F8766D", linetype="dashed", se=F)+
  geom_smooth(data=df_data, aes(x=YEAR, y=ET_JULY), method='lm',formula=y~x, color="#00BFC4", linetype="dashed", se=F)+
  #themes
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 6),
        axis.text.y=element_text(angle = 45, size = 9))+ 
  #axis
  xlab("year")+
  ylab("evapotranspiration (mm)")+
  scale_x_date(breaks = as.Date(c("1989-04-01", "1995-04-01", "1999-04-01",
                                  "2005-04-01", "2009-04-01", "2013-04-01",
                                  "2014-04-01", "2015-04-01", "2016-04-01", 
                                  "2017-04-01","2018-04-01")),date_labels = "%Y")+
  ggtitle("Mean evapotranspiration in the Lake Poopó area in April and July (1989-2018)")+
  labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
  theme_gray(base_size = 15)
et_plot


##########################################################################################
# 3.) CORRELATIONS
# Plot correlations between different variables
##########################################################################################
# --> Input file is a csv that consists of different (previously spatial data) variables that are related to each othe
#     in a way. In this case I used the csv file I exported from my Google Earth Engine project.
# --> Aim: a simple and clear representation of the relationships between the variables
#     in order to draw further conclusions.

#loading csv
corr <- read.csv(".\\01_data\\df_all_T.csv", header=T, sep=";")

#------------only necessary of NA values or not required columns ar in the dataset----------
# only necessary of NA values or not required columns ar in the dataset
# delete NA rows
corr<-corr[-c(1:13),]

# delete column "year
corr = corr[,-c(1)]
head(corr, 10)
#-------------------------------------------------------------------------------------------

#rename columns for better interpretation
colnames(corr)[1:5] <- c("evapotranspiration", "precipitation", "area Poopó", "area Titicaca", "height Titicaca")

#matrix
corr_na <-cor(corr, use = "complete.obs") # handling missing values
res <- cor(corr_na)
round(res, 2)

#plot correlations
corrplot(res, method="number", tl.col = "black", type="upper", order="hclust", tl.srt = 45)

##########################################################################################
# 4.) ANIMATION
# animating data which habe a timestamp
##########################################################################################
# --> Input file is a csv that consists of different (previously spatial data) variables that are related to each other by time.
# --> Aim: a simple and clear representation of the variables by time

# example1: world population
data <- read.csv(".\\01_data\\world_population.txt")
dim(data)
head(data)

p_plot <- ggplot(data, aes(pop, lifeExp, color=continent))+
  geom_point(aes(size=gdpPercap, frame=year, ids=country))+
  scale_x_log10()+
  xlab("population")+
  ylab("life expectation")

p_plot

ggplotly(p_plot)

#---------------------------
#example2: Lake Poopò
data_P <- read.csv(".\\01_data\\df_all_T.csv", header=T, sep=";")
dim(data_P)
head(data_P)

p_plot_P <- ggplot(data_P, aes(year, area_Poopo, color=height_Titicaca))+
  geom_point(aes(size=height_Titicaca, frame=year, ids=area_Poopo))+
  xlab("year")+
  ylab("area Lake Poopó (km²)")+
  ggtitle("Correlation between the area of Lake Poopó and water height \nof Lake Titicaca (1989 - 2019)")+
  theme_bw()
  
#p_plot_P
ggplotly(p_plot_P)
         
##########################################################################################
