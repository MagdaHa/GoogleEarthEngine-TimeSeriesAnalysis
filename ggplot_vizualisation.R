##########################################################################################
#### GENERIC VIZUALISATION

#### author: Magdalena Halbgewachs
#### August 2019
#### R version 3.6.1

##########################################################################################

#### Content: 4 generic functions that display (spatial) data quickly and easily in plots and diagrams.

#### FUNCTION 1: Raster stack time series with LEVELPLOT
#### FUNCTION 2: Time series plot with GGPLOT
#### FUNCTIoN 3: Correlation plot with CORRPLOT
#### FUNCTION 4: Time series animation with GGPLOTLY


#### all used datasets are provided in the folder '01_data'.

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
library(gganimate)

#Set working directory
setwd ("C:\\02_Studium\\02_Master\\02_Semester_2\\MET3_Python\\01_final_project\\GoogleEarthEngine-TimeSeriesAnalysis")

##########################################################################################
# 1.) RASTER STACK TIME SERIES
# simple display of raster time series, for example indices
##########################################################################################

#FUNCTION
ts_raster <- function(stack, main, names.attr) {
  cols <- colorRampPalette(brewer.pal(11,"Spectral"))    #  number of colors
  levelplot <- levelplot(stack,
            main = main,
            col.regions=cols,
            names.attr=names.attr,                     # using new defined names
            scales=list(draw=FALSE ))                  # remove axes labels & ticks
  return(levelplot)
}

#-----------------------------------------------------------------------------------------
#EXAMPLE
#loading all NDWI images with the pattern TIF, stored as list
datasets <- ".\\01_data\\NDWI"
all_datasets <- list.files(datasets, full.names = TRUE, pattern = ".tif$")
all_datasets

# Create a raster stack of the NDWI time series
data_stack <- stack(all_datasets)

# dataset specific
# view names for each raster layer
names(data_stack)
# gsub to modify label names
rasterNames  <- gsub("NDWI_","", names(data_stack))

main <- "NDWI\nLake Poopó, Bolivia (1989 - 2018)"
names.attr <- rasterNames


ts_raster(data_stack, main, names.attr)


##########################################################################################
# 2.) TIME SERIES PLOTS
# line plot of different variables within a datatable over a predefined time frame
##########################################################################################

#FUNCTION
ts_plot <- function(df_data, column_x, column_y1, column_y2, xlab, ylab, ggtitle, scale_x_date) {
  plot <- ggplot(data=df_data, aes(x=column_x, y=column_y1, group=1, color=legend)) +
    #data
    geom_bar(data=df_data, aes(x=column_x, y=column_y1, color="April"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
    geom_bar(data=df_data, aes(x=column_x, y=column_y2, color="July"), stat="identity", alpha=0.3, width=300, fill="transparent", size=1)+
    #linear regression
    geom_smooth(data=df_data, aes(x=column_x, y=column_y1), method='lm',formula=y~x, color="#F8766D", linetype="dashed", se=F)+
    geom_smooth(data=df_data, aes(x=column_x, y=column_y2), method='lm',formula=y~x, color="#00BFC4", linetype="dashed", se=F)+
    #themes
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 6),
          axis.text.y=element_text(angle = 45, size = 9))+ 
    #axis
    xlab+
    ylab+
    scale_x_date+
    ggtitle+
    #labs(caption = "data source: Servicio Nacional de Meteorología e Hidrología de Bolivia")+
    theme_gray(base_size = 15)
  return(plot)
}

#-----------------------------------------------------------------------------------------
#EXAMPLE: precipitation Lake Poopó April and July
df_data <- read.csv(".\\01_data\\prec_et_april_july.csv", header=T, sep=";")

#dataset specific
if(names(df_data)[1]=="ï..YEAR"){             #replaces mysterious column name
  names(df_data)[1]<- "YEAR"
}
df_data[[1]] <- as.Date(df_data[[1]], origin="01-04-1989")   #read as date
names(df_data) <- c ("YEAR", "PREC_APRIL", "PREC_JULY", "ET_APRIL", "ET_JULY")

column_x <- df_data[[1]]
column_y1 <- df_data[[2]]
column_y2 <- df_data[[3]]
xlab <- xlab("year")
ylab <- ylab("precipitation (mm)")
ggtitle <-   ggtitle("Mean evapotranspiration in the Lake Poopó area in April and July (1989-2018)")
scale_x_date <- scale_x_date(breaks = as.Date(c("1989-04-01", "1995-04-01", "1999-04-01",
                                "2005-04-01", "2009-04-01", "2013-04-01",
                                "2014-04-01", "2015-04-01", "2016-04-01", 
                                "2017-04-01","2018-04-01")),date_labels = "%Y")


ts_plot(df_data, column_x, column_y1, column_y2, xlab, ylab, ggtitle, scale_x_date)


##########################################################################################
# 3.) CORRELATIONS
# Plot correlations between different variables
##########################################################################################
# --> Input file is a csv that consists of different (previously spatial data) variables that are related to each othe
#     in a way. In this case I used the csv file I exported from my Google Earth Engine project.
# --> Aim: a simple and clear representation of the relationships between the variables
#     in order to draw further conclusions.

#FUNCTION
corr_plot <- function(corr) {
  #matrix
  corr_na <- cor(corr, use = "complete.obs") # handling missing values
  res <- cor(corr_na)
  round(res, 2)
  #correlation plot
  corrplot(res, method="number", tl.col = "black", type="upper", order="hclust", tl.srt = 45)
  return(corrplot)
}

#-----------------------------------------------------------------------------------------
#EXAMPLE
#loading csv
corr <- read.csv(".\\01_data\\df_all_T.csv", header=T, sep=";")

# only necessary if NA values or not required columns are in the dataset
# delete NA rows
corr<-corr[-c(1:13),]

#dataset specific
# delete column "year
corr = corr[,-c(1)]
head(corr, 10)
#rename columns for better interpretation
colnames(corr)[1:5] <- c("evapotranspiration", "precipitation", "area Poopó", "area Titicaca", "height Titicaca")


corr_plot(corr)


##########################################################################################
# 4.) ANIMATION
# animating data which habe a timestamp
##########################################################################################
# --> Input file is a csv that consists of different (previously spatial data) variables that are related to each other by time.
# --> Aim: a simple and clear representation of the variables by time

#FUNCTION
anim <- function(data, varx, vary, color, size, year, ids, xlab, ylab) {
  plot <- ggplot(data, aes(varx, vary, color=color))+
    geom_point(aes(size=size, frame=year, ids=ids))+
    scale_x_log10()+
    xlab+
    ylab
  plotly <- ggplotly(plot)
  return(plotly)
}

#-----------------------------------------------------------------------------------------
# EXAMPLE 1: world population
data <- read.csv(".\\01_data\\world_population.txt")
dim(data)
head(data)

varx <- data[[3]]
vary <- data[[5]]
color <- data[[4]]
size <- data[[6]]
year <- data[[2]]
ids <- data[[1]]
xlab <- xlab("population")
ylab <- ylab("life expectation")


anim(data, varx, vary, color, size, year, ids, xlab, ylab)    # press PLAY button in animation


#-----------------------------------------------------------------------------------------
# EXAMPLE 2: Lake Poopò
data <- read.csv(".\\01_data\\df_all_T.csv", header=T, sep=";")
dim(data)
head(data)

varx <- data[[1]]
vary <- data[[4]]
color <- data[[6]]
size <- data[[6]]
year <- data[[1]]
ids <- data[[4]]
xlab <- xlab("year")
ylab <- ylab("water area Lake Poopó [km²]")

anim(data, varx, vary, color, size, year, ids, xlab, ylab)    # press PLAY button in animation

         
##########################################################################################
