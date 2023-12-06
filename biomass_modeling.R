#install the required packages
#install.packages(c("SP","rgdal","raster","ggplot2","viridis","rasterVis"))
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("sf")
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)
library(sf)
library(dplyr)


#loading the data
Image<-raster("resampled_image.tif") 


#loading boundary of study area
boundary<- readOGR("polygon.shp")


#creating an individual raster layers for each of the spectral bands

b1<- raster("resampled_image.tif", band=1)
b2<- raster("resampled_image.tif", band=2)
b3<- raster("resampled_image.tif", band=3)
b4<- raster("resampled_image.tif", band=4)
b5<- raster("b5_june_26.tiff.tif")
b6<- raster("resampled_image.tif", band=6)
b7<- raster("resampled_image.tif", band=7)
b8<- raster("b8.tiff.tif")
b8a<-raster("b8a.tiff.tif")
b9<- raster("resampled_image.tif", band=9)
b10<- raster("resampled_image.tif", band=10)
b11<- raster("resampled_image.tif", band=11)
b12<- raster("resampled_image.tif", band=12)

#compare the raster images
compareRaster(b2,b3)
compareRaster(b9,b2)

#####
#lets crops and mask our study area for each bands

SA_crop1<- crop(b1,boundary)
SA_b1<- mask(SA_crop1, boundary)
SA_b1_scaled<- SA_b1/10000 #lets rescale the image by scaling factor 10000
hist(SA_b1_scaled)
SA_b1_scaled

SA_crop2<- crop(b2,boundary)
SA_b2<- mask(SA_crop2, boundary)
SA_b2_scaled<-SA_b2/10000
SA_b2_scaled

### continue for other bands also

#create RGB Image of the study area

RGB_image<- stack(list(b4,b3,b2))
plotRGB(RGB_image, axes= TRUE, stretch="lin", main= "RGB Composite of Study Area")

# create false color composite

FCC_image<- stack(list(b8,b4,b3))
plotRGB(FCC_image, axes= TRUE, stretch="lin", main=" FCC composite of Study area")

#visualize using ggplot2

gplot(b3) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("study area") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					   
  theme(plot.title = element_text(hjust = 0.5),             
        text = element_text(size=20),		       	   
        axis.text.x = element_text(angle = 90, hjust = 1)) 


#visualize all the bands together using facet
band_stack<- stack(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)
gplot(band_stack) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Sentinel 2 study area, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#### use brick function to display image 
imagebrick<- brick("Resampled_image.tif")
plot(imagebrick)


###################Now calculate vegetation indices#############################

NDVI<-(SA_b8a_scaled-SA_b4_scaled)/(SA_b8a_scaled+SA_b4_scaled)
#plot(NDVI)
#title(" NDVI of the study area")
output_path<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\ndvi.tif"
writeRaster(NDVI, filename= output_path, format="GTiff")



NDVI45<- (SA_b5_scaled-SA_b4_scaled)/(SA_b5_scaled+SA_b4_scaled)
NDVI45


NDVI65<- (SA_b6_scaled-SA_b5_scaled)/(SA_b6_scaled+SA_b5_scaled)
NDVI65

RGR<-(SA_b4_scaled/SA_b3_scaled)
RGR

EVI8<- 2.5*(SA_b8a_scaled-SA_b4_scaled)/(1+SA_b8a_scaled+6*SA_b4_scaled-(7.5*SA_b2_scaled))
plot(EVI8)
EVI8

EVI7<- 2.5*(SA_b7_scaled-SA_b4_scaled)/(1+SA_b7_scaled+6*SA_b4_scaled-(7.5*SA_b2_scaled))
plot(EVI7)
EVI7


SR<- SA_b8_scaled/SA_b4_scaled
SR

PSRI<- (SA_b4_scaled-SA_b3_scaled)/SA_b8_scaled
PSRI

NDII11<-(SA_b8a_scaled-SA_b11_scaled)/(SA_b8a_scaled+SA_b11_scaled)
plot(NDII11)
NDII11

NDII12<-(SA_b8a_scaled-SA_b12_scaled)/(SA_b8a_scaled+SA_b12_scaled)
plot(NDII12)
NDII12

RE_NDVI<-(SA_b8a_scaled-SA_b6_scaled)/(SA_b8a_scaled+SA_b6_scaled)
plot(RE_NDVI)
RE_NDVI

SAVI<- (SA_b8-SA_b4)/(SA_b8+SA_b4+0.428)*(1+0.428) #not scaled
SAVI

IRECI<- (SA_b7_scaled-SA_b4_scaled)/(SA_b6_scaled/SA_b5_scaled)
plot(IRECI)
IRECI

S2REP<- 705+35*(((SA_b7_scaled+SA_b4_scaled)/2)-SA_b5_scaled)/(SA_b6_scaled-SA_b5_scaled)
S2REP

WDRVI<- ((0.01*SA_b7_scaled)-SA_b4_scaled)/((0.01*SA_b7_scaled+SA_b4_scaled)+((1-0.01)/(1+0.01)))
WDRVI
plot(WDRVI)

MTCI<- (SA_b6_scaled-SA_b5_scaled)/(SA_b5_scaled-SA_b4_scaled)
MTCI

MSR<- ((SA_b7_scaled/SA_b4_scaled)-1)/sqrt((SA_b7_scaled/SA_b4_scaled)+1)
MSR

GNDVI<- (SA_b8a_scaled-SA_b3_scaled)/(SA_b8a_scaled+SA_b3_scaled)
GNDVI

CIRE<- (SA_b7_scaled/SA_b5_scaled)-1
CIRE

ARI1<-(1/SA_b3_scaled)-(1/SA_b5_scaled)
ARI1

ARI2<-(SA_b8a_scaled/SA_b3_scaled)-(SA_b8a_scaled/SA_b5_scaled)
ARI2

CRI1<- (1/SA_b2_scaled)-(1/SA_b3_scaled)
CRI1

CRI2<- (1/SA_b2_scaled)-(1/SA_b5_scaled)
CRI2

MCARI<- 1-((0.2)*(SA_b5_scaled-SA_b3_scaled)/(SA_b5_scaled-SA_b4_scaled))
MCARI

Red_edge_1<- (SA_b8-SA_b5)/(SA_b8+SA_b5)
Red_edge_1

Red_edge_2<- (SA_b8-SA_b6)/(SA_b8+SA_b6)
Red_edge_2

Red_edge_3<- (SA_b8-SA_b7)/(SA_b8+SA_b7)
Red_edge_3

Red_edge_4<- (SA_b8-SA_b8a)/(SA_b8+SA_b8a)
Red_edge_4



##### import csv file####
sample_points<- read.csv("filed_data_csv.csv")
sample_points
plot(sample_points)

#lets first assign crs to sample_points

SP_crs<- st_as_sf(sample_points, coords = c("X","Y"), crs= "+proj=utm +zone=44 +datum=WGS84")
print(SP_crs)

##### now lets extract the pixels values from plot area#####
# the plot is circular with radius 12.61m

#lets try for each bands first

PV_b1<- raster::extract(SA_b1_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b1
PV_b1$b1<- PV_b1$resampled_image_1
PV_b1

PV_b2<- raster::extract(SA_b2_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b2
PV_b1$b2<- PV_b2$resampled_image_1
PV_b1

PV_b3<- raster::extract(SA_b3_scaled, SP_crs, buffer= 12.61, fun= mean, df=TRUE)
PV_b3
PV_b1$b3<- PV_b3$resampled_image_1
PV_b1
#### continue for other bands also


#lets save the data frame
library(writexl)
write_xlsx(PV_b1, path = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\bands_pixel_8_10.xlsx")
hist(PV_b1$b2)
plot(PV_b1$b2)

# now  lets extract pixel values from vegetation index####
Pixel_value<- raster::extract(NDVI, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
View(Pixel_value)


Pixel_value$Plot<- SP_crs$Plot
Pixel_value

# lets fix the column names
names(Pixel_value)<- c("ID", "NDVI","Plot")
Pixel_value
hist(Pixel_value$NDVI)

Pixel_value_NDVI45<- raster::extract(NDVI45, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$NDVI45<-Pixel_value_NDVI45$layer

Pixel_value_NDVI65<- raster::extract(NDVI65, SP_crs, buffer= 12.61, fun=mean, df=TRUE)
Pixel_value$NDVI65<-Pixel_value_NDVI65$layer

#### continue for other vegetation indices also

#lets save the data frame in xlsx format in local disk
#install.packages("writexl")
library(writexl)
write_xlsx(Pixel_value, path = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\new_pixel_data_8_10.xlsx")

head(Pixel_value)







#### lets predict the biomass in our study area using the machine learning model##############################

library(caret)
library(caTools)
library(VSURF)
data<-read.csv("F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\ML_try.csv")
Ddata<-data[-c(28,27,47,39,13),]   #removing the bias plots from our data
write.csv(Ddata, file = "F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\Ddata.csv", row.names = FALSE)
Variable_selection<-VSURF(AGB~ .,
                          data = data, variable.names=TRUE)
plot(Variable_selection,var.names=TRUE)
plot(Variable_selection,step="pred",var.names=TRUE)
summary(Variable_selection)

controlparameters<-trainControl(method = "repeatedcv",
                                number=5,
                                repeats = 5,
                                savePredictions=TRUE,
                                classProbs = TRUE)

########################RF MODEL using vsurf variables ####################################################################################
#set.seed(123)
Final_Model<-train(AGB~b11+S2REP_new+WDRVI
                   ,
                   data = Ddata,
                   method='rf',   # use svmLinear, svmPoly, and svmRadial for SVM algorithm
                   trcontrol=controlparameters)

Final_Model$results
Final_Model$finalModel









#######lets predict##########################################

#lets save the raster files into local device first
output_path1<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\b11.tif"
writeRaster(SA_b11_scaled, filename= output_path1, format="GTiff")

output_path2<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\S2REP_new.tif"
writeRaster(S2REP_new, filename= output_path2, format="GTiff")


output_path3<-"F:\\2078 and 79\\MSc\\4th sem\\my_data\\R try\\sentinel2\\vegetation_indices\\WDRVI.tif"
writeRaster(WDRVI, filename= output_path3, format="GTiff")


############ lets make the raster stacks first#####################
file_paths<- c(output_path1, output_path2,output_path3)

raster_stack<- stack(file_paths)

print(raster_stack)
raster_stack[[1]]
raster_stack[[2]]
raster_stack[[3]]

names(raster_stack)

predRaster <- predict(raster_stack, Final_Model)
plot(predRaster)
mean(predRaster)
plot(predRaster, main = "Aboveground Forest Biomass Map")



# Calculate mean
mean_val <- cellStats(predRaster, stat = 'mean')

# Calculate minimum
min_val <- cellStats(predRaster, stat = 'min')

# Calculate maximum
max_val <- cellStats(predRaster, stat = 'max')

# Calculate standard deviation
sd_val <- cellStats(predRaster, stat = 'sd')


print(paste("Mean: ", mean_val))
print(paste("Min: ", min_val))
print(paste("Max: ", max_val))
print(paste("Standard Deviation: ", sd_val))




# Save the raster image into a TIFF file
writeRaster(predRaster, filename = "biomassmap.tif", format = "GTiff", overwrite = TRUE)


###########for carbon map#########
# Calculate Carbon from AGB
carbonRaster <- predRaster * 0.47

# Plot Carbon Map
plot(carbonRaster, main = "Forest Carbon Map")

writeRaster(carbonRaster, filename = "carbonmap.tif", format = "GTiff", overwrite = TRUE)

# Calculate mean
mean_val <- cellStats(carbonRaster, stat = 'mean')

# Calculate minimum
min_val <- cellStats(carbonRaster, stat = 'min')

# Calculate maximum
max_val <- cellStats(carbonRaster, stat = 'max')

# Calculate standard deviation
sd_val <- cellStats(carbonRaster, stat = 'sd')

print(paste("Mean: ", mean_val))
print(paste("Min: ", min_val))
print(paste("Max: ", max_val))
print(paste("Standard Deviation: ", sd_val))

