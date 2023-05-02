####################################################################################################
##QMSS 301 PROJECT 3: GEOSPATIAL ANALYSIS
##Alexis Hancz 
##20 April 2023
####################################################################################################
#PREPARATION 
####################################################################################################
#loading the necessary libraries 
library(sf) #for working with spatial data, performing spatial joins 
library(dplyr) #for data manipulation and transformation 
library(tmap) #for visualizing spatial dara 


#reading in the relevant shipfiles for Detroit grocery, neighborhood, crime and zipcode data using st_read() and saving them as spatial object dataframes 
grocery_data <- st_read("/Users/alexishancz/Desktop/Project_3_Data/Grocery_Stores_in_City_of_Detroit_Public_View.shp") #specifying path to file 
neighborhood_data <- st_read("/Users/alexishancz/Desktop/Project_3_Data/Neighborhoods.shp")
crime_data <- st_read("/Users/alexishancz/Desktop/Project_3_Data/RMS_Crime_Incidents.shp")
zipcode_data <- st_read("/Users/alexishancz/Desktop/Project_3_Data/zip_codes.shp")


#retrieving the Coordinate Reference System (CRS) of the spatial object dataframes using st_crs
st_crs(grocery_data) 
st_crs(neighborhood_data) 
st_crs(zipcode_data) 
st_crs(crime_data) 


#checking if the crs are projected using st_is_longlat() function to see if data is represented as longitude-latitude coordinates, which are not projected
st_is_longlat(grocery_data) #TRUE; CRS is not projected 
st_is_longlat(neighborhood_data) #TRUE; CRS is not projected 
st_is_longlat(crime_data) #TRUE; CRS is not projected 
st_is_longlat(zipcode_data) #TRUE; CRS is not projected 


#transforming the longlat coordinates into planar (x,y) coordinates using st_transform, which projects the data
grocery_p <- st_transform(grocery_data, crs = 5623) #arguing the crs code for Detroit, "5623"
neighborhoods_p <- st_transform(neighborhood_data, crs = 5623)
crime_p <- st_transform(crime_data, crs = 5623)
zipcode_p <- st_transform(zipcode_data, crs = 5623)


#exploring the data by column name using colnames()
colnames(crime_p)
colnames(neighborhoods_p)
colnames(zipcode_p)
colnames(grocery_p)


####################################################################################################
##TASK 1: Mapping Crime Incidents in Detroit’s Neighborhood and Zip Code Areas.
####################################################################################################
#1. Map of Crime Frequencies in Detroit Neighborhoods, 2021

#filtering the projected crime data for incidents in 2021 
crime_data_2021 <- crime_p %>% filter(year == 2021) #piping operator from dplyr passes crimpe_p to the filter() function to return 2021 incidents using a logical operator 
                                                    #filtered data is saved as a new dataframe
#checking if CRS is projected in the new dataframe 
st_is_longlat(crime_data_2021) #TRUE; it is not. 

#projecting the new dataframe to the same CRS as crime_p using st_transform and saving it as a new dataframe object 
crime_p_2021 <- st_transform(crime_data_2021, crs = 5623)

#checking if projected again
st_is_longlat(crime_p_2021) #FALSE; the new dataframe is projected. 



#grouping 2021 crime data by neighborhood name and creating a new crime by neighborhoods variable
  #order of operations:
  #1. st_join() joins the crime data with the neighborhood data 
  #2. grouby() groups the joined data by neighborhood name 
  #3. summarize() calculates the crime count of each neighborhood from the grouped data 
  #4. assignment operator assigns the summarized data to a new dataframe crime_data_agg
crime_data_agg <- summarize(group_by(st_join(crime_p_2021, neighborhoods_p), nhood_name), crime_count = sum(!is.na(crime_id)))

#checking if the new dataframe is projected to the same CRS using st_crs()
st_crs(crime_data_agg) #yes


#joining the grouped 2021 crime data with neighborhood data 
neighborhood_crime_p <- neighborhoods_p %>% #piping operator passes the projected neighborhood data to st_join() it with the new grouped dataframe
  st_join(crime_data_agg)

#preparing to create a color-coded map: assigning breaks, colors and labels to the relevant crime count intervals 
my_breaks <- c(0, 100, 1200, Inf) #defining the ranges of crime counts in a numeric vector; "Inf" includes values above 1200
my_colors <- c("green", "white", "red") #assigning a vector of colors corresponding to the breaks 
my_labels <- c("< 100 (low-crime, green)", "101 - 1200 (moderate crime, white)", "> 1200 (high-crime, red)") #vector of labels for each interval and their corresponding colors 

#creating the map
my_map_1 <- tm_shape(neighborhood_crime_p) + #initializing a tmap object with tm_shape() using neighborhood_crime_p data 
  tm_polygons(col = "crime_count", #adding neighborhood borders (polygons) to the map using tm_polygons() and arguing for the crime_count variable as the fill color (col =...) 
              breaks = my_breaks, #specifying breaks as created previously 
              palette = my_colors, #specifying color palette as created previously 
              labels = my_labels, #specifying labels as created previously 
              title = "Crime Incident Frequency", #setting the title for the legend 
              alpha = 0.8, #setting the transparency level for the polygons 
              border.alpha = 1, #setting border transparency for the polygons 
              border.col = "black", #setting border color for the polygons 
              na.value = "transparent") + #setting transparent fill for polygons w/ missing information 
  tm_text("nhood_name.y", size = 0.5) + #adding neighborhood labels to the map
  tm_layout(frame = FALSE, title = "Map of Detroit Crime Incidents by Neighborhood, 2021", inner.margins = c(0.1, 0.1, 0.1, 0.1)) + #removing a frame, titling the map, controlling inner margins 
  tm_legend(position = c("left", "bottom"), title.size = 0.8, title.fontfamily = "sans") #setting legend position, legend title size and font 
tmap_save(my_map_1, "my_map_1.pdf", width = 11, height = 9) #specifying map dimensions and saving the resulting map as a PDF 


##################################################
#2. Map of Crime Frequencies in Detroit Zipcode Areas, 2021

#grouping 2021 crime data by zipcode and creating a new crime by zipcode dataframe
crime_data_agg_2 <- summarize(group_by(st_join(crime_p_2021, zipcode_p),zipcode),crime_count = sum(!is.na(crime_id)))

#checking if the new dataframe is projected 
st_is_longlat(crime_data_agg_2) #false; it is projected 

#making sure the new dataframe is projected to the same CRS using st_crs()
st_crs(crime_data_agg_2) #yes

#joining the grouped 2021 crime data with zipcode data 
zipcode_crime_p <- zipcode_p %>%
  st_join(crime_data_agg_2)

#preparing to create a color-coded map: assigning breaks, colors and labels to the relevant crime count intervals 
my_breaks_2 <- c(0, 1000, 5000, Inf) #defining the ranges of crime counts in a numeric vector; "Inf" includes values above 5000
my_colors_2 <- c("green", "white", "red") #assigning a vector of colors corresponding to the breaks 
my_labels_2 <- c("< 1000 (low-crime, green)", "1001 - 5000 (moderate crime, white)", "> 5000 (high-crime, red)") #vector of labels for each interval and their corresponding colors 

#converting the zipcode variable to a character for labeling purposes 
zipcode_crime_p$zipcode.x <- as.character(zipcode_crime_p$zipcode.x)


#creating the map
my_map_2 <- tm_shape(zipcode_crime_p) + #initializing a tmap object with tm_shape() using zipcode_crime_p data 
  tm_polygons(col = "crime_count", #adding zipcode borders (polygons) to the map using tm_polygons() and arguing for the crime_count variable as the fill color (col =...) 
              breaks = my_breaks_2, #specifying breaks as created previously 
              palette = my_colors_2, #specifying color palette as created previously 
              labels = my_labels_2, #specifying labels as created previously 
              title = "Crime Incident Frequency", #setting legend title 
              alpha = 0.8, #setting the transparency level for the polygons 
              border.alpha = 1, #setting border transparency for the polygons 
              border.col = "black", #setting border color for the polygons 
              na.value = "transparent") + #setting transparent fill for polygons w/ missing information 
  tm_text("zipcode.x", size = 0.8) + #adding zip code labels to the map
  tm_layout(frame = FALSE, title = "Map of Detroit Crime Incidents by Zipcode, 2021", inner.margins = c(0.1, 0.1, 0.1, 0.1)) + #making sure there is no frame, adding an appropriate title and adjusting the margins 
  tm_legend(position = c("left", "bottom"), title.size = 0.8, title.fontfamily = "sans") #setting legend position, title size and font 
#saving map as PDF
tmap_save(my_map_2, "my_map_2.pdf", width = 11, height = 9) #specifying map dimensions and saving the resulting map as a PDF 

####################################################################################################
##TASK 2: Robbery Incidents around grocery stores.
# Convert the neighborhood and grocery store data to simple features
neighborhood_data_simple <- st_as_sf(neighborhood_data)
grocery_data_simple <- st_as_sf(grocery_data, coords = c("longitude", "latitude"), crs = 4326)
####################################################################################################
#1. Map of Detroit Neighborhoods and Grocery Store locations 

#just keeping the dataframe transformations in sight for reference. commented on this code earlier 
grocery_p <- st_transform(grocery_data, crs = 5623)
neighborhoods_p <- st_transform(neighborhood_data, crs = 5623)
crime_p <- st_transform(crime_data, crs = 5623)
zipcode_p <- st_transform(zipcode_data, crs = 5623)

#creating 1 mile, 2 mile and 3 mile buffers around the grocery store locations 
gro_1m_buff <- st_buffer(grocery_p, dist = 1609) #creating a buffer w/ st_buffer, arguing for projected grocery data and 1609 meter (1 mi) distance buffer 
gro_2m_buff <- st_buffer(grocery_p, dist = 3218) #creating a buffer w/ st_buffer, arguing for projected grocery data and 3218 meter (2 mi) distance buffer 
gro_3m_buff <- st_buffer(grocery_p, dist = 4828) #creating a buffer w/ st_buffer, arguing for projected grocery data and 4828 meter (3 mi) distance buffer 

#checking if the new buffers are projected 
st_is_longlat(gro_1m_buff) #false; it is projected 
st_is_longlat(gro_2m_buff) #false; it is projected 
st_is_longlat(gro_3m_buff) #false; it is projected 

#making sure the new buffers are projected to the same CRS as the grocery data using st_crs()
st_crs(grocery_p)  #5623
st_crs(gro_1m_buff) #yes
st_crs(gro_2m_buff) #yes
st_crs(gro_3m_buff) #yes

#creating the map
my_map_3 <- tm_shape(neighborhoods_p) + #setting the initial shape as neighborhood border polygons w/ projected neighborhoods data
  tm_polygons(alpha = 0.4) + #setting neighborhood boundary transparency level
  tm_text('nhood_name', size = 0.4) + #setting legend label name and size 
  tm_shape(grocery_p) + #setting the shape to grocery store location points w/ projected grocery data
  tm_dots(col = "black", size = 0.1, alpha = 1) + #adding dots to the map for grocery locations- black, small size, fully opaque
  tm_shape(gro_3m_buff) + #setting shape to the 3mi buffer polygons
  tm_borders(col = 'red', lwd = 1) + #setting border color
  tm_fill(col = 'red', alpha = 0.6) + #setting fill color and transparency of buffers
  tm_shape(gro_2m_buff) + #setting shape to the 2mi buffer polygons
  tm_borders(col = 'green', lwd = 1) + #setting border color
  tm_fill(col = 'green', alpha = 0.5) + #setting fill color and transparency of buffers
  tm_shape(gro_1m_buff) + #setting shape to the 1mi buffer polygons
  tm_borders(col = "blue", lwd = 1) + #setting border color
  tm_fill(col = "blue", alpha = 0.4) + #setting fill color and transparency of buffers
  tm_layout(main.title = "Map of Detroit Neighborhoods and Grocery Store Buffer Zones, 2021", #setting main title 
            main.title.position = "center", #positioning main title 
            main.title.size = 1, #sizing main title 
            legend.outside.size = 0.8, #setting legend width
            frame = FALSE) #removing frame 

#save as PDF
tmap_save(my_map_3, filename = "my_map_3.pdf", width = 9, height = 7) #specifying map dimensions and saving the resulting map as a PDF 


####################################################################################################
#2. Finding robbery incidents by radii of grocery stores, 2021

#filtering 2021 crime data for robberies and saving the result as a new dataframe; piping operator passes 2021 crime data to filter() argument  
robbery_data <- crime_p_2021 %>%
  filter(offense_ca == "ROBBERY")

#projecting the robbery data the same CRS as the projected grocery data using st_transform() and saving it as a new dataframe object 
robbery_p <- st_transform(robbery_data, st_crs(grocery_p))

#checking if CRS is projected in the new dataframe 
st_is_longlat(robbery_p) #FALSE; the new dataframe is projected


#creating the buffers the same way as before, just saving as new objects to keep everything separate 
grocery_buffer_1mi <- st_buffer(grocery_p, dist = 1609) 
grocery_buffer_2mi <- st_buffer(grocery_p, dist = 3218) 
grocery_buffer_3mi <- st_buffer(grocery_p, dist = 4828)

robbery_count_1mi <- st_join(grocery_buffer_1mi, robbery_p, join = st_intersects) %>% #using st_join w/ st_intersects to create a new dataset with the info on Detroit grocery stores and robberies occuring w/in 1mi. piping function passes this output to the groupby() function
  group_by(Store_Name, Address) %>% #using groupby() to group the data by grocery store name and address; piping function passes this output to the summarize() function
  summarize(n_robberies_1mi = n()) #calculating the frequency of robberies w/in 1mi of grocery stores and assigns the output to n_robberies_1mi variable

robbery_count_2mi <- st_join(grocery_buffer_2mi, robbery_p, join = st_intersects) %>% #using st_join  w/ st_intersects to create a new dataset with the info on Detroit grocery stores and robberies occuring w/in 2mi. piping function passes this output to the groupby() function
  group_by(Store_Name, Address) %>% #using groupby() to group the data by grocery store name and address; piping function passes this output to the summarize() function
  summarize(n_robberies_2mi = n()) #calculating the frequency of robberies w/in 2mi of grocery stores and assigns the output to n_robberies_2mi variable

robbery_count_3mi <- st_join(grocery_buffer_3mi, robbery_p, join = st_intersects) %>% #using st_join  w/ st_intersects to create a new dataset with the info on Detroit grocery stores and robberies occuring w/in 3mi. piping function passes this output to the groupby() function
  group_by(Store_Name, Address) %>% #using groupby() to group the data by grocery store name and address; piping function passes this output to the summarize() function
  summarize(n_robberies_3mi = n()) #calculating the frequency of robberies w/in 3mi of grocery stores and assigns the output to n_robberies_3mi variable

#joining the three new dataframes into a single data frame
robbery_counts <- robbery_count_1mi %>% #piping the 1mi dataframe to st_join()
  st_join(robbery_count_2mi, by = c("Store_Name", "Address")) %>% #joining with the 2 mi dataframe using store name and address columns as the join keys 
  st_join(robbery_count_3mi, by = c("Store_Name", "Address")) #joining with the 3 mi dataframe using store name and address columns as the join keys 

#dropping unnecessary columns
robbery_counts <- robbery_counts %>% #piping robbert counts dataframe to select()
  select(-Store_Name.y, -Address.y, -Store_Name, -Address, -geometry) #arguing for the columns to be dropped 

#renaming columns 
robbery_counts_renamed <- robbery_counts %>% #piping robbery counts dataframe to rename()
  rename(`Store Name` = Store_Name.x, `Address` = Address.x, #arguing for the new column names 
         `Robberies within 1mi` = n_robberies_1mi,
         `Robberies within 2mi` = n_robberies_2mi,
         `Robberies within 3mi` = n_robberies_3mi)

#saving the final dataframe to an Excel file using write_xlsx()
write_xlsx(robbery_counts_renamed, path = "robbery_counts_renamed.xlsx")

#getting descriptive statistics from the robbery incidents dataframe with summary()
summary(robbery_counts_renamed)