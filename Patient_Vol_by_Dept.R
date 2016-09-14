pat <- read.csv("P:/MOC/_@_MOC_TECH_BACKUP/SERVICE UTILIZATION/MED_Convert_Pat_Vol/Output/GYN_PAT_VOL_201609021357.csv")
summary(pat)
#install.packages('sqldf')
library(sqldf)
data<-sqldf("select * from pat where MEMB_YN = 'Y'")
summary(data)
#attach(data)
str(data)
data$PAT_ENC_CSN_ID <- as.character(data$PAT_ENC_CSN_ID)
data$contact_month<-as.factor(data$contact_month)
data$visit_fac_zip<-as.factor(data$visit_fac_zip)
data$PCP_fac_zip<-as.factor(data$PCP_fac_zip)
data$PCP_prov_id<-as.factor(data$PCP_prov_id)
data$PAT_ID<-as.factor(data$PAT_ID)

# Map the KP pat zip and get lat/long
#install.packages('zipcode')
library(zipcode)
data(zipcode)



#merging zipcode with my data


data<-sqldf('select 
                  data.*
                 ,visit_zip.latitude as visit_lat
                 ,visit_zip.longitude as visit_long
                 ,visit_zip.zip as visit_zipcode
                 ,visit_zip.city as visit_city
                 ,visit_zip.state as visit_state

		     ,pcp_zip.latitude as pcp_lat
                 ,pcp_zip.longitude as pcp_long
		     ,home_zip.latitude as home_lat
                 ,home_zip.longitude as home_long


                 from data left join zipcode visit_zip on data.visit_fac_zip = visit_zip.zip
                           left join zipcode pcp_zip on data.pcp_fac_zip = pcp_zip.zip
                           left join zipcode home_zip on data.pat_home_Zip = home_zip.zip')



#Visualization: 
#install.packages('ggplot2')
#library(ggplot2)
#library(rgdal)

#summary(data)
# Patient Distribution Data
#all_map<- ggplot() +  geom_point(data=data, aes(x=home_long, y=home_lat),color="red")
#all_map

#counties<-readOGR()
#install.packages("maptools")
#library(maptools)
library(RColorBrewer)
library(classInt)
#bay<- readShapePoly("P:/MOC/_@_MOC_TECH_BACKUP/SERVICE UTILIZATION/MED_Convert_Pat_Vol/inFile/bayarea_zipcodes.shp") 
#getting a shape file
#usa.states <- readOGR(dsn = "P:/MOC/_@_MOC_TECH_BACKUP/SERVICE UTILIZATION/MED_Convert_Pat_Vol/Data", layer = "states")
#ca<- usa.states[usa.states$STATE_NAME == "California", ]
#plot(ca)


######other way 
#install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
#install.packages(c("maps", "mapdata"))
#devtools::install_github("dkahle/ggmap")
library(ggplot2)
#library(ggmap)
library(maps)
library(mapdata)
states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

ca_base <-ca_base + geom_polygon(data = ca_county, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA)  # get the state border back on top
ca_base <-ca_base + coord_fixed(xlim = c(-123, -120.0), ylim = c(36, 39), ratio = 1.3) # update ca_base to bay area only
#kp_pat<-ca_base + geom_point(data=data, aes(x=home_long, y=home_lat, group =group_factor1, color=group_factor1) )
#kp_pat

################# MAP  ###########
#Removing the "others" (not the field of interest) from group_factor1

data$pat_home_city<-factor(data$pat_home_city, levels=c("others","FRE","NEW","UNC","SLN","HAY" ))
data$group_factor1<-factor(data$group_factor1, levels=c("FREtoFRE", "FREtoSLN", "FREtoUNC", "SLNtoFRE","SLNtoSLN","SLNtoUNC",
"UNCtoUNC","UNCtoFRE","UNCtoSLN","others"))

no_others<-sqldf("select * from data where group_factor1 != 'others'")
summary(no_others)
#attach(no_others)
#install.packages("ggthemes")
library(ggthemes)
style <- scale_color_brewer(palette = "Set1", name="Empaneled to Visit")

map_no_others <- ca_base + geom_point(data=no_others, aes(x=home_long, y=home_lat,group=group_factor1, color=group_factor1)) + facet_wrap(~group_factor1) + style
map_no_others <- map_no_others + ggtitle("Map1: KP Members Office + Telephone Encounters in the Dept of GYN at GSA from Jan-Aug 2016") + labs(x=" Pat Home City Longitude",y="Pat Home City Latitude")
map_no_others 


#get_telephone
no_others_tele<- sqldf("select * from no_others where enc_type = 'Telephone visit/appt'")
map_no_others_tele<-ca_base + geom_point(data=no_others_tele, aes(x=home_long, y=home_lat, group =group_factor1, color=group_factor1)) + facet_wrap(~group_factor1) 
map_no_others_tele <- map_no_others_tele + ggtitle("Map2: KP Members Telephone Encounters in the Dept of GYN at GSA from Jan-Aug 2016") + labs(x=" Pat Home City Longitude",y="Pat Home City Latitude")
map_no_others_tele

#get_office
no_others_office<-sqldf("select * from no_others where enc_type = 'Office Visit'")
map_no_others_office<-ca_base + geom_point(data=no_others_office, aes(x=home_long, y=home_lat, group =group_factor1, color=group_factor1)) + facet_wrap(~group_factor1) + style

map_no_others_office <- map_no_others_office + ggtitle("Map3: KP Members Office Encounters in the Dept of GYN at GSA from Jan-Aug 2016") + labs(x=" Pat Home City Longitude",y="Pat Home City Latitude")
map_no_others_office



################# BAR GRAPHS  ###########
#ALL_Visit 

bar_style = scale_fill_brewer(palette = "Set1") # matching with the color of the maps
all_visits<-ggplot(data=data, aes(x=pat_home_city, fill=group_factor1)) + geom_bar()+ coord_flip() +  bar_style + labs(title = "Figure1:Comparison of KP Members' GYN Empanelment and Visit Facility based on Their Home City", 
                  x = "Patient Home City", y = "Frequency", fill = "Empaneled to Visit")


all_visits

#split data into office Visit vs Telephone encounter

#office visit

office<-sqldf("select * from data where enc_type = 'Office Visit'")
office$pat_home_city<-factor(office$pat_home_city, levels=c("others","FRE","NEW","UNC","SLN","HAY" ))
office$group_factor1<-factor(office$group_factor1, levels=c("FREtoFRE", "FREtoSLN", "FREtoUNC", "SLNtoFRE","SLNtoSLN","SLNtoUNC",
"UNCtoUNC","UNCtoFRE","UNCtoSLN","others"))

office_visits<-ggplot(data=office, aes(x=pat_home_city, fill=group_factor1)) + geom_bar()+ coord_flip()+ bar_style + labs(title = "Figure2:Comparison of KP Memebers' GYN Empanelment and Office Visit Facility based on Their Home City", 
                  x = "Patient Home City", y = "Frequency", fill = "Empaneled to Visit")

office_visits

#Telephone encounter
tele<-sqldf("select * from data where enc_type = 'Telephone visit/appt'")
tele$pat_home_city<-factor(tele$pat_home_city, levels=c("others","FRE","NEW","UNC","SLN","HAY" ))
tele$group_factor1<-factor(tele$group_factor1, levels=c("FREtoFRE", "FREtoSLN", "FREtoUNC", "SLNtoFRE","SLNtoSLN","SLNtoUNC",
"UNCtoUNC","UNCtoFRE","UNCtoSLN","others"))

tele_visits<-ggplot(data=tele, aes(x=pat_home_city, fill=group_factor1)) + geom_bar()+ coord_flip()+ bar_style + labs(title = "Figure3:Comparison of KP Members' GYN Empanelment and Telephone Visit Facility based on Their Home City", 
                  x = "Patient Home City", y = "Frequency", fill = "Empaneled to Visit")

tele_visits



###################################################################################################
# For Color-blind: will implement it in the future
# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

#####################################################################################################

