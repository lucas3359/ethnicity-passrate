library("data.table")
library("stringr")
library("tidyverse")
library("dplyr")
library("forcats")
library("stringi")
library("stringdist")

stuDF <- read.csv("J:/Lucas/Ethnicity-Cluster/add school-gender/Comments/postcode/Data/9OCT.csv")%>%select(-ROAD)
dicDF <- read.csv("C:/Users/shua440/Documents/nz_street_address.csv")

###### get formal road name

#close match algorithm to correct spelling& abbreviation mistakes

ClosestMatch <- function(string, stringVector){
  
  stringVector[amatch(string, stringVector, method = "osa",maxDist=3)]
  
}


#get address data from one of the address lines and correct mistakes
get_road<-function(road_line){
  return( substr(road_line,regexpr(" ",road_line)+1,str_length(road_line)))
}



#get road name from the first address line (remove the first part before blank)
stuDF$road_name<-get_road(stuDF$CANDIDATE_ADDRESS_LINE_1)

#get distinct road name from dictionary file
dic_roadname <- dicDF%>%distinct(full_road_name)%>%mutate(full_road_name)

#match
stuDF$match_road<-ClosestMatch(stuDF$road_name,dic_roadname$full_road_name)

#those not matched, remove the second part before blank, and do the same thing as the first address line
stuDF2<-stuDF%>%filter(is.na(match_road))
stuDF<-stuDF%>%select(-road_name)

stuDF2$road_name<-get_road(stuDF2$road_name)
stuDF2$match_road<-ClosestMatch(stuDF2$road_name,dic_roadname$full_road_name)

#merge matched ones to stuDF
stuDF<-merge(x=stuDF,y=stuDF2[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$match_road.x), as.character(stuDF$match_road.y), as.character(stuDF$match_road.x))

#not matched
stuDF3<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road.x,-match_road.y,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road.x,-match_road.y)

#
stuDF3$road_name<-get_road(get_road(get_road(stuDF3$CANDIDATE_ADDRESS_LINE_1)))
stuDF3$match_road<-dic_roadname[match(stuDF3$road_name,dic_roadname$full_road_name),]
stuDF<-merge(x=stuDF,y=stuDF3[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))

stuDF4<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)

stuDF4$road_name<-get_road(get_road(get_road(get_road(stuDF4$CANDIDATE_ADDRESS_LINE_1))))
stuDF4$match_road<-dic_roadname[match(stuDF4$road_name,dic_roadname$full_road_name),]

stuDF<-merge(x=stuDF,y=stuDF4[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))

stuDF5<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)


stuDF5$road_name<- get_road(stuDF5$CANDIDATE_ADDRESS_LINE_1)%>% sub('[ ][^ ]+$', '', .)
stuDF5$match_road<-dic_roadname[match(stuDF5$road_name,dic_roadname$full_road_name),]
stuDF<-merge(x=stuDF,y=stuDF5[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))

stuDF6<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)

stuDF6$road_name<- get_road(stuDF6$CANDIDATE_ADDRESS_LINE_1)%>% sub('[ ][^ ]+$', '', .)%>% sub('[ ][^ ]+$', '', .)
stuDF6$match_road<-dic_roadname[match(stuDF6$road_name,dic_roadname$full_road_name),]
stuDF<-merge(x=stuDF,y=stuDF6[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF7<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)

stuDF7$road_name<- error_check(stuDF7$CANDIDATE_ADDRESS_LINE_1)
stuDF7$match_road<-dic_roadname[match(stuDF7$road_name,dic_roadname$full_road_name),]
stuDF<-merge(x=stuDF,y=stuDF7[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF8<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)




#
stuDF8$road_name<-get_road(stuDF8$CANDIDATE_ADDRESS_LINE_2)
stuDF8$match_road<-dic_roadname[match(stuDF8$road_name,dic_roadname$full_road_name),]

stuDF<-merge(x=stuDF,y=stuDF8[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF9<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)


stuDF9$road_name<-error_check(stuDF9$CANDIDATE_ADDRESS_LINE_2)
stuDF9$match_road<-dic_roadname[match(stuDF9$road_name,dic_roadname$full_road_name),]
stuDF<-merge(x=stuDF,y=stuDF9[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF10<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)

stuDF10$road_name<-get_road(get_road(stuDF10$CANDIDATE_ADDRESS_LINE_2))
stuDF10$match_road<-dic_roadname[match(stuDF10$road_name,dic_roadname$full_road_name),]

stuDF<-merge(x=stuDF,y=stuDF10[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF11<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)


stuDF11$road_name<-get_road(get_road(get_road(stuDF11$CANDIDATE_ADDRESS_LINE_2)))
stuDF11$match_road<-dic_roadname[match(stuDF11$road_name,dic_roadname$full_road_name),]

stuDF<-merge(x=stuDF,y=stuDF11[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF12<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)

#remove numbers and upper case
stuDF12$road_name<-error_check(stuDF12$CANDIDATE_ADDRESS_LINE_1)%>% gsub('[0-9]+', '',.)
stuDF12$road_name<-paste0(toupper(substr(stuDF12$road_name, 1, 1)), substr(stuDF12$road_name, 2, nchar(stuDF12$road_name)))
stuDF12$match_road<-dic_roadname[match(stuDF12$road_name,dic_roadname$full_road_name),]

stuDF<-merge(x=stuDF,y=stuDF12[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_road")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$ROAD_NAME <- ifelse(is.na(stuDF$ROAD_NAME), as.character(stuDF$match_road), as.character(stuDF$ROAD_NAME))
stuDF13<-stuDF%>%filter(is.na(ROAD_NAME))%>%select(-match_road,-ROAD_NAME)
stuDF<-stuDF%>%select(-match_road)


stuDF$ROAD_NAME<- stuDF$ROAD_NAME%>% replace_na("Unknown")

x<-stuDF%>%filter(ROAD_NAME=="Unknown")

#######################################subrub
dic_suburbname <- dicDF%>%distinct(suburb_locality)

get_suburb <- function(suburb_line){
  return( 
    suburb_line%>% 
      gsub("Mt ", "Mount ", .)%>%
      gsub("St ", "Saint ", .)%>%
      gsub("Pt", "Point", .)%>%
      gsub(' [0-9]+', '',.)%>%
      gsub('Beachhaven', 'Beach Haven',.)%>%
      gsub('Saint Heliers', 'St Heliers',.)%>%
      gsub('Ruatangata', 'Ruatangata West',.)
  )
}

stuDF$suburb_name<-get_suburb(stuDF$CANDIDATE_ADDRESS_LINE_2)

stuDF$SUBURB_NAME<-dic_suburbname[match(stuDF$suburb_name,dic_suburbname$suburb_locality),]
suburb_N1<-stuDF%>%filter(is.na(SUBURB_NAME))%>%select(-SUBURB_NAME,-suburb_name)
stuDF<-stuDF%>%select(-suburb_name)



###############

suburb_N1$suburb_name<-get_suburb(suburb_N1$CANDIDATE_ADDRESS_LINE_3)
suburb_N1$match_suburb<-dic_suburbname[match(suburb_N1$suburb_name,dic_suburbname$suburb_locality),]
stuDF<-merge(x=stuDF,y=suburb_N1[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_suburb")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
# stuDF<-stuDF%>%mutate(SUBURB_NAME = paste(match_suburb.x,match_suburb.y,sep=""))%>% select(-match_suburb.x,-match_suburb.y)
# stuDF$SUBURB_NAME<- stuDF$SUBURB_NAME%>%gsub("NA","", .)
# suburb_N2<-stuDF%>%filter(SUBURB_NAME=='')
# 
# 

stuDF$SUBURB_NAME <- ifelse(is.na(stuDF$SUBURB_NAME), as.character(stuDF$match_suburb), as.character(stuDF$SUBURB_NAME))
suburb_N2<-stuDF%>%filter(is.na(SUBURB_NAME))
stuDF<-stuDF%>%select(-match_suburb)


########replace wrong suburb by single street name

dic_road_suburb <- dicDF%>%select(full_road_name, suburb_locality,town_city)%>%distinct%>%
  group_by(full_road_name)%>%filter(n()==1)%>%ungroup()

#dic_road_suburb$suburb_locality<- as.character(dic_road_suburb$suburb_locality)%>%gsub(NA, "", .)

stuDF[,c("suburb_locality2","town_city2")] <-
  dic_road_suburb[match(stuDF$ROAD_NAME, dic_road_suburb$full_road_name),c("suburb_locality","town_city")]
stuDF$SUBURB_NAME <- ifelse(is.na(stuDF$suburb_locality2), stuDF$SUBURB_NAME, as.character(stuDF$suburb_locality2))
stuDF<-stuDF%>%select(-suburb_locality2)
suburb_N3<-stuDF%>%filter(is.na(SUBURB_NAME))

######################city

###need to check
get_city <- function(city_line){
  return(
    city_line%>%gsub('  [0-9]+', '', .)%>%
      gsub(' [0-9]+', '', .)%>%
      gsub(' City', '', .)%>%
      gsub('Manukau', 'Auckland', .)%>%
      gsub('Central Auckland', 'Auckland', .)%>%
      gsub('West Auckland', 'Auckland', .)%>%
      gsub('South Auckland', 'Auckland', .)%>%
      gsub('North Shore', 'Auckland', .)%>%
      # gsub('Henderson', 'Auckland', .)%>%
      # gsub('Manurewa', 'Auckland', .)%>%
      # gsub('Waitakere', 'Auckland', .)%>%
      
      gsub('Rarotonga', 'Cook Islands', .)
    
    
    
  )
  
}


dic_cityname <- dicDF%>%distinct(town_city)


stuDF$CITY_NAME<-get_city(stuDF$CANDIDATE_ADDRESS_LINE_2)

stuDF$match_city<-dic_cityname[match(stuDF$CITY_NAME,dic_cityname$town_city),]
stuDF$CITY_NAME<- ifelse(is.na(stuDF$town_city2), as.character(stuDF$match_city), as.character(stuDF$town_city2))
city_N1<-stuDF%>%filter(is.na(CITY_NAME))%>%select(-town_city2,-match_city,-CITY_NAME)

stuDF<-stuDF%>%select(-match_city,-town_city2)



city_N1$city_name<-get_city(city_N1$CANDIDATE_ADDRESS_LINE_3)
city_N1$match_city<-dic_cityname[match(city_N1$city_name,dic_cityname$town_city),]
stuDF<-merge(x=stuDF,y=city_N1[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_city")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )

stuDF$CITY_NAME<- ifelse(is.na(stuDF$CITY_NAME), as.character(stuDF$match_city), as.character(stuDF$CITY_NAME))
city_N2<-stuDF%>%filter(is.na(CITY_NAME))%>%select(-match_city)
stuDF<-stuDF%>%select(-match_city)



city_N2$CITY_NAME<-get_city(city_N2$CANDIDATE_ADDRESS_LINE_4)

city_N2$match_city<-dic_cityname[match(city_N2$CITY_NAME,dic_cityname$town_city),]

stuDF<-merge(x=stuDF,y=city_N2[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","match_city")], by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF$CITY_NAME<- ifelse(is.na(stuDF$CITY_NAME), as.character(stuDF$match_city), as.character(stuDF$CITY_NAME))
city_N3<-stuDF%>%filter(is.na(CITY_NAME))%>%select(-match_city,-CITY_NAME)

stuDF<-stuDF%>%select(-match_city)


#  assign city based on suburb-city

dic_suburb_city <- dicDF%>%select(suburb_locality,town_city)%>%distinct%>%
  group_by(suburb_locality)%>%filter(n()==1)%>%ungroup()


stuDF[,c("town_city2")] <-
  dic_suburb_city[match(stuDF$SUBURB_NAME, dic_suburb_city$suburb_locality),c("town_city")]



stuDF$CITY_NAME <- ifelse(is.na(stuDF$town_city2), stuDF$CITY_NAME, as.character(stuDF$town_city2))
stuDF<-stuDF%>%select(-town_city2)
city_N4<-stuDF%>%filter(is.na(SUBURB_NAME))



# stuDF_Final<- merge(x=stuDF,y=dplyr::distinct(dicDF[ , c("full_road_name","suburb_locality","shape_X")],.keep_all = T), 
#                     by.x=c("ROAD_NAME2","SUBURB_NAME"),
#                     by.y = c("full_road_name","suburb_locality"),
#                     all.x = TRUE )
# 



#######replace the wrong suburb by both single road name&city name

dic_road_city <- dicDF%>%select(full_road_name, suburb_locality,town_city)%>%distinct%>%
  group_by(full_road_name,town_city)%>%filter(n()==1)%>%ungroup()


stuDF[,c("suburb_locality3")] <-
  dic_road_city[match(paste(stuDF$ROAD_NAME,stuDF$CITY_NAME), paste(dic_road_city$full_road_name,dic_road_city$town_city)),c("suburb_locality")]
stuDF$SUBURB_NAME <- ifelse(is.na(stuDF$suburb_locality3), stuDF$SUBURB_NAME, as.character(stuDF$suburb_locality3))
stuDF<-stuDF%>%select(-suburb_locality3)

suburb_N4<-stuDF%>%filter(is.na(SUBURB_NAME))


#stuDF$SUBURB_NAME2[,c("stuDF$ROAD_NAME2","stuDF$SUBURB_NAME") %in% c("dic_road_suburb$full_road_name","dic_road_suburb$suburb_locality")] <- as.character(dic_road_suburb$suburb_locality)

#############

#attach a random suburb based on road name and city
stuDF[,c("suburb_locality")] <-
  dicDF[match(paste(stuDF$ROAD_NAME,stuDF$CITY_NAME), paste(dicDF$full_road_name,dicDF$town_city)),c("suburb_locality")]
stuDF$SUBURB_NAME <- ifelse(is.na(stuDF$SUBURB_NAME), as.character(stuDF$suburb_locality), as.character(stuDF$SUBURB_NAME))



# stuDFNull<- merge(x=stuDF,y=dicDF[ , c("full_road_name","suburb_locality","town_city","shape_X")], 
#                   by.x=c("ROAD_NAME","SUBURB_NAME","CITY_NAME"),
#                   by.y=c("full_road_name","suburb_locality","town_city"),
#                   all.x = TRUE )%>%filter()







y<-distinct(dicDF[ , c("full_road_name","suburb_locality","town_city","shape_X","shape_Y")])
stuDF_Final<- cbind(stuDF, longitude=y[match(paste(stuDF$ROAD_NAME,stuDF$SUBURB_NAME),paste(y$full_road_name,y$suburb_locality)),"shape_X"],
                    latitude = y[match(paste(stuDF$ROAD_NAME,stuDF$SUBURB_NAME),paste(y$full_road_name,y$suburb_locality)),"shape_Y"],
                    town_city =  y[match(paste(stuDF$ROAD_NAME,stuDF$SUBURB_NAME),paste(y$full_road_name,y$suburb_locality)),"town_city"])


stuDF_Final_NULL1<-stuDF_Final%>%filter(is.na(latitude))%>%select(-longitude, -latitude, -town_city)



#### Get random longitude and latitude by suburb and city


stuDF_Final_NULL1<- stuDF_Final_NULL1%>%cbind(stuDF_Final_NULL1, longitude=y[match(paste(stuDF_Final_NULL1$SUBURB_NAME,stuDF_Final_NULL1$CITY_NAME),paste(y$suburb_locality,y$town_city)),"shape_X"],
                                              latitude = y[match(paste(stuDF_Final_NULL1$SUBURB_NAME,stuDF_Final_NULL1$CITY_NAME),paste(y$suburb_locality,y$town_city)),"shape_Y"],
                                              town_city =  y[match(paste(stuDF_Final_NULL1$SUBURB_NAME,stuDF_Final_NULL1$CITY_NAME),paste(y$suburb_locality,y$town_city)),"town_city"])

stuDF_Final<-merge(x=stuDF_Final,y=stuDF_Final_NULL1[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","longitude","latitude","town_city")],
                   by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF_Final$longitude <- ifelse(is.na(stuDF_Final$longitude.x), as.character(stuDF_Final$longitude.y), as.character(stuDF_Final$longitude.x))
stuDF_Final$latitude <- ifelse(is.na(stuDF_Final$latitude.x), as.character(stuDF_Final$latitude.y), as.character(stuDF_Final$latitude.x))
stuDF_Final$town_city <- ifelse(is.na(stuDF_Final$town_city.x), as.character(stuDF_Final$town_city.y), as.character(stuDF_Final$town_city.x))
stuDF_Final<- stuDF_Final%>%select(-longitude.x,-longitude.y,-latitude.x,-latitude.y,-town_city.x,-town_city.y)



stuDF_Final_NULL2<-stuDF_Final%>%filter(is.na(latitude))%>%select(-longitude, -latitude, -town_city)


##### get random longitude and latitude by city 

stuDF_Final_NULL2<- stuDF_Final_NULL2%>%cbind(stuDF_Final_NULL2, longitude=y[match(stuDF_Final_NULL2$CITY_NAME,y$town_city),"shape_X"],
                                              latitude = y[match(stuDF_Final_NULL2$CITY_NAME,y$town_city),"shape_Y"],
                                              town_city =  y[match(stuDF_Final_NULL2$CITY_NAME,y$town_city),"town_city"])

stuDF_Final<-merge(x=stuDF_Final,y=stuDF_Final_NULL2[ , c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4","longitude","latitude","town_city")],
                   by=c("NSN_STUDENT_NUMBER","ETHNIC_GROUP_DESCR","CANDIDATE_ADDRESS_LINE_1","CANDIDATE_ADDRESS_LINE_2","CANDIDATE_ADDRESS_LINE_3","CANDIDATE_ADDRESS_LINE_4"), all.x = TRUE )
stuDF_Final$longitude <- ifelse(is.na(stuDF_Final$longitude.x), as.character(stuDF_Final$longitude.y), as.character(stuDF_Final$longitude.x))
stuDF_Final$latitude <- ifelse(is.na(stuDF_Final$latitude.x), as.character(stuDF_Final$latitude.y), as.character(stuDF_Final$latitude.x))
stuDF_Final$town_city <- ifelse(is.na(stuDF_Final$town_city.x), as.character(stuDF_Final$town_city.y), as.character(stuDF_Final$town_city.x))
stuDF_Final<- stuDF_Final%>%select(-longitude.x,-longitude.y,-latitude.x,-latitude.y,-town_city.x,-town_city.y)


stuDF_Final_NULL3<-stuDF_Final%>%filter(is.na(latitude))%>%select(-longitude, -latitude, -town_city)



#####suburb + city



########only suburb










write.csv(stuDF_Final, file = "stuDF_Final.csv")


# # get suburb based on city
# y<-distinct(dicDF[ , c("full_road_name","suburb_locality","town_city","shape_X","shape_Y")])
# x<- cbind(stuDF_Final_NULL1, longitude=y[match(paste(stuDF$ROAD_NAME2,stuDF$SUBURB_NAME),paste(y$full_road_name,y$town_city)),"shape_X"],
#                     latitude = y[match(paste(stuDF$ROAD_NAME2,stuDF$SUBURB_NAME),paste(y$full_road_name,y$town_city)),"shape_Y"],
#                     suburb = y[match(paste(stuDF$ROAD_NAME2,stuDF$SUBURB_NAME),paste(y$full_road_name,y$town_city)),"suburb_locality"],
#                     town_city =  y[match(paste(stuDF$ROAD_NAME2,stuDF$SUBURB_NAME),paste(y$full_road_name,y$town_city)),"town_city"])
# 





#stuDF_Final_NULL2<-stuDF_Final_NULL1%>%filter(is.na(suburb_locality2))




#############





#dic_road_suburb2<- dic_road_suburb%>%group_by(full_road_name)%>%summarize(count = n())
# 
# tmp <- dic_road_suburb[!is.na(dic_road_suburb$suburb_locality),]
# tmp <- tmp[order(dic_road_suburb$suburb_locality),]
# 
# stuDF$SUBURB_NAME<- tmp[match(stuDF$ROAD_NAME2, tmp$full_road_name),2]
# 

######################test
# x<-city_N2%>%filter(city_name=='Manukau')
x<-dicDF%>%filter(full_road_name=='James Street')



x<-dicDF%>%filter(town_city=='Whatuwhiwhi')
x<-dicDF%>%filter(suburb_locality=='Mangere')
x<-dicDF%>%filter(full_road_name=='New Windsor Road')


# y <- dic_roadname%>%filter(full_road_name=='Archibald Road')
z <- stuDF%>%filter(ROAD_NAME2=='Flatbush ')

