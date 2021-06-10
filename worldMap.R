# Libraries----
library(shiny)
library(tidyverse)
library(scales)
library(reshape2)
library(shinydlplot)
library(magrittr)
library(rvest)
library(maps)

# Data----
filenames <- c("Births.csv", "Deaths_1x1.csv", "E0per.csv", "fltper_1x1.csv", "mltper_1x1.csv", "Population.csv", "Countries.csv")
objectnames <- c("births", "deaths", "E0", "flt", "mlt", "pop", "countries")

for (i in 1:length(filenames)) {
  assign(objectnames[i], read.csv(filenames[i], sep = " ", dec = "."))
}

xpaths<-c('//*[@id="Country-Codes-A-C"]',
          '//*[@id="Country-Codes-D-H"]',
          '//*[@id="CountryCodes-I-L"]',
          '//*[@id="Country-Codes-M-P"]',
          '//*[@id="Country-Codes-Q-T"]',
          '//*[@id="Country-Codes-U-Z"]')

url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"

isoCodes<-tibble(Country=character(),ISO2=character(),ISO3=character(),UN=character())
for(xpath in xpaths){
  iso_codes <- url %>%
    read_html() %>%
    html_nodes(xpath = xpath) %>%
    html_table()
  iso_codes <- iso_codes[[1]][, -1]
  iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
  isoCodes<<-rbind(isoCodes,iso_codes)
}
colnames(isoCodes)<-c("Country","ISO2","ISO3","UN")

#write.table(isoCodes,"iso_codes.csv",sep=";")

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)

old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast",
               "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands",
               "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna")
new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation",
               rep("South Georgia and the South Sandwich Islands", 2), 
               "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands")

for (i in 1:length(old_names)){
  world_data$region[world_data$region == old_names[i]] <- new_names[i]
}

world_data<-world_data%>%
  full_join(isoCodes,by=c("region"="Country"))


popGenderRatio<-pop %>%
  group_by(Year,c_code)%>%
  summarise(pTot=sum(Total),genderRatio=sum(Male)/sum(Total))

birthsPermill<-births%>%
  full_join(.,summarize(group_by(pop,Year,c_code),sum(Total),sum(Male),sum(Female)))%>%
  transmute(Year,c_code,bPermillTotal=(1e6*Total)/`sum(Total)`,bPermillMale=(1e6*Male)/`sum(Male)`,bPermillFemale=(1e6*Female)/`sum(Female)`)

colnames(E0)<-c("Year","exFem","ExMal","exTot","c_code")

avAge<-pop%>%
  full_join(popGenderRatio,by=c("Year","c_code"))%>%
  transmute(Year,Age,Total,wTot=Total/pTot,c_code)%>%
  group_by(Year,c_code)%>%
  summarize(avAge=sum(Age * wTot)/sum(wTot))

dataTrans<-deaths%>%
  full_join(.,pop,by=c("Year","Age","c_code"))%>%
  group_by(Year,c_code)%>%
  summarise(dTot=sum(Total.x),pTot=sum(Total.y),dMale=sum(Male.x),pMale=sum(Male.y),dFemale=sum(Female.x),pFemale=sum(Female.y))%>%
  transmute(Year,c_code,dPermillTotal=(1e6*dTot)/pTot,dPermillMale=(1e6*dMale)/pMale,dPermillFemale=(1e6*dFemale)/pFemale)%>%
  full_join(.,popGenderRatio,by=c("Year","c_code"))%>%
  full_join(.,E0,by=c("Year","c_code"))%>%
  full_join(.,birthsPermill,by=c("Year","c_code"))%>%
  full_join(.,avAge,by=c("Year","c_code"))%>%
  mutate(c_code=str_sub(c_code,1,3))%>%
  drop_na(pTot)

dataTrans<-dataTrans%>%
  gather("dPermillTotal","dPermillMale","dPermillFemale","pTot","genderRatio","exFem","ExMal","exTot","bPermillTotal","bPermillMale"  ,"bPermillFemale","avAge",key = "dataType",value="data")


data_type="pTot"
year=2000

worldMaps <- function(df, world_data, data_type, year){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 20),
                       axis.title = element_text(size = 20),
                       strip.text = element_text(size = 20),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$dataType == data_type & df$Year == year,]
  plotdf <- plotdf[!is.na(plotdf$c_code), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['DataType'] <- rep(data_type, nrow(world_data))
  world_data['Year'] <- rep(year, nrow(world_data))
  world_data['Value'] <- plotdf$data[match(world_data$ISO3, plotdf$c_code)]
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(data_type == "Childlessness", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s",ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = data_type, color = data_type, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}

#worldMaps(dataTrans,world_data,data_type,year)

#write.table(dataTrans,"mapData.csv",sep=" ",dec=".")
#write.table(world_data,"worldData.csv",sep=" ",dec=".")
