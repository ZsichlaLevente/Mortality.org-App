library(RCurl)
library(tidyverse)

setwd("/Users/ASUS/Documents/MyDocs/Courses/Inf/2felev/Infalap2/mortorg_start")

countries <- arrange(
  tibble(
    country = c("Australia", "Finland", "Latvia", "Slovenia", "Austria", "France", "Lithuania", "Spain", "Belarus", "Germany", "Luxembourg", "Sweden", "Belgium", "Greece", "Netherlands", "Switzerland", "Bulgaria", "Hong Kong", "New Zealand", "Taiwan", "Canada", "Hungary", "Norway", "U.K.", "Chile", "Iceland", "Poland", "U.S.A.", "Croatia", "Ireland", "Portugal", "Ukraine", "Czechia", "Israel", "Republic of Korea", "Denmark", "Italy", "Russia", "Estonia", "Japan", "Slovakia"),
    code = c("AUS", "FIN", "LVA", "SVN", "AUT", "FRATNP", "LTU", "ESP", "BLR", "DEUTNP", "LUX", "SWE", "BEL", "GRC", "NLD", "CHE", "BGR", "HKG", "NZL_NP", "TWN", "CAN", "HUN", "NOR", "GBR_NP", "CHL", "ISL", "POL", "USA", "HRV", "IRL", "PRT", "UKR", "CZE", "ISR", "KOR", "DNK", "ITA", "RUS", "EST", "JPN", "SVK")
  ),
  country
)
write.table(file = "Countries.csv", countries)

userpwd <- "levente.zsichla@gmail.com:1621425763"
filenames <- c("mltper_1x1", "fltper_1x1", "Population", "Births", "Deaths_1x1", "E0per")

for (filename in filenames) {
  
  if (!exists(paste(filename, "_db", sep = ""))) {
    url <- getURL(paste("https://www.mortality.org/hmd/", countries$code[1], "/STATS/", filename, ".txt", sep = ""), userpwd = userpwd, httpauth = 1L)
    assign(paste(filename, "_db", sep = ""),
           mutate(
             mutate_all(
               tibble(read.table(textConnection(url),sep = "", dec = ".", header = T, fill = T, skip = 2)),
               function(x) ifelse(is.na(as.double(x)),as.double(str_sub(x,1,(str_length(x)-1))),as.double(x))
             ),
             c_code = countries$code[1])
    )
  }
  
  for (country in countries$code[2:length(countries$code)]) {
    url <- getURL(paste("https://www.mortality.org/hmd/", country, "/STATS/", filename, ".txt", sep = ""), userpwd = userpwd, httpauth = 1L)
    temp <- eval(parse(text = paste(filename, "_db", sep = "")))
    
    assign(paste(filename, "_db", sep = ""), 
           rbind(
             temp, 
             mutate(
               mutate_all(
                 tibble(read.table(textConnection(url),sep = "", dec = ".", header = T, fill = T, skip = 2)),
                 function(x) ifelse(is.na(as.double(x)),as.double(str_sub(x,1,(str_length(x)-1))),as.double(x))
                 ),
               c_code = country
               )
             )
           )
    
    if (country == countries$code[length(countries$code)]) {
      write.table(file = paste(filename, ".csv", sep = ""), eval(parse(text = paste(filename, "_db", sep = ""))))
    }
  }
}