## CDS-301-K01 (Fall 2024)
## Group 5_Final Project
## Woohyun Song & Juheon Kim
## South Korea Malaria Trend Analysis (2001-2023)
## 12/08/24



## Load libraries
library(sf)
library(ggplot2)
library(gganimate)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)
library(tidyverse)
library(echarts4r)
library(geojsonsf)



###### 1. Malaria Incidence Rates of Countries Around Korea
### Line Graph

data <- read.csv("Countries Around Korea.csv")

around_korea <- ggplot(data, aes(x = Year, y = Malaria.Cases, group = Name)) +
  geom_line(data = data %>% filter(Name != "South Korea"), 
            aes(color = Name), 
            linetype = "dotted", size = 1.5, alpha = 0.6) + 
  geom_line(data = data %>% filter(Name == "South Korea"), 
            aes(color = Name), 
            linetype = "dashed", size = 2, alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Cambodia" = "purple",
      "Laos" = "blue",
      "Philippines" = "darkgreen",
      "South Korea" = "black",
      "Thailand" = "red",
      "Vietnam" = "darkcyan"
    )
  ) +
  labs(
    title = "Incidence Rates of Countries Around Korea",
    x = "Year",
    y = "Cases",
    color = "Country",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

around_korea








###### 2. Number of malaria infections by city, county and district, 2001-2023 
### Gapminder Annimation
# We successed to make an annimation using gapminder. 
# The problem was the 'e_map_register()' relies on the "name" property in the json file. 
# However, there was no column named "name" in our json file. So we replace SIG_CD to "name" in our json file.
# Through this process, 'merged_data' and 'geojson_sigungu2' were successfully combined.
# Also, the average change in the number of infected people per year group could be correctly displayed on the map. 

# We loaded .json file to .sf object because we have to merge with malaria data 

geojson_sigungu <- st_read("sig2.json") 
st_crs(geojson_sigungu)

malaria_data <- read.csv("Number of malaria infections by city, county and district, 2001-2023.csv", 
                         fileEncoding = "CP949")
colnames(malaria_data)[1:2] <- c("City", "SIG_KOR_NM")

malaria_data <- malaria_data %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Infections"
  ) %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year))
  )

malaria_data2 <- malaria_data %>%
  mutate(
    Year_Group = case_when(
      Year >= 2001 & Year <= 2004 ~ "2001-2004",
      Year >= 2005 & Year <= 2009 ~ "2005-2009",
      Year >= 2010 & Year <= 2017 ~ "2010-2017",
      Year >= 2018 & Year <= 2023 ~ "2018-2023",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(City, SIG_KOR_NM, Region_Code, Year_Group) %>%
  summarise(
    Avg_Infections = round(mean(Infections, na.rm = TRUE), 2)
  )




# Made a new column of Region_Code in the geojson_sigungu file

geojson_sigungu <- geojson_sigungu %>%      
  mutate(Region_Code = substr(name, 1, 2)) 

# Values in the Region_Code column above was character, so we changed it to the integer.
# It is because the values of Region_Code column in the malaria data is integer.

geojson_sigungu <- geojson_sigungu %>%
  mutate(Region_Code = as.integer(Region_Code))


# Merged malaria_data and geojson_sigungu
merged_data <- merge(geojson_sigungu, malaria_data2, by = c("Region_Code", "SIG_KOR_NM"))

merged_data <- merged_data %>%
  mutate(Avg_Infections = ifelse(is.na(Avg_Infections), 0, Avg_Infections),
         Year_Group = as.factor(Year_Group)) %>%
  arrange(Year_Group, name)

merged_data <- merged_data %>%
  mutate(Year_Group = factor(Year_Group, levels = c("2001-2004", "2005-2009", "2010-2017", "2018-2023")))



# sf to geojson
geojson_sigungu2 <- sf_geojson(geojson_sigungu)


MIN <- min(merged_data$Avg_Infections, na.rm = TRUE)
MAX <- max(merged_data$Avg_Infections, na.rm = TRUE)


# Visualization
## We add zoom function using "roam = TRUE"
## Using e_tooltip(), we made region name, region code, and the average number of infected people are displayed in the tooltip.
## Using e_add_nested(), we made both SIG_KOR_NM and SIG_ENG_NM shown.

merged_data %>%
  group_by(Year_Group) %>%
  e_charts(name, timeline = TRUE) %>%
  e_map_register("Korea", json = geojson_sigungu2) %>%
  e_map(Avg_Infections, map = "Korea",roam = TRUE) %>%
  e_add_nested("label", SIG_KOR_NM, SIG_ENG_NM) %>% 
  e_visual_map(
    min = MIN,
    max = MAX,
  ) %>%
  e_title("South Korea Malaria Trend (2001-2023)", left = "center") %>%
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.data.label.SIG_KOR_NM + 
              ' (' + params.data.label.SIG_ENG_NM + ')' +
              '</strong><br />SIG_CD: ' + params.name +
              '</strong><br />Infection: ' + params.value) 
                }
    ")
  ) %>%
  e_timeline_opts(playInterval = 1000)








###### 3. Number of malaria infections by 'Seoul','Gyeonggi','Gangwon','Incheon'(2001-2023)
### Gapminder Annimation (Zoom Ver.)
# In this part, we filtered geojson_sigungu by 'Region Code', so it can represents only Seoul, Gyeonggi, Gangwon, Incheon.
# Also, we filtered merged_data by 'City' to represents only Seoul, Gyeonggi, Gangwon, Incheon.
# This annimation also includes a zoom function. 

geojson_sigungu <- st_read("sig2.json") 
st_crs(geojson_sigungu)

malaria_data <- read.csv("Number of malaria infections by city, county and district, 2001-2023.csv", 
                         fileEncoding = "CP949")
colnames(malaria_data)[1:2] <- c("City", "SIG_KOR_NM")

malaria_data <- malaria_data %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Infections"
  ) %>%
  mutate(
    Year = as.numeric(gsub("X", "", Year))
  )

malaria_data2 <- malaria_data %>%
  mutate(
    Year_Group = case_when(
      Year >= 2001 & Year <= 2004 ~ "2001-2004",
      Year >= 2005 & Year <= 2009 ~ "2005-2009",
      Year >= 2010 & Year <= 2017 ~ "2010-2017",
      Year >= 2018 & Year <= 2023 ~ "2018-2023",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(City, SIG_KOR_NM, Region_Code, Year_Group) %>%
  summarise(
    Avg_Infections = round(mean(Infections, na.rm = TRUE), 2)
  )


geojson_sigungu <- geojson_sigungu %>%      
  mutate(Region_Code = substr(name, 1, 2)) %>%
  mutate(Region_Code = as.integer(Region_Code))

geojson_filtered <- geojson_sigungu %>%
  filter(Region_Code %in% c("11", "41", "51", "28")) %>%
  sf_geojson()


merged_data <- merge(geojson_sigungu, malaria_data2, by = c("Region_Code", "SIG_KOR_NM"))

merged_data <- merged_data %>%
  mutate(Avg_Infections = ifelse(is.na(Avg_Infections), 0, Avg_Infections),
         Year_Group = as.factor(Year_Group)) %>%
  arrange(Year_Group, name)

merged_data <- merged_data %>%
  mutate(Year_Group = factor(Year_Group, levels = c("2001-2004", "2005-2009", "2010-2017", "2018-2023")))


filtered_data <- merged_data %>%
  filter(City %in% c("서울", "경기", "강원", "인천"))



MIN <- min(filtered_data$Avg_Infections, na.rm = TRUE)
MAX <- max(filtered_data$Avg_Infections, na.rm = TRUE)


filtered_data %>%
  group_by(Year_Group) %>%
  e_charts(name, timeline = TRUE) %>%
  e_map_register("Filtered_Korea", json = geojson_filtered) %>%  
  e_map(Avg_Infections, map = "Filtered_Korea", roam = TRUE) %>% 
  e_add_nested("label", SIG_KOR_NM, SIG_ENG_NM) %>%
  e_visual_map(
    min = MIN,
    max = MAX
  ) %>%
  e_title("Malaria Trend in Seoul, Gyeonggi, Gangwon, Incheon (2001-2023)", left = "center") %>%
  e_tooltip(
    formatter = htmlwidgets::JS("
      function(params){
        return('<strong>' + params.data.label.SIG_KOR_NM + 
              ' (' + params.data.label.SIG_ENG_NM + ')' +
              '</strong><br />SIG_CD: ' + params.name +
              '</strong><br />Infection: ' + params.value) 
                }
    ")
  ) %>%
  e_timeline_opts(playInterval = 1000)








###### 4. Number of malaria infections by 'Seoul','Gyeonggi','Gangwon','Incheon'(2001-2023)
### Line Graph
# In this line graph, we add the tooltip to show the Year, Infections, and Policy (only in certain years)
# The Policy can be seen in 2004, 2011, and 2019.

korea_line <- read_csv("Number of malaria infections.csv", locale = locale(encoding = "CP949"))

korea_long <- korea_line %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",    
    values_to = "Infections"
  ) %>%
  mutate(
    Policy = case_when(
      Year == "2004" ~ "Military and civilian control strengthened",
      Year == "2011" ~ "Mosquito surveillance enhanced",
      Year == "2019" ~ "The First Basic Plan (Rapid diagnostic kits introduced)",
      TRUE ~ NA_character_
    ),
    tooltip = ifelse(
      is.na(Policy),
      paste("Year:", Year, "<br>Infections:", Infections),
      paste("Year:", Year, "<br>Infections:", Infections, "<br>Policy:", Policy)
    )
  )

korea_line2 <- ggplot() +
  geom_col(
    data = korea_long %>% filter(Country == "전국"),
    aes(x = as.integer(Year), y = Infections, fill = "전국"),
    alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_line(
    data = korea_long %>% filter(Country != "전국"),
    aes(x = as.integer(Year), y = Infections, color = Country),
    size = 1
  ) +
  geom_point(
    data = korea_long %>% filter(Country != "전국"),
    aes(x = as.integer(Year), y = Infections, color = Country, text = tooltip),
    size = 2
  ) +
  scale_fill_manual(
    name = "Country",
    values = c("전국" = "grey80")
  ) +
  scale_color_discrete(name = "Country") +
  labs(
    title = "Malaria Infections in South Korea (2001-2023)",
    x = "Year",
    y = "Number of Infections"
  ) +
  theme_minimal(base_family = "nanumgothic") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

korea_linegraph <- ggplotly(korea_line2, tooltip = "text")

trace_names <- c("전국", "강원", "경기", "서울", "인천")
for (i in seq_along(korea_linegraph$x$data)) {
  korea_linegraph$x$data[[i]]$name <- trace_names[i]
}

korea_linegraph

korea_linegraph