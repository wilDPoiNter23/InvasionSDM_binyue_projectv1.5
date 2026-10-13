# Coastal reclamation city inventory
# Input: Reclamation polygons and city metadata
# Output: City inventory and geographic summaries

source(file.path(Sys.getenv("INVASIONSDM_PROJECT_ROOT", "."), "config", "bootstrap.R"))
library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggforce)
library(dplyr)
library(readxl)
city_df <- read.csv(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/01_reclaimedcities_withIAPs.csv"))
# Define the list of cities and their corresponding provinces and countries
City = c("Mombasa", "Maputo", "Mogadishu", "Dar es Salaam", "Luanda", "Daula",
         "Pointe-Noire", "Algiers", "Alexandria", "Tripoli", "Casablanca", "Rabat",
         "Tangier", "Tunis", "Cape Town", "Port Elizabeth", "Abidjan", "Accra",
         "Conakry", "Morovia", "Nouakchott", "Lagos", "Dakar", "Freetown", "Lome",
         "Dalian", "Dongying", "Guangzhou", "Haikou", "Jiaxing", "Ningbo", "Putian",
         "Qingdao", "Qinhuangdao", "Quanzhou", "Rizhao", "Shanghai", "Shantou",
         "Shenzhen", "Tangshan", "Tianjin", "Weihai", "Wenzhou", "Xiamen", "Yantai",
         "Yingkou", "Zhanjiang", "Zhongshan", "Zhuhai", "HongKong", "Kaohsiung",
         "Taipei", "Taichung", "Tokoname", "Hiroshima", "Osaka", "Kitakyushu",
         "Shizuoka", "Tokyo", "Busan", "Incheon", "Chittagong", "Chennai", "Kochi",
         "Kozhikode", "Mumbai", "Thiruvananthapuram", "Visakhapatnam", "Karachi",
         "Bandar Lampung", "Jakarta", "Makassar", "Samarang", "Surabaya", "Manila",
         "Singapore", "Chon Buri", "Da Nang", "Haifa", "Tel Aviv-Jafia", "Kuwait city",
         "Beirut", "Muscat", "Ad-Dammam", "Jiddah", "Antalya", "Istanbul", "Izmir",
         "Abu Dhabi", "Sharjah", "Dubai", "St Petersburg", "Copenhagen", "Helsinki",
         "Dublin", "Athens", "Naples", "Lisbon", "Porto", "Barcelona", "Amsterdam",
         "Rotterdam", "Havana", "Santo Domingo", "Port-au-Prince", "San Juan",
         "Panama city", "Buenos Aires", "Florianopolis", "Fortaleza", "Jo?o Pessoa",
         "Maceio", "Recife", "Rio de Janeiro", "Salvador", "Cartagena", "Lima",
         "Montevideo", "Maracaibo", "Boston", "Jacksonville", "Los Angeles- Long Beach-Santa Ana",
         "Miami", "Vancouver", "New York", "San Diego", "San Francisco", "Tampa- St. Petersburg",
         "Virginia Beach", "Adelaide", "Brisbane", "Melbourne", "Perth", "Sydney",
         "Auckland")
Province = c("Coast Province", "Maputo Province", "Banaadir", "Dar es Salaam", "Luanda Province", "Littoral Region",
             "Pointe-Noire", "Algiers Province", "Alexandria", "Tripoli District", "Casablanca-Settat", "Rabat-Salé-Kénitra",
             "Tangier-Tetouan-Al Hoceima", "Tunis Governorate", "Western Cape", "Eastern Cape", "Lagunes", "Greater Accra",
             "Conakry", "Montserrado", "Nouakchott", "Lagos State", "Dakar Region", "Western Area", "Maritime Region",
             "Liaoning", "Shandong", "Guangdong", "Hainan", "Zhejiang", "Zhejiang", "Fujian",
             "Shandong", "Hebei", "Fujian", "Shandong", "Shanghai", "Guangdong",
             "Guangdong", "Hebei", "Tianjin", "Shandong", "Zhejiang", "Fujian", "Shandong",
             "Liaoning", "Guangdong", "Guangdong", "Guangdong", "Hong Kong", "Kaohsiung",
             "Taipei", "Taichung", "Aichi", "Hiroshima", "Osaka", "Fukuoka",
             "Shizuoka", "Tokyo", "Busan", "Incheon", "Chittagong", "Tamil Nadu", "Kerala",
             "Kerala", "Maharashtra", "Kerala", "Andhra Pradesh", "Sindh",
             "Lampung", "Jakarta", "South Sulawesi", "Central Java", "East Java", "Metro Manila",
             "Central Singapore", "Chon Buri", "Da Nang", "Haifa", "Tel Aviv District", "Al Asimah",
             "Beirut", "Muscat", "Eastern Province", "Makkah Province", "Antalya Province", "Istanbul", "Izmir",
             "Abu Dhabi", "Sharjah", "Dubai", "Saint Petersburg", "Capital Region", "Uusimaa",
             "Leinster", "Attica", "Campania", "Lisbon", "Porto", "Catalonia", "North Holland",
             "South Holland", "La Habana", "Santo Domingo", "Ouest", "San Juan",
             "Panamá Province", "Buenos Aires", "Santa Catarina", "Ceará", "Paraíba",
             "Alagoas", "Pernambuco", "Rio de Janeiro", "Bahia", "Bolívar", "Lima",
             "Montevideo", "Zulia", "Massachusetts", "Florida", "California",
             "Florida", "British Columbia", "New York", "California", "California", "Florida",
             "Virginia", "South Australia", "Queensland", "Victoria", "Western Australia", "New South Wales",
             "Auckland Region")
Country = c("Kenya", "Mozambique", "Somalia", "Tanzania", "Angola", "Cameroon",
            "Republic of the Congo", "Algeria", "Egypt", "Libya", "Morocco", "Morocco",
            "Morocco", "Tunisia", "South Africa", "South Africa", "Ivory Coast", "Ghana",
            "Guinea", "Liberia", "Mauritania", "Nigeria", "Senegal", "Sierra Leone", "Togo",
            "China", "China", "China", "China", "China", "China", "China",
            "China", "China", "China", "China", "China", "China",
            "China", "China", "China", "China", "China", "China", "China",
            "China", "China", "China", "China", "China", "Taiwan",
            "Taiwan", "Taiwan", "Japan", "Japan", "Japan", "Japan",
            "Japan", "Japan", "South Korea", "South Korea", "Bangladesh", "India", "India",
            "India", "India", "India", "Pakistan",
            "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Philippines",
            "Singapore", "Thailand", "Vietnam", "Israel", "Israel", "Kuwait",
            "Lebanon", "Oman", "Saudi Arabia", "Saudi Arabia", "Turkey", "Turkey", "Turkey",
            "United Arab Emirates", "United Arab Emirates", "United Arab Emirates", "Russia", "Denmark", "Finland",
            "Ireland", "Greece", "Italy", "Portugal", "Portugal", "Spain", "Netherlands",
            "Netherlands", "Cuba", "Dominican Republic", "Haiti", "Puerto Rico",
            "Panama", "Argentina", "Brazil", "Brazil", "Brazil",
            "Brazil", "Brazil", "Brazil", "Brazil", "Colombia", "Peru",
            "Uruguay", "Venezuela", "United States", "United States", "United States",
            "United States", "Canada", "United States", "United States", "United States", "United States", "United States",
            "United States", "Australia", "Australia", "Australia", "Australia", "Australia","New Zealand")
city_details <- data.frame(
  City ,
  Province ,
  Country
)

# Save the result to a CSV file
write_csv(city_details, publication_path("LOCAL_ANALYSIS_ROOT", "data/Reclamation_Cities_Provinces_States_Countries.csv"))

# Print the first few rows of the data frame
print(head(city_details))

city_df
# 2. World basemap (sf).
world <- ne_countries(scale = "medium", returnclass = "sf")
crs_moll <- "+proj=moll +datum=WGS84"

world_moll <- st_transform(world, crs_moll)
# 3. Convert cities to sf points (WGS84).
cities_moll <- st_as_sf(
  city_df,
  coords = c("Longitude", "Latitude"),
  crs = 4326
) |>
  st_transform(crs_moll)
#-----------------------------
# 3. Elliptical ocean frame extending beyond the world extent.
#-----------------------------
bbox <- st_bbox(world_moll)

x0 <- (bbox["xmin"] + bbox["xmax"]) / 2   # Ellipse centre: x coordinate.
y0 <- (bbox["ymin"] + bbox["ymax"]) / 2   # Ellipse centre: y coordinate.

range_x <- bbox["xmax"] - bbox["xmin"]
range_y <- bbox["ymax"] - bbox["ymin"]

# Extend the major and minor semiaxes beyond the world extent to enclose all land.
a <- as.numeric(range_x * 0.55)
b <- as.numeric(range_y * 0.55)

theta <- seq(0, 2*pi, length.out = 500)

ellipse_coords <- cbind(
  x0 + a * cos(theta),
  y0 + b * sin(theta)
)

# Close the polygon by repeating the first vertex at the end.
ellipse_closed <- rbind(ellipse_coords, ellipse_coords[1, ])

ellipse_sf <- st_sfc(
  st_polygon(list(ellipse_closed)),
  crs = crs_moll
) |>
  st_sf()

#-----------------------------
# 4. Plot the elliptical ocean, land and city points.
#-----------------------------
p <- ggplot() +
  # Elliptical ocean background with a dark grey border.
  geom_sf(
    data   = ellipse_sf,
    fill   = "#D8E8F3",   # Light blue ocean.
    colour = "grey30",    # Dark grey border.
    linewidth = 0.6
  ) +
  # Land fully enclosed by the ellipse.
  geom_sf(
    data   = world_moll,
    fill   = "#F1E0C5",   # Beige land.
    colour = "#818181",     # Thin black national boundaries.
    linewidth = 0.2
  ) +
  # Orange city points.
 geom_sf(
  data   = cities_moll,
  colour = scales::alpha("#9c7149", 0.65),
  fill   = scales::alpha("#eca87d", 0.55),
  size   = 4,
  stroke = 1
) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(5, 5, 5, 5)
  )

p
ggsave(publication_path("LOCAL_ANALYSIS_ROOT", "data/organized_table/cities_forppt.pdf"), p, width = 8, height = 6, dpi = 600)
