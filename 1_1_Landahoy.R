# Preparation

# Needed packages 
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("mapview")
install.packages("rnaturalearthdata")

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(mapview)
library(rnaturalearthdata)

# 1. Let R know, where you want to work (aka set working direction)
setwd("..")

# 2. What exactly are we analysing? What kind of data are we looking at?

st_layers("RawData/CLC2018.gpkg")
# With this output we can see, that our Dataset has multiple layers
# The first layer is our layer of interest, here we can find the data of continental europe - still over 2 Mio charakterized areas.
# Because 2 Mio are way to much information, we have to narrow the data down.
# Besides writing code, it´s almost as import to ask ourself what do we want to achieve with the code.
# In our case: We want to know from this datasets, where we can find a land to buy for a vineyard.
# This dataset contains the information about how land is used in Europe, if we investigate the distribution of vineyards, 
# we can get a first idea, where it would be possible to fine a land matching our needs.

# 2. Let R know, what you want to analyse (aka Data Import)

# 2.1: Let´s import just that part that we are interested in: Vineyards (they are marked with Code 211 in the dataset)
# Advantage: we can easier interpret the Map and we don´t need that much time regarding data import

clc_vine <- st_read(
  "RawData/CLC2018.gpkg",
  layer = "U2018_CLC2018_V2020_20u1",
  query = "SELECT * FROM U2018_CLC2018_V2020_20u1 WHERE CODE_18 = 221"
)

# Now we have loaded 21177 features (in ou case vineyards)
# But where we can find them? 
# Lets look at this on a Map! Now it´s the time for our first plot. (Yey!)

# Just our vineyard data would be boring because we cannot see where exactly this vineyard can be found.
# To get context, let´s add a Map.

countries <- ne_countries(scale = "medium", returnclass = "sf")

# Because we want this to plot together with our CLC Data, we need to transform it in the same format (EPSG:3035)
countries <- st_transform(countries, 3035)

# And Plot!
ggplot() +
  geom_sf(data = countries_europe, fill = "grey95", color = "grey70") +
  geom_sf(data = clc_vine, fill = "darkred", color = NA) +
  theme_minimal() +
  labs(
    title = "Vineyards in Europe (CLC data 2018)",
    caption = "Data: Copernicus Land Monitoring Service (CLC2018)"
  )

# So far so good - but still to far away. This Plot looks nice, but it´s not really informative.

# Let´s "zoom in" - and transform both into a more matching format
clc_vine_4326 <- st_transform(clc_vine, 4326)
countries_europe_4326 <- st_transform(countries_europe, 4326)

p_dist_eu <- ggplot() +
  geom_sf(data = countries_europe_4326, fill = "grey95", color = "grey70") +
  geom_sf(data = clc_vine_4326, fill = "darkred", color = NA) +
  coord_sf(xlim = c(-15, 30), ylim = c(34, 60)) +
  theme_minimal()+
  labs(
    title = "Distribution of vineyards in Europe (CLC data 2018)",
    caption = "Vineyards marked in red"
  )

p_dist_eu

ggsave("a plot says more than a thousand tables/Vineyards_europe_distribution.png", plot = p_dist_eu, width = 10, height = 8, dpi = 300)

# Better! Now we slowly get an idea of vineyard distribution.
# Let´s safe this Plot in our Output folder.

# Interested in which country has the most vineyards? Let´s do a barplot!

# Data preparation:
clc_vine$area_m2 <- st_area(clc_vine)
vine_by_country <- st_join(clc_vine, countries_europe["name_long"])

vine_stats <- vine_by_country %>%
  st_set_geometry(NULL) %>%
  group_by(name_long) %>%
  summarise(
    vineyard_area_ha = sum(as.numeric(area_m2)) / 10000  # 1 ha = 10.000 m²
  ) %>%
  arrange(desc(vineyard_area_ha))

# Plot it!
p_vineyards_eu_barplot <- ggplot(vine_stats, aes(x = reorder(name_long, vineyard_area_ha), y = vineyard_area_ha)) +
  geom_col(fill = "#7B1E1E") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Vineyard area per Country in Europe (CLC data 2018)",
    x = "Country",
    y = "Vineyard area [Ha]"
  )

p_vineyards_eu_barplot

ggsave("a plot says more than a thousand tables/Vineyards_ha_europe_ranking.png", plot = p_vineyards_eu_barplot, width = 13, height = 8, dpi = 300)

# France and Spain at the top, followed by Italy - and at least suprising for me - Romania!
# Please keep in mind that our data is from 2018.

# If we further investigate our Plot, we see a NA in the list of countries. 
# This means, that some of the vineyards could not be assigned to any country.
# Wonder why? Let´s have a look.

mapview(vine_by_country[is.na(vine_by_country$name_long), ],
        col.regions = "red",
        alpha.regions = 0.7,
        legend = FALSE,
        layer.name = "NA areas")

# Here we can see, that the most NA areas are found in coastal parts or small islands.
# This has to do with the country shapes that we are assigning in line 90. 
# For our first overview it is not that important, so we are fine with it and we will 
# fix it later, if we are more interested in one of especially this regions.

# For now, let´s focus on the middle or even lower listed countries.
# As new born vintners we don´t want to have a lot of competitors - but still a good infrastructure.
# Let´s take Germany! In the world known for delicious beer, and soon also for data driven wine?

vine_de <- vine_by_country %>% 
  filter(name_long == "Germany")

rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
st_crs(countries_europe)
rivers <- st_transform(rivers, st_crs(countries_europe))
rivers_de <- st_intersection(
  rivers,
  countries_europe %>% filter(name_long == "Germany")
)

p_de <- ggplot() +
  geom_sf(data = countries_europe %>% filter(name_long == "Germany"), 
          fill = "grey95", color = "grey60") +
  geom_sf(data = rivers_de, color = "#4A90E2", size = 0.3, alpha = 0.7) +  # Flüsse
  geom_sf(data = vine_de, fill = "#8B0000", color = "darkred", size = 0.05) +
  coord_sf(crs = st_crs(3035)) +
  theme_minimal() +
  labs(
    title = "Vineyards and Major Rivers in Germany (CLC 2018)",
    subtitle = "Data: Copernicus CORINE Land Cover + Natural Earth Rivers",
    caption = "Vineyard class: CLC-Code 221"
  )

p_de

ggsave("a plot says more than a thousand tables/Vineyards_Germany.png", plot = p_de, width = 13, height = 8, dpi = 300)

# For a quick check how these areas geographically look:
mapview(vine_de,
        col.regions = "red",
        alpha.regions = 0.7,
        legend = FALSE,
        layer.name = "Vineyards")

# Thats enough for an overview - know the hot (or cool) data is going to be analyzed, talking about climate obviously.
# Stay tuned!
