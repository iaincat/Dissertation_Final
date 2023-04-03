
```{r test_store}
# Load bishy store boundary shapefile
bishy <- read_sf("bishy.shp")

# Convert this to the correct 3857 CRS
bishy = st_transform(bishy, crs = st_crs(3857))

# rename the geometry column
bishy <- bishy  |>  rename_at('geometry', ~'bishygeometry')
```

```{r yisochronecalc}
#############################################################################
#                                                                           #
#                           METHOD                                          #
#                                                                           #
#############################################################################
# Uses library(mapview) and library(openrouteservice)
# Create an isochrone around the Bishy Weigh zero waste store

# embed data in the output file
#mapviewOptions(fgb = FALSE)

# Get the coordinates of the bishy weigh
#coordinates <- data.frame(lon = bishy$Lon, lat = bishy$Lat)

## Calculate a 20 minute walking time to the store
#ywalktime20 <- ors_isochrones(coordinates, range = 1200, interval = 1200, 
#                      output = "sf", profile = ors_profile("walking"))
#values <- levels(factor(ywalktime20$value))
#ranges <- split(ywalktime20, values)
#ranges <- ranges[rev(values)]
#names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)

# Convert the walktime20 isochrone to the correct CRS
#ywalktime20 <- st_transform(ywalktime20, crs = st_crs(3857))

#remove(coordinates, values, ranges)
#
# Save the York isochrone data
#saveRDS(obj = ywalktime20, file = "ywalktime20.rds")

#############################################################################

# Load the York isochrone data
ywalktime20 <- readRDS("ywalktime20.rds")

# Uses library(ggspatial)
# Convert the walktime20 isochrone to the correct CRS
ywalktime20 <- st_transform(ywalktime20, crs = st_crs(3857))
```

```{r yca_map, fig.cap="20 minutes walking distance isochrone from The Bishy Weigh store in York"}
# Map the 20 mins isochrone and store location
# use the walktime LAYER isochrone as the main data layer
ggplot(ywalktime20, aes(colour = "20 minutes walk time")) +
  # Add the basemap tile 
  annotation_map_tile(zoomin = 0, type = "osm") +
  # set the walktime LAYER colour fill and transparency
  geom_sf(fill = "blue", alpha = 0.1) +
  # add the store and set its colour
  geom_sf(data = bishy, colour = "red", size = 2,
          aes(fill = st_nam)) +
  # Adjust the legend titles based on the aes settings for both layers
  guides(color = guide_legend(title = "Catchment Area")) +
  guides(fill = guide_legend(title = "Store Name")) +
  
  # North arrow and scale bar
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical) +
  theme_bw()
```

```{r york_extract}

# Load the lsoa data
lsoas <- readRDS("lsoas.rds")
# Uses library(stringr)
# Clip the lsoas file so that only those lsoas that are covered by York are shown
# Extract all the York rows from lsoas to ylsoa dataframe and set the CRS
ylsoa <- lsoas[str_detect(lsoas$LSOA21NM, "^York"), ]

# Make sure the CRS is correct
ylsoa = st_transform(ylsoa, crs = st_crs(3857))

# Have a dataframe that only contain the Bishopthorpe Road Retail Centre and set the CRS
# Load the retail centre data 
rc_results <- readRDS("rc_results.rds")
# Convert the rc_results data to a sf
rc_results = st_as_sf(rc_results)

# CLip the data for the bishopthorpe road RC
yrcbs <- rc_results[str_detect(rc_results$RC_Name, "^Bishopthorpe"), ]

# Make sure the CRS is correct
yrcbs = st_transform(yrcbs, crs = st_crs(3857))

# remove data that is not needed to save memory
remove(rc_results, lsoas)
```

```{r yca_map2, fig.cap="20 minutes walking distance isochrone from The Bishy Weigh store in York, with York LSOAs"}
# Plot the york lsoas with the bishy weigh store and the isochrone(res)
ggplot(ywalktime20, aes(colour = "20 minutes walk time")) +
  ## Baselayer
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(fill = "blue", alpha = 0.4) +
  geom_sf(data = ylsoa, colour = "black", 
          aes(geometry = lsoageometry, alpha = "")) +
  geom_sf(data = bishy, colour = "red", aes(fill = st_nam)) +
  guides(fill = guide_legend(title = "Store Name")) +
  guides(colour = guide_legend(title = "Catchment Area")) +
  guides(alpha = guide_legend(title = "LSOAs")) +
  
  # North arrow andd scale bar
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical) +
  theme_bw()
```

```{r ylsoaclip}
# Load the lsoas data
lsoas <- readRDS("lsoas.rds")
# convert lsoas back to sf format
#lsoas = st_as_sf(lsoas)
# Load the PWC data
lsoa_pwc <- readRDS("lsoa_pwc.rds")
# Make sure the crs is correct
lsoa_pwc = st_transform(lsoa_pwc, crs = st_crs(3857))
ywalktime20 = st_transform(ywalktime20, crs = st_crs(3857))
# clip the PWCs to only show those inside the 20 minute walk time
y_pwc_clip = lsoa_pwc[ywalktime20,]
# clip the LSOAs to only show those for the clipped PWCs
ylsoa_clip = lsoas[y_pwc_clip,]
```

```{r yca_map3, fig.cap="20 minutes walking distance isochrone from The Bishy Weigh store in York, with catchment area LSOAs, and retail centre."}
# Plot the York lsoas from ylsoa that touch the walktime20 boundary on a map
# Isochrone Layer & Legend settings
ggplot(ywalktime20, aes(colour = "20 minutes walk time")) +
  guides(colour = guide_legend(title = "Catchment Area")) +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  # fill the catchment area blue
  geom_sf(fill = "blue", alpha = 0.2) +
  # LSOA Layer + Legend Settings
  geom_sf(data = ylsoa_clip, colour = "black", aes(alpha = "")) +
  guides(alpha = guide_legend(title = "LSOAs")) +
  # Store Layer + Legend Settings
  geom_sf(data = bishy, colour = "red", aes(fill = st_nam)) +
  guides(fill = guide_legend(title = "Store Name")) +
  # Population Weighted Centroid Layer + Legend Settings
  geom_sf(data = y_pwc_clip, aes(size = ""), colour = "black") +
  guides(size = guide_legend(title = "Population Weighted Centroids")) +
  
  # North arrow andd scale bar
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical) +
  theme_bw()
```

```{r y_de_map, fig.cap = "Map showing 'degree educated' variable scores for the LSOAs captured by the isochrone around the store"}
# plot the 1st of 5 variables in study - use The Bishy Weigh as the example
ggplot(ylsoa_clip) +
  geom_sf(aes(fill = dedscr)) +
  scale_fill_viridis_c(breaks=seq(0,10,by=1)) +
  guides(fill = guide_legend(title = "Degree Educated Scores", reverse = TRUE)) +
  geom_sf(data = bishy, aes(colour = bishy$st_nam)) +
  guides(colour = guide_legend(title = "Store Name"))

```

```{r y_occ_map, fig.cap = "Map showing 'occupations' variable scores for the LSOAs captured by the isochrone around the store"}
# plot the 2nd of 5 variables in study - use The Bishy Weigh as the example
ggplot(ylsoa_clip) +
  geom_sf(aes(fill = occscr)) +
  scale_fill_viridis_c(breaks=seq(0,10,by=1)) +
  guides(fill = guide_legend(title = "Occupation Scores", reverse = TRUE)) +
  geom_sf(data = bishy, aes(colour = bishy$st_nam)) +
  guides(colour = guide_legend(title = "Store Name"))
```

```{r  y_ten_map, fig.cap = "Map showing 'tenure' variable scores for the LSOAs captured by the isochrone around the store"}
# plot the 3rd of 5 variables in study - use The Bishy Weigh as the example
ggplot(ylsoa_clip) +
  geom_sf(aes(fill = tenscr)) +
  scale_fill_viridis_c(breaks=seq(0,10,by=1)) +
  guides(fill = guide_legend(title = "Tenure Scores", reverse = TRUE)) +
  geom_sf(data = bishy, aes(colour = bishy$st_nam)) +
  guides(colour = guide_legend(title = "Store Name"))
```

```{r y_car_map, fig.cap = "Map showing 'access to number of cars (1)' variable scores for the LSOAs captured by the isochrone around the store"}
# plot the 4th of 5 variables in study - use The Bishy Weigh as the example
ggplot(ylsoa_clip) +
  geom_sf(aes(fill = carscr)) +
  scale_fill_viridis_c(breaks=seq(0,10,by=1)) +
  guides(fill = guide_legend(title = "One Car access", reverse = TRUE)) +
  geom_sf(data = bishy, aes(colour = bishy$st_nam)) +
  guides(colour = guide_legend(title = "Store Name"))
```

```{r y_ppl_map, fig.cap = "Map showing 'households comprising 2 people' variable scores for the LSOAs captured by the isochrone around the store"}

# plot the 5th of 5 variables in study - use The Bishy Weigh as the example
ggplot(ylsoa_clip) +
  geom_sf(aes(fill = pplscr)) +
  scale_fill_viridis_c(breaks=seq(0,10,by=1)) +
  guides(fill = guide_legend(title = "2 Person household scores", reverse = TRUE)) +
  geom_sf(data = bishy, aes(colour = st_nam)) +
  guides(colour = guide_legend(title = "Store Name"))
```

```{r bishy_scr_prep}
# Join the ylsoa_clip data
ylsoa_joined <- st_union(ylsoa_clip$lsoageometry)

# Total the scores for the ylsoas
#convert the ylsoa_clip data to a dataframe
ylsoa_clip = as.data.frame(ylsoa_clip)

# Add a total column for each row
ylsoa_clip = ylsoa_clip |> 
  mutate(totscr = rowSums(ylsoa_clip[,9:13], na.rm = TRUE),
         .after=dedscr)

# Total the scrores to a new row at the bottom of the dataframe
scrs = ylsoa_clip |>
  bind_rows(summarise(ylsoa_clip[,9:14], across(where(is.numeric), mean),
                      across(where(is.character), ~'Total')))

# Get the scores data
scrs = scrs[nrow(scrs),9:14]

# Round the scores to 0 decimal places
scrs = round(scrs, digits = 0)

# Add the scores data to the bishy data
bishy = cbind(bishy, scrs)

# Add the isochrone data to the bishy data
# rename the iso geometry
ywalktime20 = st_transform(ywalktime20, crs = st_crs(3857))
ywalktime20 <- ywalktime20  |>  rename_at('geometry', ~'isogeometry')
ywalktime20 <- ywalktime20  |>  rename_at('center', ~'isocenter')
bishy = cbind(bishy, ywalktime20, ylsoa_joined)
bishy <- bishy  |>  rename_at('geometry', ~'lsoajoingeometry')
```

```{r plot_bishy_finalscrs}
# Make sure the bishy crs is correct
bishy = st_transform(bishy, crs = st_crs(3857))
# Plot the bishy data
ggplot(bishy$isogeometry, aes(colour = "20 minutes walk time")) +
  guides(colour = guide_legend(title = "Catchment Area")) +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  # LSOA Layer + Legend Settings
  geom_sf(data = bishy$lsoajoingeometry, colour = "black", aes(alpha = bishy$totscr)) +
  guides(alpha = guide_legend(title = "Joined LSOA Total Score")) +
  # fill the catchment area blue
  geom_sf(fill = "blue", alpha = 0.2) +
  # Store Layer + Legend Settings
  geom_sf(data = bishy, colour = "red", aes(fill = st_nam)) +
  guides(fill = guide_legend(title = "Store Name")) +
  # Score Layer
  #  geom_sf(data = bishy$totscr, aes(colour = "yellow")) +
  #  guides(guide_legend(title = "Total Score")) +
  
  # Population Weighted Centroid Layer + Legend Settings
  #  geom_sf(data = y_pwc_clip, aes(size = ""), colour = "black") +
  #  guides(size = guide_legend(title = "Population Weighted Centroids")) +
  
  # North arrow andd scale bar
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_nautical) +
  theme_bw()
```

```{r remove_bishy_data}
remove(bishy, lsoa_pwc, lsoas, scrs, y_pwc_clip, ylsoa, ylsoa_clip, ylsoa_joined, yrcbs, ywalktime20, zws)
```

```{r load_data}
zws_results <- readRDS("zws_results.rds")
rc_results <- readRDS("rc_results.rds")
lsoas <- readRDS("lsoas.rds")
lsoa_pwc <- readRDS("lsoa_pwc.rds")
```

```{r retailcentres}
# Load retail centres shapefile
#retail_c <- read_sf("retail_centres.shp")

# Convert this to the correct 3857 CRS
#retail_c = st_transform(retail_c, crs = st_crs(3857))

#rename the retail centre geometry
#retail_c <- retail_c  |>  rename_at('geometry', ~'rcgeometry')

# Load the retail centre centroids shapefile
#rc_centroid <- read_sf("rc_centroids.shp")

# Convert this to the correct 3857 CRS
#rc_centroid = st_transform(rc_centroid, crs = st_crs(3857))

#rename the rc_centroid geometry
#rc_centroid <- rc_centroid  |>  rename_at('geometry', ~'rccentroidgeom')
```

```{r lsoadata}
# Load LSOA boundary shapefile - LSOA scale
#lsoas <- read_sf("lsoas.shp")

# Convert this to the correct 3857 CRS
#lsoas = st_transform(lsoas, crs = st_crs(3857))

# rename the gemetry column
#lsoas <- lsoas |> rename_at('geometry', ~'lsoageometry')

# rename the other column names that are wrong
#colnames(lsoas)[13] <- 'pplscr'

# move a column to the correct place
#lsoas <- lsoas |> relocate(dedscr, .after = pplscr)

# save this as a RDS file
#saveRDS(obj = lsoas, file = "lsoas.rds")

# Load zero waste stores shapefile - LSOA scale
#zws <- read_sf("zws.shp")

# Convert this to the correct 3857 CRS
#zws = st_transform(zws, crs = st_crs(3857))

# rename the geometry column
#zws <- zws  |>  rename_at('geometry', ~'zwsgeometry')

# Clean the stores that do not have a location from the dataframe (zws)
#zws = zws |>  drop_na(Lon)
# 18 "Cupboard Love" - middle of nowhere so no pwcs
# 36 "Health Foods For You"
# 37 "Incredible Bulk"
# 48 "Love + Joy Home"
# 52 "NADA"
# 113 "The Quiet SIte"
# Test these 5 individually

# Leave 52 and 36 in zws - the rest have no pwc around them so remove them
#zws = zws  |> 
#  filter(!row_number() %in% c(113, 53, 48, 37, 18))

# Load LSOA population weighted centroids shapefile - LSOA scale
#lsoa_pwc <- read_sf("lsoa_pwc.shp")

# Convert this to the correct 3857 CRS
#lsoa_pwc = st_transform(lsoa_pwc, crs = st_crs(3857))

# rename the geometry column
#lsoa_pwc <- lsoa_pwc  |>  rename_at('geometry', ~'pwcgeometry')

#saveRDS(obj = lsoa_pwc, file = "lsoa_pwc.rds")

# Join the PWCS data to the LSOAs data
# Convert to a dataframe
lsoas <- as.data.frame(lsoas)
lsoa_pwc <- as.data.frame(lsoa_pwc)

# Join the data together
lsoas_pwc = lsoas  |> 
  left_join(lsoa_pwc, by='LSOA21CD')

# remove the pcw data
remove(lsoa_pwc, lsoas)
```

```{r multicollinearity}
# Test for multicollinearity
mcortest = st_drop_geometry(lsoas_pwc[,9:13])
head(mcortest)

# correlation test 1
M <- cor(mcortest)
head(round(M,2))
# plot the values to visualise
corrplot(M, method="number")

# plot the trends
plot(mcortest[,c(1:5)], cex = 0.3, 
     col = grey(0.145,alpha=0.5), upper.panel=panel.smooth)

# regression model for significance values
m1 <- lm(occscr ~ tenscr + carscr + pplscr + dedscr, data = mcortest)
m2 <- lm(tenscr ~ occscr + carscr + pplscr + dedscr, data = mcortest)
m3 <- lm(carscr ~ occscr + tenscr + pplscr + dedscr, data = mcortest)
m4 <- lm(pplscr ~ occscr + tenscr + carscr + dedscr, data = mcortest)
m5 <- lm(dedscr ~ occscr + tenscr + pplscr + carscr, data = mcortest)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

# Remove unrequired variables
remove(M)
remove(m1)
remove(m2)
remove(m3)
remove(m4)
remove(m5)
```

```{r totscores}
# Sum the variable scores into a new column in lsoas
# Convert the lsoas sf file to a data.frame
lsoas = as.data.frame(lsoas)

# sum the individual LSOA row values in columns 9 to 13 and add the result to a new column
lsoas = lsoas |> 
  mutate(totscr = rowSums(lsoas[,9:13], na.rm = TRUE),
         .after=dedscr)
```

```{r msoadata}
# Load the MSOA scale data 
# Load MSOA population weighted centroids shapefile - MSOA scale
#msoa_pwc <- read_sf("msoa_pwc.shp")
# Convert this to the correct 3857 CRS
#msoa_pwc = st_transform(msoa_pwc, crs = st_crs(3857))

# Load MSOA boundary shapefile- MSOA scale
#msoas <- read_sf("msoas.shp")
# Convert this to the correct 3857 CRS
#msoas = st_transform(msoas, crs = st_crs(3857))

# Load 55 years of age data shapefile - MSOA scale
#age_ff <- read_sf("age.shp")
# Convert this to the correct 3857 CRS
#age_ff = st_transform(age_ff, crs = st_crs(3857))
```

```{r zwsmapembed, include=FALSE}
# Uses library(ggplot2) and library(basemaps)
#get_maptypes()

#ext <- st_bbox(c(xmin = -1246875 , xmax = 374932, ymin = 6273808, ymax = 8174095), crs = 3857)
#zwsmap <- ggplot() + 
#  basemap_gglayer(ext, map_service = "esri", map_type = "world_light_gray_base" ,map_res = 1) + 
#  geom_sf(data = zws, shape = 4,  size = 1, color = "blue") +
#  coord_sf() +
#  scale_fill_identity() +
#  labs(x = "", y = "")
#ggsave("zwsmap.png")
```



```{r zws_isoloop}
# Loops throughthe zws file and creates a new dataframe (zwsiso) that holds the zws store data and the isochrone for the walktime20 mins boundary.
remove(i)
remove(isodf)
#remove(zwsiso)
remove(coordinates)
remove(ranges)
remove(values)
remove(walktime20)
remove(zwsisoc)
remove(zwsiso)

# create a dataframe to hold the iso results
zwsiso <- data.frame(matrix(ncol = 12, nrow = 0))
#############################################################################
#i = 0
#i = i + 1
#############################################################################
for(i in 1:nrow(zws)) {
  # create a df variable to hold the zws values from i
  isodf = zws[i,]
  # embed data in the output file rather than html streaming option
  mapviewOptions(fgb = FALSE)
  
  # Get the coordinates of the store in the (df)
  coordinates <- data.frame(lon = isodf$Lon, lat = isodf$Lat)
  
  # Calculate a 20 minute walking time isochrone around the store
  walktime20 <- ors_isochrones(coordinates, range = 1200, interval = 1200, 
                               output = "sf", profile = ors_profile("walking"))
  values <- levels(factor(walktime20$value))
  ranges <- split(walktime20, values)
  ranges <- ranges[rev(values)]
  names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)
  
  # Convert the walktime20 isochrone to the correct CRS
  # USE THIS AS THE ISOCHRONE
  walktime20 <- st_transform(walktime20, crs = st_crs(3857))
  
  # rename the walktime20 geometry column
  walktime20 <- walktime20  |>  rename_at('geometry', ~'walktimegeometry')  
  #walktime20 <- as.data.frame(walktime20)
  
  # Join the calculated walktime 20 data to the zws data in isodf
  zwsisoc <- cbind(isodf, walktime20)
  zwsiso = zwsiso <- rbind(zwsiso, zwsisoc) 
  
  cat(i, "completed\r")
  
  # print i as a count of rows that have completed
  #print(i)
  
  # remove the temporary environments
  remove(isodf)
  remove(coordinates)
  remove(ranges)
  remove(values)
  remove(walktime20)
  remove(zwsisoc)
}
```

```{r totloop}
#zwsiso = zwsiso  |> 
#  filter(!row_number() %in% c(53))

## Uses the ischrone data from (zwsiso) to clip the lsoas and sum the scores for each variable.

# Creates a loop that iterates through the zwsiso file.
# Each line contains an isochrone.
# It uses this boundary to search and clip the pwc data to any centroids falling within the boundary.
# It then clips the lsoa data to only those lsoas that match the centroids.
# A total score for each store needs to be calculated and added to a new dataframe to be used for comparison.
remove(i)
remove(joined_lsoas)
remove(alljoined)
remove(df)
remove(ft)
remove(coordinates)
remove(walktime20)
remove(lsoa_pwc_clip)
remove(lsoa_clip)
remove(lsoa_clip_tot)
remove(finaldf)


# Remove the broken stores from zwsio
#zwsiso = zwsiso  |> 
#  filter(!row_number() %in% c(113, 53, 48, 37, 18))

#zwsiso = zwsiso  |> 
#  filter(!row_number() %in% c(53))
####################### Start the loop

############################################################################
# initialise the index to 0
#i = 0
# set i to 1 
#i = i + 1
############################################################################

#create a final dataframe
finaldf <- data.frame(matrix(ncol = 19, nrow = 0))

# Start the loop
for(i in 1:nrow(zwsiso)) {
  
  # copy the data in row(i) to a temporary df 
  df = zwsiso[i,]
  
  # Convert lsoas back to an sf format
  lsoas = st_as_sf(lsoas)
  # Clip pwc's to a new dataframe for those that are in walktime20 isochrone
  lsoa_pwc_clip  <-  lsoa_pwc[zwsiso$walktimegeometry[i],]
  
  # If the lsoa_pwc_clip dataframe has 0 rows skip to the next zwsiso row
  #  if (nrow(lsoa_pwc_clip) = 0) {
  #e <- 
  #    next
  #}
  
  # clip the LSOAs to a new dataframe for those that are the clipped lsoa_PWCs
  lsoa_clip <-  lsoas[lsoa_pwc_clip,]
  
  # create the lsoa_clip_tot variable by totalling all the scores from the clipped area
  # Convert the lsoas sf file to a data.frame
  lsoa_clip_tot = lsoa_clip
  
  
  
  lsoa_clip_tot = as.data.frame(lsoa_clip)
  
  # sum the row values in column 14 and add the result to a new column
  lsoa_clip_tot = lsoa_clip |> 
    mutate(fintot = colSums(lsoa_clip_tot[14], na.rm = TRUE),.after = totscr)
  
  # Join the lsoas to a single shape - USE THIS "JOINED_LSOAS" AS THE FINAL LSOA SHAPE
  joined_lsoas <- st_union(lsoa_clip_tot$lsoageometry)
  joined_lsoas <- as.data.frame(joined_lsoas)
  joined_lsoas <- joined_lsoas  |>  rename_at('geometry', ~'finlsoageometry')  
  
  # Create a tempp df "FT" to hold the totals for the whole joined LSOAs for each variable
  ft <- data.frame(matrix(ncol = 6, nrow = 0))
  # rename the columns of the new temp df
  colnames(ft)<-c("occscrtot","tenscrtot","carscrtot","pplscrtot","dedscrtot",
                  "totscrtot")
  
  # Convert lsoa_clip_tot to a dataframe
  lsoa_clip_tot = as.data.frame(lsoa_clip_tot)
  # sum the individual variables for the whole lsoa_clip_tot and add the result to the bottom of lsoa_clip_tot
  ft[nrow(ft) + 1,] = (colSums(lsoa_clip_tot[,9:14]))
  
  
  # Convert df to a dataframe
  df <- as.data.frame(df)
  
  # Join all the data together
  # df contains the zws store and isochrone data
  # joined_lsoas contains the single joined LSOA shape
  # ft contains total scores for 5 variables & the combined total area score
  alljoined = cbind(df, joined_lsoas, ft)
  #names(alljoined)
  
  # Add this (alljoined) data to a new line in our final data frame (finaldf)
  #remove(finaldf)
  finaldf <- rbind(finaldf, alljoined)
  
  # remove the temp dataframes etc
  remove(alljoined)  
  remove(df)
  remove(ft)
  remove(lsoa_clip_tot)
  remove(joined_lsoas)
  remove(lsoa_clip)
  remove(lsoa_pwc_clip)
  
  # Print i as a count of rows that have completed
  #print(i)
  #  i=i+1
  cat(i, "completed\r")
}
remove(i)
finaldf = st_as_sf(finaldf)
finaldf <- st_transform(finaldf, crs = st_crs(3857))
kable(finaldf)

#Save "finadf" as a csv
#finaldf <- as.data.frame(finaldf)
#finaldf = st_as_sf(finaldf)
#saveRDS(obj = finaldf, file = "finaldf.rds")
#saveRDS(obj = retail_c, file = "retail_c.rds")

# Display a table of finaldf
#kable(zws[1:10,], caption = "\\label{tab:zws}Variables included in zero-waste stores data")|>
#  kable_styling(full_width = FALSE, latex_options = "HOLD_position", font_size = 8)
```

```{r test_totmaps}
# convert finaldf to a dataframe
finaldf = as.data.frame(finaldf)

# convert finaldf to an sf format
finaldf = st_as_sf(finaldf)

# Convert finaldf to CRS 3857
finaldf <- st_transform(finaldf, crs = st_crs(3857))

# Map a store data from the final dataframe
ggplot(finaldf[1,]) +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  # Add the isochrone
  geom_sf(aes(geometry = walktimegeometry, alpha = ""), colour = "blue") +
  guides(alpha = guide_legend(title = "20 minutes walking isochrone")) +
  # Add the Store
  geom_sf(data = finaldf[1,], aes(geometry = zwsgeometry, fill = finaldf[1,]$st_nam), colour = "red") +
  guides(fill = guide_legend(title = "Store name")) +
  # Add the joined lsoa shape
  geom_sf(data = finaldf[1,], aes(geometry = finaldf[1,]$finlsoageometry, colour = ""), alpha = 0.2) +
  guides(colour = guide_legend(title = "Final LSOA")) +
  # add the totals for each variable
  ggtitle("Total Score", finaldf[1,]$totscrtot)
```

```{r avetotscrcalc}
# Find the average total score from finaldf totscrtot
finaldf  = as.data.frame(finaldf)
avgtotscr = mean(finaldf$totscrtot)
```

```{r testtots}
# Create a table of the Variables totals
#tots = as.data.frame(finaldf[1, 11:15])
## convert to a sf
#tots = st_as_sf(tots)
#st_drop_geometry(tots)
```

```{r rc_centr_joiner}
# Join the retail centres and rc-centroid data in to a single dataframe called "retail_c" 
retail_c = cbind(retail_c, rc_centroid)
remove(rc_centroid)
# Test for success...
# make df1 a dataframe to be able to compare
#df1<-data.frame(retail_c$RC_ID,retail_c$RC_ID.1)
## make df2 No if the rows values dont match
#df2 <- ifelse(df1$retail_c.RC_ID==df1$retail_c.RC_ID.1,"","No")
## convert df2 to a dataframe
#df2 = as.data.frame(df2)
## Sum the nummber of "No" values in df2 
#sum(str_detect(df2$df2, '^No$')) > 0
## remove df1 and df2
#remove(df1)
#remove(df2)
```

```{r retail_c_prep}
# Remove retail centres from retail_c that are not required

# Remove retail centres that are not in England and Wales from retail_c
retail_c <- retail_c[grep(c("Wales|England"), retail_c$Country),]

# Remove retail centres that are too large
# Those zwstores that are located within a retail centre are mainly in :-
# Town Centre
# Small Local Centre
# Local Centre
# Market Town
retail_c <- retail_c[grep(c("Town Centre|Small Local Centre|Local Centre|Market Town"), retail_c$Classifica),]

table(retail_c$Classifica)

# Split the rc_no_zws file in to chunks so that they can be processed
# and then re-combined

# Create the 2 temporary dataframes
rc_1 <- data.frame(matrix(ncol = 20, nrow = 0))
rc_2 <- data.frame(matrix(ncol = 20, nrow = 0))
rc_3 <- data.frame(matrix(ncol = 20, nrow = 0))
rc_4 <- data.frame(matrix(ncol = 20, nrow = 0))
rc_5 <- data.frame(matrix(ncol = 20, nrow = 0))

# Set the retail_c to a dataframe
retail_c <- as.data.frame(retail_c)

# Move the relevant rows to the 5 new dataframes
rc_1 <- retail_c[retail_c$Classifica == "Local Centre",]
rc_2 <- retail_c[retail_c$Classifica == "Major Town Centre",]
rc_3 <- retail_c[retail_c$Classifica == "Market Town",]
rc_4 <- retail_c[retail_c$Classifica == "Small Local Centre",]
rc_5 <- retail_c[retail_c$Classifica == "Town Centre",]

rc_a <- rbind(rc_1, rc_3)
rc_b <- rbind(rc_2, rc_4, rc_5)

# remove the 5 temporary variables
remove(rc_1, rc_2, rc_3, rc_4, rc_5)
#remove(rc_a, rc_b)

# Split the rc_b file in to 2 leaving rc_b and rc_c
rc_c <- tail(rc_b, -2500)
rc_b = rc_b[!(rc_b$RC_ID %in% rc_c$RC_ID),]

rc_b = rc_c
identical(rc_b, rc_c)
```

```{r rc_a_isoloop}
# create a loop to find the isochrones for all retail centre centroids (20 minute walk time again)
remove(a)
remove(isodf)
remove(rciso)
remove(coordinates)
remove(ranges)
remove(values)
remove(rcwalktime20)
#remove(rca_results)

rca_results <- data.frame(matrix(ncol = 25, nrow = 0))
#############################################################################
i = 0
i = i + 1
#############################################################################
for(i in 1:nrow(rc_a)) {
  # create a dataframe to hold the retail centres isochrone data for retail centres
  rciso <- data.frame(matrix(ncol = 4, nrow = 0))
  # create a df variable to hold the individual retail center row values from i
  isodf = rc_a[i,]
  # embed data in the output file rather than html streaming option
  mapviewOptions(fgb = FALSE)
  #############################################################################
  # Get the coordinates of the store in the (df)
  coordinates <- data.frame(isodf$rccentroidgeom)
  #coordinates <- data.frame(lon = isodf$Lon, lat = isodf$Lat)
  # convert coordinates in to an sf file
  coordinates = st_as_sf(coordinates)
  # convert retail centres sf file to CRS 4326
  coordinates <- st_transform(coordinates, crs = st_crs(4236))
  
  # split the geometry in coordinates to x and y
  # Convert coordinates back to a dataframe
  coordinates <- as.data.frame(st_coordinates(coordinates))
  # rename x and y to Lon and Lat 
  coordinates <- coordinates  |>  rename_at('X', ~'Lon')
  coordinates <- coordinates  |>  rename_at('Y', ~'Lat')
  
  #coordinates <- st_transform(coordinates, crs = st_crs(3857))
  # Calculate a 20 minute walking time isochrone around the store
  rcwalktime20 <- ors_isochrones(coordinates, range = 1200, interval = 1200, 
                                 output = "sf", profile = ors_profile("walking"))
  values <- levels(factor(rcwalktime20$value))
  ranges <- split(rcwalktime20, values)
  ranges <- ranges[rev(values)]
  names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)
  
  # Convert the walktime20 isochrone to the correct CRS
  # USE THIS AS THE ISOCHRONE
  rcwalktime20 <- st_transform(rcwalktime20, crs = st_crs(4326))
  
  # rename the walktime20 geometry column
  rcwalktime20 <- rcwalktime20  |>  rename_at('geometry', ~'walktimegeometry')  
  
  
  # Join the calculated rcwalktime20 data to the rciso data
  #jointoretail_c <- cbind(retail_c, rcwalktime20)
  rcwalktime20 <- st_transform(rcwalktime20, crs = st_crs(3857))
  rcwalktime20 <- cbind(isodf$RC_ID, rcwalktime20)
  rciso = rciso <- rbind(rciso,  rcwalktime20)
  
  # Combine the iso results to the row that was used to make the calculation from the rc_a file
  a <- cbind(isodf, rciso)
  rca_results <- rbind(rca_results, a)
  # remove temp dataframes etc
  remove(a)
  remove(isodf)
  remove(rciso)
  remove(coordinates)
  remove(ranges)
  remove(values)
  remove(rcwalktime20)
  # Print (i) as a counter
  cat(i, "completed\r")
  Sys.sleep(2.5)
}
# convert rciso to crs 4326
#rciso <- st_transform(rciso, crs = st_crs(4326))
# convert retail_c to crs 3857
#rciso <- st_transform(rciso, crs = st_crs(3857))


# Change the name of the centre column to make it identifiable
rca_results <- rca_results  |>  rename_at('center', ~'rcisocenter')

# remove the temporary dataframes
remove(i)
# save the rcb_results and the rc_b as RDS files
saveRDS(obj = rca_results, file = "rca_results.rds")
saveRDS(obj = rc_a, file = "rc_a.rds")
```

```{r rcaiso_testmap}
# Map the retail centre centroid and isochrone
ggplot(rca_results) +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(aes(geometry = walktimegeometry, alpha = "")) +
  geom_sf(aes(geometry = rccentroidgeom))
```

```{r rc_b_isoloop}
# create a loop to find the isochrones for all retail centre centroids (20 minute walk time again)
remove(a)
remove(isodf)
remove(rciso)
remove(coordinates)
remove(ranges)
remove(values)
remove(rcwalktime20)

# Create the rcb dataframe to hold the reults in 
rcb_results <- data.frame(matrix(ncol = 25, nrow = 0))

# Initialise (i)
i = 0

# set (i) to 1
i = i + 1

# Start the loop
for(i in 1:nrow(rc_b)) {
  
  # create a temporary dataframe to hold the retail centres isochrone data for each retail centre
  rciso <- data.frame(matrix(ncol = 4, nrow = 0))
  # create a df variable to hold the individual retail center row values from i
  isodf = rc_b[i,]
  # embed data in the output file rather than html streaming option
  mapviewOptions(fgb = FALSE)
  
  # Get the coordinates of the store in the (df)
  coordinates <- data.frame(isodf$rccentroidgeom)
  
  # convert coordinates in to an sf file
  coordinates = st_as_sf(coordinates)
  # convert retail centres sf file to CRS 4326
  coordinates <- st_transform(coordinates, crs = st_crs(4236))
  
  # split the geometry in coordinates to x and y
  # Convert coordinates back to a dataframe
  coordinates <- as.data.frame(st_coordinates(coordinates))
  # rename x and y to Lon and Lat 
  coordinates <- coordinates  |>  rename_at('X', ~'Lon')
  coordinates <- coordinates  |>  rename_at('Y', ~'Lat')
  
  #coordinates <- st_transform(coordinates, crs = st_crs(3857))
  # Calculate a 20 minute walking time isochrone around the store
  rcwalktime20 <- ors_isochrones(coordinates, range = 1200, interval = 1200, 
                                 output = "sf", profile = ors_profile("walking"))
  values <- levels(factor(rcwalktime20$value))
  ranges <- split(rcwalktime20, values)
  ranges <- ranges[rev(values)]
  names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)
  
  # Convert the walktime20 isochrone to the correct CRS
  # USE THIS AS THE ISOCHRONE
  rcwalktime20 <- st_transform(rcwalktime20, crs = st_crs(4326))
  
  # rename the walktime20 geometry column
  rcwalktime20 <- rcwalktime20  |>  rename_at('geometry', ~'walktimegeometry')  
  
  
  # Join the calculated rcwalktime20 data to the rciso data
  rcwalktime20 <- st_transform(rcwalktime20, crs = st_crs(3857))
  # add the rc_id column to the walktime data
  rcwalktime20 <- cbind(isodf$RC_ID, rcwalktime20)
  # add the full walktime data row to the rciso dataframe 
  rciso = rciso <- rbind(rciso,  rcwalktime20)
  
  # Combine the iso results to the row that was used to make the calculation from the rc_a file
  a <- cbind(isodf, rciso)
  # add the final row the results dataframe for rc_b
  rcb_results <- rbind(rcb_results, a)
  # remove temp dataframes etc
  remove(a)
  remove(isodf)
  remove(rciso)
  remove(coordinates)
  remove(ranges)
  remove(values)
  remove(rcwalktime20)
  # Print (i) as a counter
  cat(i, "completed\r")
  # Wait for 3 seconds before the next iteration
  Sys.sleep(3)
}

# Change the name of the centre column to make it identifiable
rcb_results <- rcb_results  |>  rename_at('center', ~'rcisocenter')

# remove the temporary dataframes
remove(i)

# save the rcb_results and the rc_b as RDS files
saveRDS(obj = rcb_results, file = "rcb_results.rds")
saveRDS(obj = rc_b, file = "rc_b.rds")
```

```{r rc_c_isoloop}
# create a loop to find the isochrones for all retail centre centroids (20 minute walk time again)
remove(a)
remove(isodf)
remove(rciso)
remove(coordinates)
remove(ranges)
remove(values)
remove(rcwalktime20)

# Create the rcb dataframe to hold the reults in 
rcc_results <- data.frame(matrix(ncol = 25, nrow = 0))

# Initialise (i)
i = 0

# set (i) to 1
i = i + 1

# Start the loop
for(i in 1:nrow(rc_c)) {
  # create a dataframe to hold the retail centres isochrone data for retail centres
  rciso <- data.frame(matrix(ncol = 4, nrow = 0))
  # create a df variable to hold the individual retail center row values from i
  isodf = rc_c[i,]
  # embed data in the output file rather than html streaming option
  mapviewOptions(fgb = FALSE)
  
  # Get the coordinates of the store in the (df)
  coordinates <- data.frame(isodf$rccentroidgeom)
  
  # convert coordinates in to an sf file
  coordinates = st_as_sf(coordinates)
  
  # convert retail centres sf file to CRS 4326
  coordinates <- st_transform(coordinates, crs = st_crs(4236))
  
  # split the geometry in coordinates to x and y
  # Convert coordinates back to a dataframe
  coordinates <- as.data.frame(st_coordinates(coordinates))
  # rename x and y to Lon and Lat 
  coordinates <- coordinates  |>  rename_at('X', ~'Lon')
  coordinates <- coordinates  |>  rename_at('Y', ~'Lat')
  
  #coordinates <- st_transform(coordinates, crs = st_crs(3857))
  # Calculate a 20 minute walking time isochrone around the store
  rcwalktime20 <- ors_isochrones(coordinates, range = 1200, interval = 1200, 
                                 output = "sf", profile = ors_profile("walking"))
  values <- levels(factor(rcwalktime20$value))
  ranges <- split(rcwalktime20, values)
  ranges <- ranges[rev(values)]
  names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)
  
  # Convert the walktime20 isochrone to the correct CRS
  # USE THIS AS THE ISOCHRONE
  rcwalktime20 <- st_transform(rcwalktime20, crs = st_crs(4326))
  
  # rename the walktime20 geometry column
  rcwalktime20 <- rcwalktime20  |>  rename_at('geometry', ~'walktimegeometry')  
  
  
  # Join the calculated rcwalktime20 data to the rciso data
  #jointoretail_c <- cbind(retail_c, rcwalktime20)
  rcwalktime20 <- st_transform(rcwalktime20, crs = st_crs(3857))
  rcwalktime20 <- cbind(isodf$RC_ID, rcwalktime20)
  rciso = rciso <- rbind(rciso,  rcwalktime20)
  
  # Combine the iso results to the row that was used to make the calculation from the rc_a file
  a <- cbind(isodf, rciso)
  rcc_results <- rbind(rcc_results, a)
  
  # remove temp dataframes etc
  remove(a)
  remove(isodf)
  remove(rciso)
  remove(coordinates)
  remove(ranges)
  remove(values)
  remove(rcwalktime20)
  
  # Print (i) as a counter
  cat(i, "completed\r")
  Sys.sleep(3)
}

# Change the name of the centre column to make it identifiable
rcc_results <- rcc_results  |>  rename_at('center', ~'rcisocenter')

# remove the temporary dataframes
remove(i)

# save the rcb_results and the rc_b as RDS files
saveRDS(obj = rcc_results, file = "rcc_results.rds")
saveRDS(obj = rc_c, file = "rc_c.rds")
```

```{r combine_rc_results}
rc_results = rc_results <- rbind(rca_results, rcb_results, rcc_results)
names(rc_results)

# Remove the duplicate and not needed columns from the dataframe rc_results
rc_results = rc_results |> select(-one_of('fid.1', 'RC_ID.1', 'RC_Name.1', 'Classifica.1', 'Country.1', 'Region_NM.1', 'H3_count.1', 'H3_count.1', 'Area_km2.1', 'Retail_N.1', 'group_index', 'value', 'isodf.RC_ID', 'fid'))


# Check this worked
names(rc_results)

# Remove the saved dataframes to save memory
remove(rc_a, rc_b, rc_c, rca_results, rcb_results, rcc_results, retail_c, finaldf)

saveRDS(obj = rc_results, file = "rc_results.rds")
```

```{r}
#rc_results1 = rc_results
#rownames(rc_results) <- 1:nrow(rc_results)
#remove(rc_results1)
```

```{r}

# convert the dataframes to sf for plotting
#rc_results = st_as_sf(rc_results)
#zws_results = st_as_sf(zws_results)

# Set the CRS of both to the relevant ones
#rc_results <- st_transform(rc_results, crs = st_crs(3857))
#zws_results <- st_transform(zws_results, crs = st_crs(3857))
#ggplot() +
# Basemap Layer
#  annotation_map_tile(zoomin = 0, type = "osm") +
#  geom_sf(data = rc_results, aes(geometry = rc_results[2,]$walktimegeometry), fill = 0.1)+
#  geom_sf(data = rc_results, aes(geometry = rc_results[2,]$rccentroidgeom))
```

```{r}
# Join the rc_a, rc_b and rc_c dataframes by row to create the final isochrone data for the retail centres containing all rows from all 3
#rc_results <- rbind(rca_results, rcb_results, rcc_results)
# remove rc_a and rc_b
#remove(coordinates, i, rc_a, rc_b, rca_results, rcb_results, rciso)
```

```{r rc_clipping}
# Using the final rc_results data that contains the isochrone calculations, clip these results so that we are left with retail centres that do not contain a zws in them - rc_nozws_results

# convert the rc_results and finaldf files to sf format
rc_results = st_as_sf(rc_results)
zws_results = st_as_sf(zws_results)

# convert them both to the same geometry (matching the retail centres (4326))
rc_results <- st_transform(rc_results, crs = st_crs(4326))
zws_results <- st_transform(zws_results, crs = st_crs(4326))

# Find the retail centres that DO have a zws in them
rc_with_zws = rc_results[zws_results$zwsgeometry,]

# clip the rc_results to only show those retail centres that DO NOT have a zws in them
#Convert them both to a dataframe
rc_results  = as.data.frame(rc_results)
rc_with_zws  = as.data.frame(rc_with_zws)
# Clip rc_results to a new dataframe to only contain the rc's that DO NOT have a zws in them. 
rc_no_zws = rc_results[!(rc_results$RC_ID  %in% rc_with_zws$RC_ID),]
```

```{r nozws_testmap}
ggplot(rc_no_zws[2,]) +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(aes(geometry = walktimegeometry), alpha = 0.1)
```

```{r rc_lsoa_clipping}
# Using all retail centres with NO zero waste store (rc_no_zws); find the lsoa pwc's that sit within the rc isochrones




ggplot() +
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(data = lsoas_in_rcisos[1,], aes(geometry = lsoas_in_rcisos[1,]$lsoageometry, alpha = 0.1)) +
  geom_sf(data = lsoas_in_rcisos[1,], aes(geometry = lsoas_in_rcisos[1,]$pwcgeometry))



# clip the PWCs to only show those inside the 20 minute walk time
lsoa_pwc_clip = lsoa_pwc[rc_no_zws$walktimegeometry,]
# clip the LSOAs to only show those for the clipped PWCs
tlsoa_clip = lsoas[tlsoa_pwc_clip,]
# clip the populations for the LSOAs based on the clipped PWCs
pop_clip = pop_final[lsoa_pwc_clip,]

# Find the lsoapwc's that appear in the walktimegeometry
# Load LSOA population weighted centroids shapefile - LSOA scale
#lsoa_pwc <- read_sf("lsoa_pwc.shp")
# Convert this to the correct 3857 CRS
#lsoa_pwc = st_transform(lsoa_pwc, crs = st_crs(4326))
#lsoa_pwc <- lsoa_pwc  |>  rename_at('geometry', ~'pwcgeometry')

# convert the rc_results and rc_no_zws files to sf format
#rc_results = st_as_sf(rc_results)
#rc_no_zws = st_as_sf(rc_no_zws)
#lsoas = st_as_sf(lsoas)

# convert lsoa_pwc and rc_no_zws to CRS (3857)
#lsoas = st_transform(lsoas, crs = st_crs(3857))
#rc_no_zws = st_transform(rc_no_zws, crs = st_crs(3857))

# clip the lsoas to only show those lsoa_pwcs inside the 20 minute walk time for the retail centres
#nozws_lsoapwc_clip = lsoas[rc_no_zws$walktimegeometry,]
#remove(no_zwslsoa_pwc_clip)
# Clip the lsoa's so that only those pwc are within the isochrones for the retail centres with no zws are shown
#lsoa_clip = lsoas[nozws_lsoapwc_clip$pwcgeometry,]

# remove the lsoas data
#remove(lsoas)
```

```{r rc_join_to_lsoas}

# Find the lsoas that appear in the walktimegeometry for the retail centre
lsoas_in_rcisos = lsoas[rc_no_zws$walktimegeometry,]
#remove(zws_results)

# reindex the rc_no_zws dataframe
rownames(rc_no_zws) <- NULL

# find the lsoa pwc's inside the retail centres isochrones
###################################################################
###########                            ############################
###########       START HERE           ############################
###########                            ############################
###################################################################



# for each individiual retail centre that has no zws, join the lsoas around the retail centre, total the scores for the joined lsoas, and add the scores, joined_lsoa geometry and retaiil centre data to a new dataframe  




# Join the lsoas together
#lsoa_join <- st_union(b$lsoageometry)
# combine the lsoas with pwc's that fall within the rc tisochrones

#############################################################################
remove(a)
remove(b)
remove(c)
remove(joined_lsoas)
remove(zws_results)
remove(rc_results)
remove(rc_with_zws)
remove(join_lsoa_scr_df)
remove(i)
remove(lsoa_clip)
remove(no_zwslsoa_pwc_clip)


# convert lsoa_pwc and rc_no_zws to CRS (3857)
rc_no_zws = st_transform(rc_no_zws, crs = st_crs(3857))
lsoa_clip = st_transform(lsoa_clip, crs = st_crs(3857))
# clip the lsoa_pwc to only show those inside the 20 minute walk time

#initialise i
i = 0
i = i + 1

# Create the joined_lsoa_score dataframe to hold the reults in 
#join_lsoa_scr_df <- data.frame(matrix(ncol = 8, nrow = 0))

# for each row in rc_no_zws find the rows in lsoas_in_rc_isos that have pwcs inside the rc_no_zws isochrone, then calculate scores data for the lsoas found, then the join the lsoa geometry to one shape and score row, then add this to the rc_no_zws data

for (i in 1:nrow(rc_no_zws)) {
  
  # Select the pwcs that appear in the isochrone for that (i) record in rc_no_zws
  a = lsoas_in_rcisos[rc_no_zws$walktimegeometry,]
  b = lsoa_clip[a$pwcgeometry,]
  
  
  if(nrow(b) == 0) {
    next
  }
  # Convert b to a dataframe to manipulate it
  b = as.data.frame(b)
  
  # Create a column for the totals or each individual LSOA in the selecetd area
  b = b |> 
    mutate( %in%  = rowSums(b[,9:13], na.rm = TRUE),
            .after=dedscr)
  
  # create a row in b dataframe for the total for all lsoas in the area and the total of those totals
  b <- b |>
    bind_rows(summarise(b, across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  
  # convert b to sf format
  b = st_as_sf(b)
  # convert b to the correct CRS
  b = st_transform(b, crs = st_crs(3857))
  
  # Join the lsoas together
  joined_lsoas <- st_union(b$lsoageometry)
  
  #convert joined_lsoas to a dataframe
  joined_lsoas <- as.data.frame(joined_lsoas)
  # rename the geometry column
  joined_lsoas <- joined_lsoas  |>  rename_at('geometry', ~'finlsoageometry')
  
  # join the joined geometry to the total scores for the joined lsoas and the rcid from rc_no_zws row i
  c = cbind(rc_no_zws[i,]$RC_ID, tail(b[9:14], n=1), joined_lsoas)
  
  # convert c to a dataframe
  c <- as.data.frame(c)
  
  # delete the empty geometry column
  c = c |> select(-one_of('lsoageometry'))
  
  # Join the results together
  join_lsoa_scr_df <- rbind(join_lsoa_scr_df, c)
  
  #remove(a, b, c, joined_lsoas)
  
  # Print (i) as a counter
  cat(i, "completed\r")
  Sys.sleep(3)
  
}
remove(join_lsoa_scr_df)
remove(i)

joined_lsoas = st_as_sf(joined_lsoas)
joined_lsoas = st_transform(joined_lsoas, crs = st_crs(3857))
c = cbind(rc_no_zws$RC_ID, joined_lsoas)
```

```{r}
lsoa_pwc = st_transform(lsoa_pwc, crs = st_crs(3857))
ggplot() +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(data = lsoa_pwc[11,], aes(geometry = lsoa_pwc[11,]$pwcgeometry), fill = 0.1)
```

```{r}
b = st_transform(b, crs = st_crs(3857))
joined_lsoas = st_transform(joined_lsoas, crs = st_crs(3857))
ggplot() +
  # Basemap Layer
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(data=  joined_lsoas, aes(geometry = finlsoageometry), fill = 0.1)
```

```{r rc_class_count}
# count each occurence of the rc classification type
classdf = aggregate(data.frame(count = rc_clip$Classifica), list(value = rc_clip$Classifica), length)

# Sort the class df descending
classdf[order(-classdf$count), ]
```

```{r startagain}
#############################################################################        LOAD THE DATA TO BE TESTED
###################### Find the retail centres with no zws in them

# convert the rc_results and finaldf files to sf format

rc_results = st_as_sf(rc_results)
zws_results = st_as_sf(zws_results)

# convert them both to the same geometry (matching the retail centres (4326))
rc_results <- st_transform(rc_results, crs = st_crs(4326))
zws_results <- st_transform(zws_results, crs = st_crs(4326))

# Find the retail centres that DO have a zws in them
rc_with_zws = rc_results[zws_results$zwsgeometry,]

# find the retail centres that DO NOT have a zws in them
#Convert them both to a dataframe
rc_results  = as.data.frame(rc_results)
rc_with_zws  = as.data.frame(rc_with_zws)
# rc_no_zws ends up only containing rc's with no zws in them 
rc_no_zws = rc_results[!(rc_results$RC_ID  %in% rc_with_zws$RC_ID),]

# remove the dataframes that are not needed
remove(rc_with_zws)
remove(zws_results)
remove(rc_results)

#############################################################################
################################ Find the lsoas/pwc's for these

# Find all lsoas/pwc's that sit within any walktime20 for any rc
lsoas_pwc = st_as_sf(lsoas_pwc)
rc_no_zws = st_as_sf(rc_no_zws)
lsoas_in_rcs_no_zws <- lsoas_pwc[rc_no_zws$walktimegeometry,] 
remove(lsoas_pwc)

# remove the retail centres that are not needed
rc_no_zws = rc_no_zws <- subset(rc_no_zws, Classifica != 'Major Town Centre
') 

#############################################################################
##For each rc_no_zws find the pws's sitting inside the walktime20 boundary
# get the total scores for each lsoa in this group
# Join the lsoa in to one shape and get the totoal score for this

# convert lsoas_pwc and rc_no_zws to CRS (3857)
rc_no_zws = st_transform(rc_no_zws, crs = st_crs(3857))
lsoas_in_rcs_no_zws = st_transform(lsoas_pwc, crs = st_crs(3857))

#initialise i
i = 0
i = i + 1
for (i in 1:nrow(rc_no_zws)) {
  
  # Select the pwcs that appear in the isochrone for that (i) record in rc_no_zws
  #rename the pwc geometry column in lsoas_in_rcs_no_zws
  lsoas_in_rcs_no_zws <- lsoas_in_rcs_no_zws  |>  rename_at('pwcgeometry', ~'geometry')
  a = lsoas_in_rcs_no_zws[rc_no_zws[1,]$walktimegeometry,]
  
  ############################################################################
  # USE SEPARATE PWC AND LSOA FILES TO DO THE CLIPPING THEN JOIN THEM AFTER CALCULATIONS HAVE BEEN MADE
  ############################################################################
  
  
  
  # Plot a
  rc_no_zws = st_transform(rc_no_zws, crs = st_crs(4326))
  ggplot() +
    annotation_map_tile(zoomin = 0, type = "osm") +
    geom_sf(data = rc_no_zws[i], 
            aes(geometry = rc_no_zws[i,]$walktimegeometry), alpha = 0.1) +
    geom_sf(data = a, aes(geometry = a$lsoageometry, alpha = 0.1)) +
    geom_sf(data = a, aes(geometry = a$pwcgeometry))
  rc_no_zws = st_transform(rc_no_zws, crs = st_crs(3857))
  # select the pwcs that 
  b = lsoas_in_rcs_no_zws[a$pwcgeometry,]
  
  
  if(nrow(b) == 0) {
    next
  }
  # Convert b to a dataframe to manipulate it
  b = as.data.frame(b)
  
  # Create a column for the totals or each individual LSOA in the selecetd area
  b = b |> 
    mutate(totscr = rowSums(b[,9:13], na.rm = TRUE),
           .after=dedscr)
  
  # create a row in b dataframe for the total for all lsoas in the area and the total of those totals
  b <- b |>
    bind_rows(summarise(b, across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  
  # convert b to sf format
  b = st_as_sf(b)
  # convert b to the correct CRS
  b = st_transform(b, crs = st_crs(3857))
  
  # Join the lsoas together
  joined_lsoas <- st_union(b$lsoageometry)
  
  #convert joined_lsoas to a dataframe
  joined_lsoas <- as.data.frame(joined_lsoas)
  # rename the geometry column
  joined_lsoas <- joined_lsoas  |>  rename_at('geometry', ~'finlsoageometry')
  
  # join the joined geometry to the total scores for the joined lsoas and the rcid from rc_no_zws row i
  c = cbind(rc_no_zws[i,]$RC_ID, tail(b[9:14], n=1), joined_lsoas)
  
  # convert c to a dataframe
  c <- as.data.frame(c)
  
  # delete the empty geometry column
  c = c |> select(-one_of('lsoageometry'))
  
  # Join the results together
  join_lsoa_scr_df <- rbind(join_lsoa_scr_df, c)
  
  #remove(a, b, c, joined_lsoas)
  
  # Print (i) as a counter
  cat(i, "completed\r")
  Sys.sleep(3)
  
}


# Join the total scores and joined lsoa shape to the rc data
```

```{r zws_scr_prep}
# Load the data required

# Load Retail Centres data
#rc_results <- readRDS("rc_results.rds")
# create a dataframe to hold the RCID and geometry for the isochrone
#rc = select(rc_results, 2, 13)
# remove rc_results
#remove(rc_results)
# Convert rc to a sf format
#rc = st_as_sf(rc)
# Load Lsoa data
lsoas <- readRDS("lsoas.rds")
# Load Population Weighted Centroid data and zws results
lsoa_pwc <- readRDS("lsoa_pwc.rds")
zws_results_test <- readRDS("zws_results_test.rds")

# remove the scores from zws_test
zws_results_test <- subset(zws_results_test, select = -c(11:16))

# Find all the PWCs that appear in any of the zws_isochrones
pwc_in_zwsiso = lsoa_pwc[zws_results_test$walktimegeometry,]

# find all the pwc_in_rciso related LSOAs
lsoa_in_zwsiso = lsoas[(lsoas$LSOA21CD  %in% pwc_in_zwsiso$LSOA21CD),]

# Keep any retail centres that contain a zws
#rc = st_transform(rc, crs = st_crs(3857))
#zws_results = st_transform(zws_results, crs = st_crs(3857))
#rc_with_zws = rc[zws_results$zwsgeometry,]
#rc_no_zws = rc[!(rc$RC_ID  %in% rc_with_zws$RC_ID),]
#remove(rc_no_zws)
# Save the rc_no_zws data
#saveRDS(obj = rc_with_zws, file = "rc_with_zws.rds")

#zws_results = st_transform(zws_results, crs = st_crs(3857))
#ggplot(data = zws_results[93,]) +
#  annotation_map_tile(zoomin = 0, type = "osm") +
#  geom_sf(aes(geometry = zws_results[93,]$walktimegeometry), alpha = 0.1)

# Create a dataframe to hold all the joined results
zws_joined_lsoa_scr <- data.frame(matrix(ncol = 8, nrow = 0))
```

```{r zws_scr_loop}
# Loop through each retail centre to find the pwcs in pwc_in_rciso related to that retail centre 
#initialise i
i = 0
i = i + 1
for (i in 1:nrow(zws_results_test)) {
  
  # Select the pwcs that appear in the isochrone for that (i) record in rc_no_zws
  
  # get all the pwcs that appear in the iso for that rc
  a = pwc_in_zwsiso[zws_results_test[i,]$walktimegeometry,]
  
  # Check if (a) has 0 rows. If so, sack it off and move to the next record
  if(nrow(a) == 0) {
    next
  }
  
  # Convert a to a dataframe to manipulate it
  a = as.data.frame(a)
  
  # Get all the lsoas that relate to the pwcs just found
  b = lsoas[(lsoas$LSOA21CD %in% a$LSOA21CD),]
  
  #convert b to a dataframe
  b = as.data.frame(b)
  
  # Create a column in b for the totals of each individual LSOA in the selecetd area
  b = b |>
    mutate(totscr = rowSums(b[,9:13], na.rm = TRUE),
           .after=dedscr)
  
  
  ##### create a row in the b dataframe for the total for all lsoas in the area and the total mean of those totals
  #r = nrow(b)
  b <- b |>
    bind_rows(summarise(b[9:14], across(where(is.numeric), mean),
                        across(where(is.character), ~'Total')))
  
  # Join the lsoas to a single shape so that a single line for this shape with the averages score can be used.
  
  # convert b to sf format
  b = st_as_sf(b)
  # convert b to the correct CRS
  b = st_transform(b, crs = st_crs(3857))
  
  # Join the lsoas together
  joined_lsoas <- st_union(b$lsoageometry)
  ## Create a new dataframe  for the joined data
  joined_lsoas_b = cbind(joined_lsoas, b[nrow(b),9:14])
  # delete the blank geometry column
  joined_lsoas_b = subset(joined_lsoas_b, select = -c(lsoageometry))
  # convert both to a dataframe
  b = as.data.frame(b)
  joined_lsoas_b = as.data.frame(joined_lsoas_b)
  
  # retrieve the RCID from rc_no_zws
  d = zws_results_test[i,1]$st_nam
  d = as.data.frame(d)
  e = cbind(d, joined_lsoas_b)
  # Join them
  zws_joined_lsoa_scr <- rbind(zws_joined_lsoa_scr, e)
  
  #ggplot() +
  #  annotation_map_tile(zoomin = 0, type = "osm") +
  #  geom_sf(data = joined_lsoa_scr_rc[i,], aes(geometry = geometry), alpha = 0.1)
  
  # remove the temporary dataframes sf files etc 
  remove(a, b, d, e, joined_lsoas, joined_lsoas_b)
  
  
  # Print (i) as a counter
  cat(i, "completed\r")
  
  #  ggplot() +
  #  annotation_map_tile(zoomin = 0, type = "osm") +
  #  geom_sf(data = zws_joined_lsoa_scr[i,], aes(geometry = geometry), alpha = 0.1)
  #  Sys.sleep(2)
}

# save the retail centres joined lsoas data that contains the average scores for the individua variables and the total meanscore for the whole joined lsoas area.
saveRDS(obj = zws_joined_lsoa_scr, file = "zws_joined_lsoa_scr.rds")
```

```{r rc_with_zws_prep}
# Load the data required

# Load Retail Centres data
rc_results <- readRDS("rc_results.rds")
# create a dataframe to hold the RCID and geometry for the isochrone
rc = select(rc_results, 2, 13)
# remove rc_results
remove(rc_results)
# Convert rc to a sf format
rc = st_as_sf(rc)
# Load Lsoa data
lsoas <- readRDS("lsoas.rds")
# Load Population Weighted Centroid data and zws results
lsoa_pwc <- readRDS("lsoa_pwc.rds")
zws_results <- readRDS("zws_results.rds")
# Find all the PWCs that appear in any of the rc_isochrones
pwc_in_rciso = lsoa_pwc[rc,]

# find all the pwc_in_rciso related LSOAs
lsoa_in_rciso = lsoas[(lsoas$LSOA21CD  %in% pwc_in_rciso$LSOA21CD),]

# Keep any retail centres that contain a zws
rc = st_transform(rc, crs = st_crs(3857))
zws_results = st_transform(zws_results, crs = st_crs(3857))
rc_with_zws = rc[zws_results$zwsgeometry,]
#rc_no_zws = rc[!(rc$RC_ID  %in% rc_with_zws$RC_ID),]
#remove(rc_no_zws)
# Save the rc_no_zws data
#saveRDS(obj = rc_with_zws, file = "rc_with_zws.rds")

#zws_results = st_transform(zws_results, crs = st_crs(3857))
#ggplot(data = zws_results[93,]) +
#  annotation_map_tile(zoomin = 0, type = "osm") +
#  geom_sf(aes(geometry = zws_results[93,]$walktimegeometry), alpha = 0.1)

# Create a dataframe to hold all the joined results
zws_joined_lsoa_scr_rc <- data.frame(matrix(ncol = 8, nrow = 0))
```

```{r rc_with_zws_loop}
# Loop through each retail centre to find the pwcs in pwc_in_rciso related to that retail centre 
#initialise i
i = 0
i = i + 1
for (i in 1:nrow(rc_with_zws)) {
  
  # Select the pwcs that appear in the isochrone for that (i) record in rc_no_zws
  
  # get all the pwcs that appear in the iso for that rc
  a = pwc_in_rciso[rc_with_zws[i,]$walktimegeometry,]
  
  # Check if (a) has 0 rows. If so, sack it off and move to the next record
  if(nrow(a) == 0) {
    next
  }
  
  # Convert a to a dataframe to manipulate it
  a = as.data.frame(a)
  
  # Get all the lsoas that relate to the pwcs just found
  b = lsoas[(lsoas$LSOA21CD %in% a$LSOA21CD),]
  
  #convert b to a dataframe
  b = as.data.frame(b)
  
  # Create a column in b for the totals of each individual LSOA in the selecetd area
  b = b |>
    mutate(totscr = rowSums(b[,9:13], na.rm = TRUE),
           .after=dedscr)
  
  
  ##### create a row in the b dataframe for the total for all lsoas in the area and the total of those totals
  #r = nrow(b)
  b <- b |>
    bind_rows(summarise(b, across(where(is.numeric), mean),
                        across(where(is.character), ~'Total')))
  
  # Join the lsoas to a single shape so that a single line for this shape with the averages score can be used.
  
  # convert b to sf format
  b = st_as_sf(b)
  # convert b to the correct CRS
  b = st_transform(b, crs = st_crs(3857))
  
  # Join the lsoas together
  joined_lsoas <- st_union(b$lsoageometry)
  ## Create a new dataframe  for the joined data
  joined_lsoas_b = cbind(joined_lsoas, b[nrow(b),9:14])
  # delete the blank geometry column
  joined_lsoas_b = subset(joined_lsoas_b, select = -c(lsoageometry))
  # convert both to a dataframe
  b = as.data.frame(b)
  joined_lsoas_b = as.data.frame(joined_lsoas_b)
  
  # retrieve the RCID from rc_no_zws
  d = rc_with_zws[i,1]$RC_ID
  d = as.data.frame(d)
  e = cbind(d, joined_lsoas_b)
  # Join them
  zws_joined_lsoa_scr_rc <- rbind(zws_joined_lsoa_scr_rc, e)
  
  #ggplot() +
  #  annotation_map_tile(zoomin = 0, type = "osm") +
  #  geom_sf(data = joined_lsoa_scr_rc[i,], aes(geometry = geometry), alpha = 0.1)
  
  # remove the temporary dataframes sf files etc 
  remove(a, b, d, e, joined_lsoas, joined_lsoas_b)
  
  
  # Print (i) as a counter
  cat(i, "completed\r")
  #  Sys.sleep(2)
}

# save the retail centres joined lsoas data that contains the average scores for the individua variables and the total meanscore for the whole joined lsoas area.
saveRDS(obj = zws_joined_lsoa_scr_rc, file = "zws_joined_lsoa_scr_rc.rds")
```

```{r rc_no_zws_prep_keep}
# Load the data required

# Load Retail Centres data
#rc_results <- readRDS("rc_results.rds")
# create a dataframe to hold the RCID and geometry for the isochrone
#rc = select(rc_results, 2, 13)
# remove rc_results
#remove(rc_results)
# Convert rc to a sf format
#rc = st_as_sf(rc)
# Load Lsoa data
#lsoas <- readRDS("lsoas.rds")
# Load Population Weighted Centroid data and zws results
#lsoa_pwc <- readRDS("lsoa_pwc.rds")
#zws_results <- readRDS("zws_results.rds")
# Find all the PWCs that appear in any of the rc_isochrones
#pwc_in_rciso = lsoa_pwc[rc,]

# find all the pwc_in_rciso related LSOAs
#lsoa_in_rciso = lsoas[(lsoas$LSOA21CD  %in% pwc_in_rciso$LSOA21CD),]

# Remove any retail centres that contain a zws
#rc = st_transform(rc, crs = st_crs(3857))
#zws_results = st_transform(zws_results, crs = st_crs(3857))
#rc_with_zws = rc[zws_results$zwsgeometry,]
#rc_no_zws = rc[!(rc$RC_ID  %in% rc_with_zws$RC_ID),]
#remove(rc_with_zws)
# Save the rc_no_zws data
#saveRDS(obj = rc_no_zws, file = "rc_no_zws.rds")

#zws_results = st_transform(zws_results, crs = st_crs(3857))
#ggplot(data = zws_results[93,]) +
#  annotation_map_tile(zoomin = 0, type = "osm") +
#  geom_sf(aes(geometry = zws_results[93,]$walktimegeometry), alpha = 0.1)

# Create a dataframe to hold all the joined results
#joined_lsoa_scr_rc <- data.frame(matrix(ncol = 8, nrow = 0))
```

```{r rc_no_zws_rc_loop_keep}
# Loop through each retail centre to find the pwcs in pwc_in_rciso related to that retail centre 
#initialise i
#i = 0
#i = i + 1
#for (i in 1:nrow(rc_no_zws)) {

# Select the pwcs that appear in the isochrone for that (i) record in rc_no_zws

# get all the pwcs that appear in the iso for that rc
#  a = pwc_in_rciso[rc_no_zws[i,]$walktimegeometry,]

# Check if (a) has 0 rows. If so, sack it off and move to the next record
# if(nrow(a) == 0) {
#  next
# }

# Convert a to a dataframe to manipulate it
#  a = as.data.frame(a)

# Get all the lsoas that relate to the pwcs just found
#  b = lsoas[(lsoas$LSOA21CD %in% a$LSOA21CD),]

#convert b to a dataframe
#  b = as.data.frame(b)

# Create a column in b for the totals of each individual LSOA in the selecetd area
#  b = b |>
#  mutate(totscr = rowSums(b[,9:13], na.rm = TRUE),
#        .after=dedscr)


##### create a row in the b dataframe for the total for all lsoas in the area and the total of those totals
#r = nrow(b)
#b <- b |>
#  bind_rows(summarise(b, across(where(is.numeric), mean),
#                                   across(where(is.character), ~'Total')))

# Join the lsoas to a single shape so that a single line for this shape with the averages score can be used.

# convert b to sf format
#b = st_as_sf(b)
# convert b to the correct CRS
#b = st_transform(b, crs = st_crs(3857))

# Join the lsoas together
#joined_lsoas <- st_union(b$lsoageometry)
## Create a new dataframe  for the joined data
#joined_lsoas_b = cbind(joined_lsoas, b[nrow(b),9:14])
# delete the blank geometry column
#joined_lsoas_b = subset(joined_lsoas_b, select = -c(lsoageometry))
# convert both to a dataframe
#b = as.data.frame(b)
#joined_lsoas_b = as.data.frame(joined_lsoas_b)

# retrieve the RCID from rc_no_zws
#d = rc_no_zws[i,1]$RC_ID
#d = as.data.frame(d)
#e = cbind(d, joined_lsoas_b)
# Join them
#joined_lsoa_scr_rc <- rbind(joined_lsoa_scr_rc, e)

#ggplot() +
#  annotation_map_tile(zoomin = 0, type = "osm") +
#  geom_sf(data = joined_lsoa_scr_rc[i,], aes(geometry = geometry), alpha = 0.1)

# remove the temporary dataframes sf files etc 
#remove(a, b, d, e, joined_lsoas, joined_lsoas_b)


# Print (i) as a counter
#  cat(i, "completed\r")
#  Sys.sleep(2)
#}

# save the retail centres joined lsoas data that contains the average scores for the individua variables and the total meanscore for the whole joined lsoas area.
#saveRDS(obj = joined_lsoa_scr_rc, file = "joined_lsoa_scr_rc.rds")
```

```{r score_analysis}
# Load all the data from the retail centres
#rc_results <- readRDS("rc_results.rds")
#remove(rc_results)
# delete the useless 1st column
#rc_results = rc_results |> select(-1)

#joined_lsoa_scr_rc <- readRDS("joined_lsoa_scr_rc.rds")

# rename the d column to RC_ID
#joined_lsoa_scr_rc <- joined_lsoa_scr_rc  |>  rename_at('d', ~'RC_ID')

# Join the data on RCDID
#joined_lsoa_scr_rc <- dplyr::inner_join(joined_lsoa_scr_rc, rc_results, by='RC_ID')

# rename the geometry column to make sense
#joined_lsoa_scr_rc <- joined_lsoa_scr_rc  |>  rename_at('geometry', ~'rcnozwslsoageometry')

# Round the scores in joined_lsoa_scr_rc
#joined_lsoa_scr_rc[,2:7] = round(joined_lsoa_scr_rc[,2:7], digits = 0)


# Save the joined_lsoa_scr_rc data
#saveRDS(obj = joined_lsoa_scr_rc, file = "joined_lsoa_scr_rc.rds")

# Load the final data
joined_lsoa_scr_rc <- readRDS("joined_lsoa_scr_rc.rds")

# Plot the retail centre at (i)
i = 11
ggplot() +
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(aes(geometry = joined_lsoa_scr_rc[i,]$rccentroidgeom, colour="red")) +
  geom_sf(aes(geometry = joined_lsoa_scr_rc[i,]$rcnozwslsoageometry, fill = joined_lsoa_scr_rc[i,]$totscr), alpha = 0.1) +
  geom_sf(aes(geometry = joined_lsoa_scr_rc[i,]$walktimegeometry), alpha = 0.1)

j = 35
ggplot() +
  annotation_map_tile(zoomin = 0, type = "osm") +
  geom_sf(aes(geometry = joined_lsoa_scr_rc[j,]$rccentroidgeom, colour="red")) +
  geom_sf(aes(geometry = joined_lsoa_scr_rc[j,]$rcnozwslsoageometry, fill = joined_lsoa_scr_rc[i,]$totscr), alpha = 0.1) +
  geom_sf(aes(geometry = joined_lsoa_scr_rc[j,]$walktimegeometry), alpha = 0.1)


# Summary of the total scores
summary(joined_lsoa_scr_rc$totscr)

min(joined_lsoa_scr_rc$totscr)
max(joined_lsoa_scr_rc$totscr)
```
