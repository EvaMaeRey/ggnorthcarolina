## code to prepare `DATASET` dataset goes here


###### 0. Read in shape file data  #####

library(sf)
northcarolina_county_sf <- sf::st_read(system.file("shape/nc.shp", package="sf")) |>
  dplyr::rename(county_name = NAME,
                fips = FIPS)

### save as is if desired #####
usethis::use_data(northcarolina_county_sf, overwrite = TRUE)


#### 1, create polygon reference dataframe w xmin, ymin, xmax and ymax and save
return_st_bbox_df <- function(sf_df){

  data.frame(xmin = sf::st_bbox(sf_df)[1],
             xmax = sf::st_bbox(sf_df)[2],
             ymin = sf::st_bbox(sf_df)[3],
             ymax = sf::st_bbox(sf_df)[4])

}


northcarolina_county_reference <- northcarolina_county_sf |>
  dplyr::select(county_name, fips, geometry) |>
  dplyr::mutate(bb =
                  purrr::map(geometry, st_bbox_df)) |>
  tidyr::unnest(bb)


# northcarolina_county_reference <- northcarolina_county_sf |>
#   ggnc::create_geometries_reference(
#                             id_cols = c(county_name, fips))

usethis::use_data(northcarolina_county_reference, overwrite = TRUE)


####### 2. create and save flat file for examples, if desired ####

northcarolina_county_sf %>%
  sf::st_drop_geometry() ->
northcarolina_county_flat

usethis::use_data(northcarolina_county_flat, overwrite = TRUE)

############### 3. create polygon centers and labels reference data frame

# county centers for labeling polygons

northcarolina_county_centers <- northcarolina_county_sf |>
  ggnc::prepare_polygon_labeling_data(id_cols = c(county_name, fips))


usethis::use_data(northcarolina_county_centers, overwrite = TRUE)
