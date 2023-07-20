# step 00 reference data
# northcarolina_county_centers <- data.frame(     x   =  -81.49496,   y = 36.42112,  county_name = "Ashe",   fips = "37009")


# step 1
#' Title
#'
#' @param data
#' @param scales
#' @param keep_county
#'
#' @return
#' @export
#'
#' @examples
#' northcarolina_flat |>
#'   dplyr::rename(fips = FIPS) |>
#'   dplyr::rename(label = NAME) |>
#'   compute_panel_county_centers()
compute_panel_county_centers <- function(data,
                                         scales,
                                         keep_county = NULL){

  northcarolina_county_centers_filtered <- northcarolina_county_centers

  if(!is.null(keep_county)){
    keep_county %>% tolower() -> keep_county

    northcarolina_county_centers_filtered %>%
      dplyr::filter(.data$county_name %>%
                      tolower() %in%
                      keep_county) ->
      northcarolina_county_centers_filtered}

  data %>%
    dplyr::inner_join(northcarolina_county_centers_filtered) %>%
    dplyr::select(x, y, label)

}




# step 2 proto
StatCountycenters <- ggplot2::ggproto(
  `_class` = "StatRownumber",
  `_inherit` = ggplot2::Stat,
  # required_aes = c("label"), # for some reason this breaks things... why?
  compute_panel = compute_panel_county_centers
)

#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' northcarolina_flat %>%
#'  ggplot() +
#'  aes(fips = FIPS, label = NAME) +
#'  geom_label_northcarolina_county()
#'
#' northcarolina_flat %>%
#'  ggplot() +
#'  aes(fips = FIPS, label = NAME) +
#'  geom_sf_countynorthcarolina() +
#'  geom_label_northcarolina_county()
#'
#'  northcarolina_flat %>%
#'  ggplot() +
#'  aes(fips = FIPS, label = SID74, fill = SID74) +
#'  geom_sf_countynorthcarolina() +
#'  geom_label_northcarolina_county(color = "oldlace")
#'
#'  northcarolina_flat %>%
#'  ggplot() +
#'  aes(fips = FIPS, fill = SID74,
#'      label = paste0(NAME, "\n", SID74)) +
#'  geom_sf_countynorthcarolina() +
#'  geom_label_northcarolina_county(lineheight = .7,
#'  size = 2, check_overlap= TRUE,
#'  color = "oldlace")
geom_label_northcarolina_county <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatCountycenters,  # proto object from Step 2
    geom = ggplot2::GeomText,  # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

