
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggnorthcarolina

<!-- badges: start -->

<!-- badges: end -->

The goal of ggnorthcarolina is to …

## Installation

You can install the development version of ggnorthcarolina from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EvaMaeRey/ggnorthcarolina")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggnorthcarolina)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
library(ggplot2)
ggplot() +
  stamp_sf_countynorthcarolina()
```

<img src="man/figures/README-cars-1.png" width="100%" />

``` r

library(ggplot2)
northcarolina_flat %>%
ggplot() +
aes(fips = FIPS) +
geom_sf_countynorthcarolina()
#> Joining with `by = join_by(fips)`
```

<img src="man/figures/README-cars-2.png" width="100%" />

``` r

last_plot() + 
  aes(fill = BIR74)
#> Joining with `by = join_by(fips)`
```

<img src="man/figures/README-cars-3.png" width="100%" />

``` r


northcarolina_flat %>%
ggplot() +
aes(fips = FIPS, fill = SID74,
    label = paste0(NAME, "\n", SID74)) +
geom_sf_countynorthcarolina() +
geom_label_northcarolina_county(lineheight = .7,
size = 2, check_overlap= TRUE,
color = "oldlace")
#> Joining with `by = join_by(fips)`
#> Joining with `by = join_by(fips)`
```

<img src="man/figures/README-cars-4.png" width="100%" />
