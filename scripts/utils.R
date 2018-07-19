
x = cnty_irr['nwis_value']
to = clipped_hucs

st_interpolate_cw <- function (x, to, variable) {
  if (!inherits(to, "sf") && !inherits(to, "sfc")) 
    stop("st_interpolate_aw requires geometries in argument to")
  if (!all_constant(x)) 
    warning("st_interpolate_aw assumes attributes are constant over areas of x")
  i = st_intersection(st_geometry(x), st_geometry(to))
  idx = attr(i, "idx")
  i = st_cast(i, "MULTIPOLYGON")
  x$...value_s = x[,variable]
  st_geometry(x) = NULL
  x = x[idx[, 1], ]
  x$...value_st = i[,variable]
  target = sapply(split(st_area(i), idx[, 2]), sum)
  df = data.frame(area = target, idx = as.integer(names(target)))
  x = lapply(x, function(v) v * x$...area_st/x$...area_s)
  x = aggregate(x, list(idx[, 2]), sum)
  df = st_sf(x, geometry = st_geometry(to)[x$Group.1])
  df$...area_t = df$...area_st = df$...area_s = NULL
  st_agr(df) = "aggregate"
  df
}

library(velox)
library(raster)

# load population data
pop <- velox('data/gridded_pop/clipped_pop.tif')

# population by county
cnty_pop <- pop$extract(st_transform(counties_acf,pop$crs),fun=function(x) sum(x,na.rm=T),df=TRUE)

counties_acf$pop <- cnty_pop$out

mapview(counties_acf, zcol="pop")

# population by partial hucs
cnty_huc <- counties_acf %>%
  dplyr::select(cnty_fips=GEOID,cnty_pop = pop) %>%
  st_intersection(clipped_hucs) %>%
  st_cast('MULTIPOLYGON') %>%
  st_transform(pop$crs)

mapview(cnty_huc_pop)

huc_pop <- pop$extract(cnty_huc,fun=function(x) sum(x,na.rm=T),df=TRUE)

cnty_huc_pop <- cnty_huc %>%
  dplyr::mutate(huc_pop = huc_pop$out,
                p = round(huc_pop/cnty_pop,3))






