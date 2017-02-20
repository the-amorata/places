library(ggplot2)

region_string <- function(ll, lvl) {
  if (lvl == 'county') {
    paste(ll$administrative_area_level_1, 
          gsub(' County$', '', ll$administrative_area_level_2), 
          sep = ',')
  } else {
    ll$administrative_area_level_1
  }
}

plot_geo <- function(ll, lvl, hex, lt, ps) {
  m = map_data(lvl, region = region_string(ll, lvl))
  p = ggplot() + 
      geom_polygon(data=m, aes(x=long, y=lat, group=group), colour=hex, fill=NA, size = lt) +
      geom_point(data=ll, aes(x=lon, y=lat), colour=hex, size = ps) +
      coord_map()
  if (hex == '#FFFFFF') {p = p + blank_theme_w} else {p = p + blank_theme}
  p
}

blank_theme = theme(title = element_blank()
      ,plot.background = element_blank()
      ,panel.background = element_blank()
      ,legend.key = element_blank()
      ,legend.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_blank()
      ,axis.text = element_blank()
      ,axis.ticks = element_blank())

blank_theme_w <- theme(
  title = element_blank()
  ,plot.background = element_rect(fill = "grey")
  ,panel.background = element_rect(fill = "grey")
  ,legend.key = element_blank()
  ,legend.background = element_rect(fill = "grey")
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
  ,panel.border = element_blank()
  ,axis.text = element_blank()
  ,axis.ticks = element_blank()
  ,text = element_blank()
)