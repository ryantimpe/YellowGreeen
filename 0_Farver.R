library(tidyverse)
library(farver)


gycodes = list(
  green       = "#00ff00",
  yellowgreen = "#9acd32",
  greenyellow = "#adff2f",
  yellow      = "#ffff00"
)

unlist(gycodes)

# Color distance -----
gycodes[c("yellowgreen", "greenyellow")] %>% unlist() %>% 
  decode_colour(to = "lab") 

#Euclidean distance between YG and GY

gycodes[c("yellowgreen", "greenyellow")] %>% unlist() %>% 
  decode_colour(to = "rgb") %>% 
  compare_colour(from_space = "rgb")

# Is Green-yellow actually yellow? Is Yellow-green actually green?  

decode_colour(unlist(gycodes)) %>% 
  compare_colour(from_space = "rgb",
                 method = "cie2000")


decode_colour(unlist(gycodes), to = "lab")

gy_distances = function(space, method){
  decode_colour(unlist(gycodes)[c("greenyellow", "yellowgreen")]) %>% 
    compare_colour(to = decode_colour(unlist(gycodes)[c("green", "yellow")]),
                   from_space = space,
                   method = method) %>% 
    as.data.frame() %>% 
    mutate(target_color = rownames(.),
           space = space, 
           method = method) %>% 
    select(space, method, target_color, everything())
}
gy_distances("lab", "euclidean")

crossing(space = c("rgb", "lab", "hsl"), method = c("euclidean", "cie2000")) %>% 
  purrr::pmap_dfr(gy_distances) %>% 
  pivot_longer(c(green, yellow), names_to = "matched_color", values_to = "distance") %>% 
  group_by(target_color, method, space) %>% 
  filter(distance == min(distance)) %>% 
  ungroup() %>% 
  # pivot_wider(names_from = method, values_from = distance) %>% 
  mutate(true_color = str_extract(target_color, "green$|yellow$"),
         match = matched_color == true_color) %>% filter(match) 

count(target_color, true_color, match) %>% 
  spread(match, n)

#For blog, try just rgb lab hsl, then try them all?

# HSL Space ----
decode_colour(unlist(gycodes), to = "hsl") %>% 
  as.data.frame() %>% 
  mutate(color_name = row.names(.),
         hex = unlist(gycodes))

#HSL with hue in between YG & GY, but the saturation & lightness of Y and G
tibble(h=81, s=100, l=50) %>% 
  encode_colour(from = "hsl") %>% 
  scales::show_col()

#In hsl space, only hue separates Yellow & Green
# But YG differs in hue & saturation
# and GY differs in hue & lightness

#lab space 
plot_color_space_single = function(space, this_x, this_y){
  decode_colour(unlist(gycodes), to = space) %>% 
    as.data.frame() %>% 
    mutate(color_name = row.names(.),
           hex = unlist(gycodes)) -> dc_space
  
  dc_space %>% 
    ggplot(aes(x=.data[[this_x]], y=.data[[this_y]])) +
    geom_line(data = dc_space %>% filter(color_name %in% c("green", "yellow"))) +
    geom_point(color = "black", size = 4) +
    geom_point(aes(color = hex), size = 3) +
    scale_color_identity() +
    ggrepel::geom_text_repel(aes(label = color_name), vjust=0, nudge_y = 3)
}
#will work for at least lab, hsl, rgb
plot_color_space = function(space, image){
  strsplit(space, "") %>% unlist() -> space_char
  
  image_plot <- ggplot() +
    ggpubr::background_image(png::readPNG(image))
  
  list(
    plot_color_space_single(space, space_char[1], space_char[2]),
    plot_color_space_single(space, space_char[3], space_char[2]),
    image_plot,
    plot_color_space_single(space, space_char[3], space_char[1])
  ) %>% 
    patchwork::wrap_plots(design = "AB
                                    CD")
}

plot_color_space("lab", "lab_3d.png")

#3d scatter
library(magrittr)
library(rgl)
space = "lab"

rgl.open()

rgl.bg(color = "white") 

decode_colour(unlist(gycodes), to = "lab") %>% 
  as.data.frame()  %>% 
  mutate(color_name = row.names(.),
         hex = unlist(gycodes)) %$% 
rgl::rgl.spheres(l, a, b, r = 5,
                color = hex, back = "lines")
rgl.bbox(xlab = "l", ylab = "a", zlab = "b", 
         expand = 1.5,
         xunit = 10, yunit = 10, zunit = 10,
         color=c("#999999","black"), shininess=5, alpha=0.8)

rgl.snapshot("lab_3d.png")
#Every color in between YG and GY ----
space = "rgb"

decode_colour(unlist(gycodes), to = space) %>% 
  as.data.frame() %>% 
  mutate(color_name = row.names(.),
         hex = unlist(gycodes)) %>% 
  filter(color_name %in% c("greenyellow", "yellowgreen")) -> yg_gy_edges


crossing(s1 = seq(yg_gy_edges[1, 1], to = yg_gy_edges[2, 1], length.out = 10),
         s2 = seq(yg_gy_edges[1, 2], to = yg_gy_edges[2, 2], length.out = 10),
         s3 = seq(yg_gy_edges[1, 3], to = yg_gy_edges[2, 3], length.out = 10)) %>% 
  mutate(hex = encode_colour(., from = space)) -> full_surface

full_surface %>% 
  pull(hex) %>% 
  scales::show_col(labels = F)

#PLot 3d
# convert into a matrix
full_surface %>% 
  pull(s3) %>% 
  
  rgl::rgl.open()
rgl::rgl.surface(full_surface$s1, full_surface$s2, full_surface$s3, 
                 color = full_surface$hex, back = "lines")

rgl::rgl.points(full_surface$s1, full_surface$s2, full_surface$s3, 
                color = full_surface$hex, back = "lines")
