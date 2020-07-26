library(tidyverse)

c("#adff2f", "#9acd32") %>% scales::show_col()


c("#adff2f", "#9acd32") %>% col2rgb() %>% 
  as.data.frame() %>% 
  set_names(c("GrYel", "YelGr")) %>% 
  mutate(Channel = c("R", "G", "B")) %>% 
  mutate(Gr10Y10 = (10*GrYel+10*YelGr)/20) %>% 
  pivot_longer(-Channel, names_to = "color") %>% 
  pivot_wider(names_from = Channel) %>% 
  rowwise() %>% 
  mutate(hex = rgb(R, G, B, maxColorValue = 255)) %>% 
  ungroup() -> col_df

col_df %>% 
  filter(color == "Gr10Y10") %>% 
  ggplot(aes(x=1, y=1)) +
  geom_raster(aes(fill = hex)) +
  scale_fill_identity() + 
  coord_cartesian(expand = FALSE) +
  theme_void()

ggsave("GY_A3E630.png", width = 6, height = 3)


c("#adff2f", "#9acd32") %>% col2rgb() %>% rgb2hsv() %>% 
  as.data.frame() %>% 
  set_names(c("GrYel", "YelGr")) %>% 
  mutate(Channel = c("H", "S", "V")) %>% 
  mutate(Gr10Y10 = (10*GrYel+10*YelGr)/20) %>% 
  pivot_longer(-Channel, names_to = "color") %>% 
  pivot_wider(names_from = Channel) %>% 
  rowwise() %>% 
  mutate(hex = hsv(H, S, V)) %>% 
  ungroup() -> hsv_df


#Differences

#What color do we combine with Green to get Yellow Green
"#9acd32"
c("#00ff00", "#9acd32") %>% col2rgb() %>% rgb2hsv() %>% 
  as.data.frame() %>% 
  set_names(c("Green", "YelGr")) %>% 
  mutate(Channel = c("h", "s", "v")) %>%
  select(Channel, everything()) %>% 
  mutate(Yellow_calc = YelGr + (YelGr - Green)) %>% 
  pivot_longer(-Channel, names_to = "color") %>% 
  pivot_wider(names_from = Channel) %>% 
  rowwise() %>% 
  mutate(hex = hsv(h, s, v)) %>% 
  ungroup() 

rgb(255, 128, 82, maxColorValue = 255) %>% scales::show_col()
"#9B804C" %>% scales::show_col()


crossing(x = 1:10, y = 1:10, color = "#ffffff") %>% 
  # bind_rows() %>% 
  ggplot(aes(x, y, group = color)) +
  geom_raster(data = crossing(x = 1:10, y = 1:10, color = "#00ff00"),
              aes(fill = color, alpha = y/10)) + 
  geom_raster(data = crossing(x = 1:10, y = 1:10, color = "#9B804C") %>% 
                mutate(alpha = x/10),
              aes(fill = color, alpha = alpha)) +
  geom_raster(data = crossing(x = 1:10, y = 7, color = "#9acd32"),
               aes(fill = color), alpha = 1) +
  scale_fill_identity() +
  scale_alpha_identity() +
  coord_fixed() +
  theme_void()


"#9acd32" %>% scales::show_col()




c("#00ff00", "#ff8052") %>% col2rgb() %>% 
  as.data.frame() %>% 
  set_names(c("Green", "Added")) %>% 
  mutate(Channel = c("R", "G", "B")) %>% 
  mutate(YelGr = ((.4529307)*Green+(1-0.4529307)*Added)) %>% 
  pivot_longer(-Channel, names_to = "color") %>% 
  pivot_wider(names_from = Channel) %>% 
  rowwise() %>% 
  mutate(hex = rgb(R, G, B, maxColorValue = 255)) %>% 
  ungroup() %>% 
  filter(color == "YelGr") %>% 
  pull(hex) %>% 
  scales::show_col()

