##funnel plot comb----
library(magick)
library(cowplot)
library(patchwork)
library(ggplot2)
##funnel_comb----
img10 <- magick::image_read("plot/funnel_richness.png")
img11 <- magick::image_read("plot/funnel_shannon.png")

img13 <- magick::image_read("plot/funnel_homo.png")
img14 <- magick::image_read("plot/funnel_shift.png")
# combine with cowplot (convert to ggdraw)

p10 <- ggdraw() + draw_image(img10)
p11 <- ggdraw() + draw_image(img11)

p13 <- ggdraw() + draw_image(img13)
p14 <- ggdraw() + draw_image(img14)
combined1 <- wrap_plots( p10, p11, p13, p14, ncol = 1)

combined1 + 
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 17, face = "bold"))

##egger_comb----
img10 <- magick::image_read("plot/egger_richness.png")
img11 <- magick::image_read("plot/egger_shannon.png")

img13 <- magick::image_read("plot/egger_homo.png")
img14 <- magick::image_read("plot/egger_shift.png")
# combine with cowplot (convert to ggdraw)

p10 <- ggdraw() + draw_image(img10)
p11 <- ggdraw() + draw_image(img11)

p13 <- ggdraw() + draw_image(img13)
p14 <- ggdraw() + draw_image(img14)
combined1 <- wrap_plots( p10, p11, p13, p14, ncol = 1)

combined1 + 
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 17, face = "bold"))

