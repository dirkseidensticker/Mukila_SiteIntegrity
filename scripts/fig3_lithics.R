#############################
# Lithics
#############################

source("scripts/MUK1952_GiteIIB.R")
source("scripts/MUK2018-1030-10.R")

# c14 dates

plt.c14 <- ggplot(c14.18.calprobdist.norm) + 
  ggridges::geom_ridgeline(data = c14.52.calprobdist.norm, 
                           aes(x = (-calage + 1950)/1000, 
                               y = DEPTH, 
                               height = density, 
                               group = LABNR), 
                           scale = 15, fill = "black", color = NA) + #, fill = "#f5f5f5", color = "grey") + 
  ggridges::geom_ridgeline(aes(x = (-calage + 1950)/1000, 
                               y = DEPTH-2, 
                               height = density, 
                               group = LABNR), 
                           scale = 15, fill = "black", color = NA) + 
  scale_x_continuous(breaks = c(seq(-40, 2, 5)),
                     limits = c(-41.25, 2),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  scale_y_reverse(limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 
  annotate("rect", ymin = 642, ymax = 658, xmin = -Inf, xmax = -40.5, alpha = .1) + 
  annotate("rect", ymin = 642, ymax = Inf, xmin = -39.1, xmax = -40.5, alpha = .15) + 
  annotate("rect", ymin = 445, ymax = Inf, xmin = -36.443, xmax = -34.315, alpha = .15) + 
  annotate("rect", ymin = 445-10, ymax = 445+10, xmin = -Inf, xmax = -34.315, alpha = .15) + 
  annotate("rect", ymin = 302, ymax = 358, xmin = -Inf, xmax = -12.570, alpha = .1) + 
  annotate("rect", ymin = 302, ymax = Inf, xmin = -12.031, xmax = -12.570, alpha = .15) + 
  #annotate("text", y = 330, x = -13500, label = "VIII") + 
  annotate("rect", ymin = 222, ymax = 298, xmin = -Inf, xmax = -9.355, alpha = .1) + 
  annotate("rect", ymin = 222, ymax = Inf, xmin = -9.890, xmax = -9.355, alpha = .15) + 
  #annotate("text", y = 270, x = -13500, label = "VII") + 
  #annotate("text", y = 220, x = -13500, label = "VI") + 
  annotate("rect", ymin = 162, ymax = 178, xmin = -Inf, xmax = -3.014, alpha = .1) + 
  annotate("rect", ymin = 162, ymax = Inf, xmin = -3.341, xmax = -3.014, alpha = .15) + 
  #annotate("text", y = 180, x = -13500, label = "V") + 
  annotate("rect", ymin = 102, ymax = 158, xmin = -Inf, xmax = -1.453, alpha = .1) + 
  annotate("rect", ymin = 102, ymax = Inf, xmin = -1.771, xmax = -1.453, alpha = .15) + 
  #annotate("text", y = 130, x = -13500, label = "IV") + 
  annotate("rect", ymin = 62, ymax = 98, xmin = -Inf, xmax = .116, alpha = .1) + 
  annotate("rect", ymin = 62, ymax = Inf, xmin = -.352, xmax = .116, alpha = .15) + 
  #annotate("text", y = 70, x = -13500, label = "II") + 
  annotate("rect", ymin = 42, ymax = 58, xmin = -Inf, xmax = 1.633, alpha = .1) + 
  annotate("rect", ymin = 42, ymax = Inf, xmin = 1.326, xmax = 1.633, alpha = .15) + 
  #annotate("text", y = 50, x = -13500, label = "I") + 
  scale_size(range = c(0, 6)) + 
  #geom_hline(yintercept = 60, linetype="dashed") + 
  #geom_hline(yintercept = 80, linetype="dashed") + 
  #geom_hline(yintercept = 100, linetype="dashed") + 
  #geom_hline(yintercept = 160, linetype="dashed") + 
  #geom_hline(yintercept = 200, linetype="dashed") + 
  #geom_hline(yintercept = 240, linetype="dashed") + 
  #geom_hline(yintercept = 300, linetype="dashed") + 
  labs(y = "Depth cm", x = "Age [k yrs cal BCE/CE]") + 
  theme_light() + 
  theme(legend.position = "none", 
        panel.grid.minor.y = element_line(color = "grey80"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# combined plot

plt <- cowplot::plot_grid(
  plt.c14, 
  plt.li.1952.qty, 
  plt.li.2018.qty,
  nrow = 1,
  rel_widths = c(1, 1, .5),
  align = 'h', axis = "tb", 
  labels = "AUTO")

ggsave("output/Fig_3_MUK1952-2018_Lithics.jpg", width = 16/1.5, height = 10/1.5)
ggsave("output/fig_3_muk1952-2018_lithics.pdf", width = 16/1.5, height = 10/1.5)