#####################
# Discussion Figure #
#####################

source("scripts/MUK1952_GiteIIB.R")
source("scripts/MUK2018-1030-10.R")

# c14 dates ----
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
  
  geom_hline(yintercept = 60, linetype="dashed") + 
  geom_hline(yintercept = 100, linetype="dashed") + 
  geom_hline(yintercept = 140, linetype="dashed") + 
  geom_hline(yintercept = 200, linetype="dashed") + 
  geom_hline(yintercept = 240, linetype="dashed") + 
  geom_hline(yintercept = 300, linetype="dashed") + 
  
  labs(y = "Depth cm", x = "Age [k yrs cal BCE/CE]") + 
  theme_light() + 
  theme(legend.position = "none", 
        panel.grid.minor.y = element_line(color = "grey80"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

li.2018.refit.edges <- data.table::fread("input/MUK2018_1030_10_lithics_refits.csv", encoding = "UTF-8") %>%
  dplyr::mutate(type = factor(type, levels = c("Sequential", "Direct")))

# charcoal species ----


# refits ----
plt.refit.seq <- ggplot() + 
  geom_segment(data = li.1952.refit.edges %>% 
                 dplyr::filter(type %in% c("Sequential", "Direct")) %>%
                 dplyr::mutate(type = factor(type, levels = c("Direct", "Sequential"))), 
               aes(x = x.A, xend = x.B, 
                   y = z1, yend = z2,
                   linetype = type),
               color = "#619cff") + 
  geom_segment(data = li.2018.refit.edges %>%
                 dplyr::rename(z1 = z.FROM, 
                               z2 = z.TO), 
               aes(x = 0, xend = 1, 
                   y = z1, yend = z2,
                   linetype = type),
               color = "#619cff") + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_x_continuous("") + 
  scale_y_reverse("", 
                  limits = c(660, 0),
                  breaks = seq(660, 0, -20), 
                  expand = c(0, 0)) + 
  geom_hline(yintercept = 60, linetype="dashed") + 
  geom_hline(yintercept = 100, linetype="dashed") + 
  geom_hline(yintercept = 140, linetype="dashed") + 
  geom_hline(yintercept = 200, linetype="dashed") + 
  geom_hline(yintercept = 240, linetype="dashed") + 
  geom_hline(yintercept = 300, linetype="dashed") + 
  
  guides(linetype = guide_legend(reverse = TRUE, ncol = 1)) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank(), 
        #axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank())

# more general grainsizes ----

gs.2018 <- read.csv("input/MUK2018_1030_10_GrainSizes.csv")
#gs.2018 <- filter(gs.2018, X <= 360)

gs.2018 <- reshape2::melt(gs.2018[,c("X", "Sand..", "Silt..", "Clay..")], id.vars = "X")

#gs.2018$variable <- factor(gs.2018$variable , levels=c("Clay..", "Silt..", "Sand..") )
gs.2018$variable <- factor(gs.2018$variable , levels=c("Sand..", "Silt..", "Clay..") )

plt.gs.2018 <- ggplot(gs.2018 %>% 
         dplyr::mutate(variable = factor(variable, levels = c("Sand..", "Silt..", "Clay.."))), 
       aes(x = X, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 20, color = "grey") +
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 
  scale_y_continuous("Soil\nTexture\n[%]", 
                     limits = c(0, 100), 
                     expand = c(0, 0)) + 
  #scale_fill_viridis(option="plasma", discrete=TRUE) + 
  scale_fill_manual("", values = c("navajowhite2", "lightsteelblue2", "plum2"),
                    labels = c("Sand", "Silt", "Clay")) + 
  #scale_fill_brewer("", palette = "Accent", 
  #                  labels = c("Clay", "Silt", "Sand")) + 
  coord_flip() + 
  theme_light() + 
  
  geom_vline(xintercept = 60, linetype="dashed") + 
  geom_vline(xintercept = 100, linetype="dashed") + 
  geom_vline(xintercept = 140, linetype="dashed") + 
  geom_vline(xintercept = 200, linetype="dashed") + 
  geom_vline(xintercept = 240, linetype="dashed") + 
  geom_vline(xintercept = 300, linetype="dashed") + 

  theme(axis.text.y = element_blank(), 
        legend.position = "bottom", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(fill=guide_legend(ncol = 1))

# combined plot
plt <- cowplot::plot_grid(
  plt.c14, 
  plt.ch.2018.species,
  plt.refit.seq,
  plt.gs.2018,
  nrow = 1, 
  rel_widths = c(2, 2, 1, .5),
  align = 'h', axis = "tb", 
  labels = "AUTO")

ggsave("output/Fig_7_MUK_Overview.jpg", width = 16/1.25, height = 10/1.25)
ggsave("output/fig_7_muk_overview.pdf", width = 16/1.25, height = 10/1.25)
