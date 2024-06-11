#############################
# Pottery
#############################


source("scripts/MUK1952_GiteIIB.R")
source("scripts/MUK2018-1030-10.R")

plt.po.1952.qty <- ggplot(po.1952.z.sum, 
                          aes(x = z, 
                              weight = n)) + 
  geom_bar(fill = "#f8766d", 
           color = "black", 
           width = 18) + 
  scale_x_reverse("Depth [cm]", 
                  breaks = c(seq(680, 0, -20)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  scale_y_continuous("Quantity (1952)", 
                     expand = c(0, 0), 
                     limits = c(0, 1.1*max(po.1952.z.sum$n))) + 
  coord_flip() + 
  theme_light() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())


po.2018$n[is.na(po.2018$n)] <- 1

po.2018.geom <- merge(x = po.2018, by.x = "Act",
                      y = excav.2018, by.y = "act")

plt.po.2018.qty <- po.2018.geom %>%
  dplyr::group_by(z) %>%
  dplyr::summarise(Pottery = sum(n)) %>%
  ggplot(aes(x = (z-10), y = Pottery)) + 
  geom_bar(stat = "identity", fill = "#f8766d", color = "black") + 
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) +
  scale_y_continuous("Quantity (2018)", breaks = c(seq(0, 250, 50)),
                     limits = c(0, 290),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  #geom_vline(xintercept = 60, linetype="dashed") + 
  coord_flip() + 
  theme_light() + 
  theme(#axis.title.x = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


plt.po.2018.refit <- ggplot() +
  geom_segment(
    data = po.2018.refit.m2, 
    aes(x = 1, xend = 2,
        y = z.x, yend = z.y, 
        linetype = note), 
    color = "#f8766d") + 
  scale_y_reverse("", 
                  limits = c(660, 0),
                  breaks = seq(660, 0, -20), 
                  expand = c(0, 0)) + 
  guides(linetype = guide_legend(ncol = 1)) + 
  scale_linetype_manual(values = c("solid", "dotted")) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank())


cowplot::plot_grid(
  plt.po.1952.qty, 
  plt.po.2018.qty,
  plt.po.2018.refit, 
  nrow = 1, 
  rel_widths = c(1.1, 1, 1),
  align = 'h', axis = "tb", 
  labels = "AUTO"
)
ggsave("output/Fig_5_MUK1952-2018_Pottery.jpg", width = 6, height = 6)
ggsave("output/fig_5_muk1952-2018_pottery.pdf", width = 6, height = 6)

