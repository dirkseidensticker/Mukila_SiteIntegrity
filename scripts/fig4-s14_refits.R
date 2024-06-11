
source("scripts/MUK1952_GiteIIB.R")
source("scripts/MUK2018-1030-10.R")


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
  geom_hline(yintercept = 120, linetype="dashed") + 
  geom_hline(yintercept = 200, linetype="dashed") + 
  geom_hline(yintercept = 240, linetype="dashed") + 
  geom_hline(yintercept = 300, linetype="dashed") + 
  
  labs(y = "Depth cm", x = "Age [k yrs cal BCE/CE]") + 
  theme_light() + 
  theme(legend.position = "none", 
        panel.grid.minor.y = element_line(color = "grey80"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


li.1952.refit.edges <- data.table::fread("input/MUK52_lithicrefits.csv") %>%
  dplyr::mutate(FROM = gsub("\\-.*", "", ID.Museum.A), 
                TO = gsub("\\-.*", "", ID.Museum.B)) %>%
  dplyr::rename(InvNo = FROM) %>%
  dplyr::left_join(li.1952.m.z %>% dplyr::select(c(InvNo, z)) %>% dplyr::distinct(InvNo, z), by = "InvNo") %>%
  dplyr::rename(FROM = InvNo, 
                z1 = z) %>%
  dplyr::rename(InvNo = TO) %>%
  dplyr::left_join(li.1952.m.z %>% dplyr::select(c(InvNo, z)) %>% dplyr::distinct(InvNo, z), by = "InvNo") %>%
  dplyr::rename(TO = InvNo, 
                z2 = z) %>%
  dplyr::mutate(z.delta = abs(z1 - z2))

xlsx::write.xlsx(li.1952.refit.edges, 
                 file = "output/lithics1952_refit_edges.xlsx", 
                 row.names = F)

plt.li.1952.refit <- ggplot(li.1952.refit.edges %>% 
         dplyr::filter(type != "Modern") %>%
         #dplyr::filter(!FigS9 %in% c("10", "12")) %>%
         dplyr::mutate(type = factor(type, levels = c("Sequential", "Direct"))), 
                            aes(x = x.A, xend = x.B, 
                                y = z1, yend = z2, 
                                linetype = type)) +
  geom_segment(color = "#619cff") + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + 
  scale_x_continuous("1952") + 
  scale_y_reverse("", 
                  limits = c(660, 0),
                  breaks = seq(660, 0, -20), 
                  expand = c(0, 0)) + 
  geom_hline(yintercept = 60, linetype="dashed") + 
  geom_hline(yintercept = 100, linetype="dashed") + 
  geom_hline(yintercept = 120, linetype="dashed") + 
  geom_hline(yintercept = 200, linetype="dashed") + 
  geom_hline(yintercept = 240, linetype="dashed") + 
  geom_hline(yintercept = 300, linetype="dashed") + 
  
  guides(linetype = guide_legend(ncol = 1)) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank(), 
        #axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank())


li.2018.refit.edges <- data.table::fread("input/MUK2018_1030_10_lithics_refits.csv", encoding = "UTF-8") %>%
  dplyr::mutate(type = factor(type, levels = c("Sequential", "Direct")))


plt.li.2018.refit <- ggplot(li.2018.refit.edges, 
                            aes(x = 1, xend = 2, 
                                y = z.FROM-10, yend = z.TO-10, 
                                linetype = type)) +
  geom_segment(color = "#619cff") + 
  scale_linetype_manual(values = c("solid", "dashed")) + 
  scale_x_continuous("2018") + 
  scale_y_reverse("", 
                  limits = c(660, 0),
                  breaks = seq(660, 0, -20), 
                  expand = c(0, 0)) + 
  geom_hline(yintercept = 60, linetype="dashed") + 
  geom_hline(yintercept = 100, linetype="dashed") + 
  geom_hline(yintercept = 120, linetype="dashed") + 
  geom_hline(yintercept = 200, linetype="dashed") + 
  geom_hline(yintercept = 240, linetype="dashed") + 
  geom_hline(yintercept = 300, linetype="dashed") + 
  
  guides(linetype = guide_legend(ncol = 1)) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank(), 
        #axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank())
  

plt.refit.multseq <- ggplot() + 
  geom_segment(data = li.1952.refit.edges %>% 
                 dplyr::filter(type == "Sequential" & FigS9 %in% c("10", "12")), 
               aes(x = x.A, xend = x.B, 
                   y = z1, yend = z2,
                   linetype = type), 
             #curvature = .2, 
             arrow = arrow(length = unit(0.04, "npc"), type = "closed"),
             color = "#619cff") + 
  scale_linetype_manual(values = c("solid", "dashed")) + 
  scale_x_continuous("") + 
  scale_y_reverse("", 
                  limits = c(660, 0),
                  breaks = seq(660, 0, -20), 
                  expand = c(0, 0)) + 
  geom_hline(yintercept = 60, linetype="dashed") + 
  geom_hline(yintercept = 100, linetype="dashed") + 
  geom_hline(yintercept = 120, linetype="dashed") + 
  geom_hline(yintercept = 200, linetype="dashed") + 
  geom_hline(yintercept = 240, linetype="dashed") + 
  geom_hline(yintercept = 300, linetype="dashed") + 
  
  guides(linetype = guide_legend(ncol = 1)) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank(), 
        #axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "none", 
        legend.title = element_blank())


plt.li <- ggplot() + 
  geom_bar(data = li.1952.m.z.sum %>%
             dplyr::mutate(variable = as.character(variable)) %>%
             dplyr::mutate(variable = replace(variable, variable == "2-8 cm", "> 2 cm")) %>%
             dplyr::mutate(variable = replace(variable, variable == ">= 8 cm", "> 2 cm")) %>%
             dplyr::mutate(variable = factor(variable, levels = c("> 2 cm", "< 2 cm"))), 
           aes(x = z, weight = value * -1, fill = variable),
           color = "black") + 
  geom_bar(data = li.2018.qty %>% 
             dplyr::rename(z = DEPTH) %>% 
             dplyr::mutate(value = value) %>%
             dplyr::mutate(variable = factor(variable, levels = c("> 2 cm", "< 2 cm"))), 
           aes(x = (z-10), y = value, fill = variable), 
           stat = "identity", color = "black") + 
  scale_fill_manual("", values = c("#619cff", "#d9e6fa"), 
                    guide = guide_legend(reverse = TRUE)) + 
  scale_x_reverse("", 
                  breaks = c(seq(660, 0, -20)), 
                  limits = c(660, 0),
                  expand = c(0, 0)) + 
  geom_hline(yintercept = 0) + 
  scale_y_continuous("Quantity", 
                     breaks = seq(-600, 100, 100),
                     labels = abs(seq(-600, 100, 100))) + 
  #scale_y_continuous("Quantity", 
  #                   expand = c(0, 0), 
  #                   limits = c(0, 1.05*max(li.1952.m.z.sum.max$n))) + 
  scale_alpha_manual(values = c(.25, 1)) + #
  
  annotate("text", x = 20, y = -500, label = "1952") + 
  annotate("text", x = 20, y = 50, label = "2018") + 
  
  geom_vline(xintercept = 60, linetype="dashed") + 
  geom_vline(xintercept = 100, linetype="dashed") + 
  geom_vline(xintercept = 120, linetype="dashed") + 
  geom_vline(xintercept = 200, linetype="dashed") + 
  geom_vline(xintercept = 240, linetype="dashed") + 
  geom_vline(xintercept = 300, linetype="dashed") + 
  
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

cowplot::plot_grid(
  plt.c14 + 
    geom_hline(yintercept = 60, linetype="dashed") + 
    geom_hline(yintercept = 100, linetype="dashed") + 
    geom_hline(yintercept = 120, linetype="dashed") + 
    geom_hline(yintercept = 200, linetype="dashed") + 
    geom_hline(yintercept = 240, linetype="dashed") + 
    geom_hline(yintercept = 300, linetype="dashed"),
  plt.li.1952.refit, 
  plt.li.2018.refit,
  plt.refit.multseq,
  plt.li,
  nrow = 1,
  labels = "AUTO", 
  align = 'h', axis = "tb", 
  rel_widths = c(2, 1, 1, 1, 2)
)
ggsave("output/Fig_S8_MUK_refitsOverview.jpg", width = 16, height = 10)
ggsave("output/fig_s8_muk_refitsoverview.pdf", width = 16, height = 10)


p1 <- ggplot(li.1952.refit.edges %>%  
         dplyr::filter(type != "Modern") %>%
         dplyr::mutate(type = factor(type, levels = c("Sequential", "Direct"))), 
       aes(x = x.A, xend = x.B, 
           y = z1, yend = z2, 
           linetype = type)) +
  geom_segment(color = "#619cff") + 
  scale_linetype_manual(values = c("solid", "dashed")) + 
  theme_light() +
  theme(legend.position = "top",
        legend.title = element_blank())

legend = cowplot::get_plot_component(p1, 'guide-box-top', return_all = TRUE)

cowplot::plot_grid(
  cowplot::plot_grid(
    ggplot(li.1952.refit.edges %>%  
             dplyr::filter(type != "Modern") %>%
             dplyr::mutate(type = factor(type, levels = c("Sequential", "Direct"))), 
           aes(x = x.A, xend = x.B, 
               y = z1, yend = z2, 
               linetype = type)) +
      geom_segment(color = "#619cff") + 
      scale_linetype_manual(values = c("solid", "dashed")) + 
      scale_x_continuous("1952") + 
      scale_y_reverse("", 
                      limits = c(660, 0),
                      breaks = seq(660, 0, -20), 
                      expand = c(0, 0)) + 
      #guides(linetype = guide_legend(ncol = 1)) + 
      theme_light() + 
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.grid.major.x = element_blank(), 
            legend.position = "none"), 
    ggplot(li.2018.refit.edges, 
           aes(x = 1, xend = 2, 
               y = z.FROM-10, yend = z.TO-10, 
               linetype = type)) +
      geom_segment(color = "#619cff") + 
      scale_linetype_manual(values = c("solid", "dashed")) + 
      scale_x_continuous("2018") + 
      scale_y_reverse("", 
                      limits = c(660, 0),
                      breaks = seq(660, 0, -20), 
                      expand = c(0, 0)) + 
      guides(linetype = guide_legend(ncol = 1)) + 
      theme_light() + 
      theme(axis.ticks.x = element_blank(),
            axis.text = element_blank(), 
            #axis.title.x = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.grid.major.x = element_blank(), 
            legend.position = "none"),
    nrow = 1,
    labels = "AUTO", 
    align = 'h', axis = "tb", 
    rel_widths = c(1.1, 1)
  ),
  legend, 
  ncol = 1, 
  rel_heights = c(10,1)
)
ggsave("output/Fig_4_MUK_refits.jpg", width = 6, height = 6)
ggsave("output/fig_4_muk_refits.pdf", width = 6, height = 6)


# text ---

# make all inv no unique (even if not given in raw data)

li.1952.refit.edges$ID.Museum.A <- ifelse(
  grepl("-", li.1952.refit.edges$ID.Museum.A), 
  li.1952.refit.edges$ID.Museum.A, 
  paste0(li.1952.refit.edges$ID.Museum.A, ".", as.numeric(rownames(li.1952.refit.edges))))

li.1952.refit.edges$ID.Museum.B <- ifelse(
  grepl("-", li.1952.refit.edges$ID.Museum.B), 
  li.1952.refit.edges$ID.Museum.B, 
  paste0(li.1952.refit.edges$ID.Museum.B, ".", (nrow(li.1952.refit.edges) + as.numeric(rownames(li.1952.refit.edges)))))

# number of objects refitted
c(li.1952.refit.edges$ID.Museum.A, li.1952.refit.edges$ID.Museum.B) %>%
  unique() %>%
  length()

# % of direct vs. sequential refits
li.1952.refit.edges %>%
  dplyr::filter(type != "Modern") %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(pct = paste(round(n / sum(n) * 100), "%"))


li.1952.refit.edges %>% 
  dplyr::arrange(desc(z.delta))

# 5 of refits with less than 20 cm vertical delta
li.1952.refit.edges %>%
  dplyr::filter(z.delta <= 20) %>%
  nrow() / nrow(li.1952.refit.edges) * 100
