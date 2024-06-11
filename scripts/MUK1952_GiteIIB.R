########################
# MUK 1952
########################

source("scripts/header.R")

# Datasets ----

# Inventory of the collection (only lithics)
li.1952.coll <- data.table::fread("input/MUK52_RMCAcollection.csv",
                                  encoding = "UTF-8") %>%
  dplyr::filter(!is.na(SQUARE))

li.1952.coll$N[is.na(li.1952.coll$N)] <- 1

# list of finds in the inventory by square
li.1952.max.depth <- data.frame(
  max.depth = tapply(
    dplyr::filter(
      li.1952.coll, 
      !is.na(li.1952.coll$DEPTH))$DEPTH, 
    dplyr::filter(
      li.1952.coll, 
      !is.na(li.1952.coll$DEPTH))$SQUARE, max)) %>%
  dplyr::arrange(desc(max.depth))

# Lithics data KJ ----
li.1952 <- data.table::fread("input/MUK52_lithics.csv", encoding = "UTF-8")
li.1952$InvNo <- gsub("\\-.*", "", li.1952$ID.Museum.Ind)

# RECONSTRUCTING Z-VALUES ----
li.1952$X.depth. <- gsub(",", ".", li.1952$X.depth.)
li.1952$X.depth. <- as.numeric(li.1952$X.depth.)

li.1952$depth <- gsub(",", ".", li.1952$depth)
li.1952$depth <- as.numeric(li.1952$depth)

li.1952.depth.comp <- li.1952 %>%
  dplyr::group_by(InvNo) %>%
  dplyr::summarise(depth_refp = mean(X.depth., na.rm = T),
                   depth_surf = mean(depth, na.rm = T))

# inv no  with both z values:
li.1952.depth.comp %>%
  tidyr::drop_na() %>%
  dplyr::mutate(div = depth_surf - depth_refp)

# number records for depth below reference point (in parenthesis):
length(li.1952.depth.comp$depth_refp[!is.na(li.1952.depth.comp$depth_refp)])
# number of records for depth below surface (not in parenthesis):
length(li.1952.depth.comp$depth_surf[!is.na(li.1952.depth.comp$depth_surf)])

# z-values
inv.1952.z <- li.1952.depth.comp %>%
  dplyr::mutate(depth_refp_corr = depth_refp + .42) %>% # correcting 42 cm offset for depths below reference point (cf. little card)
  dplyr::select(-depth_refp)

# in case only the depth below surface was give, use this (after correction) as depth
inv.1952.z[is.na(inv.1952.z$depth_surf),]$depth_surf <- inv.1952.z[is.na(inv.1952.z$depth_surf),]$depth_refp_corr

inv.1952.z <- inv.1952.z %>% 
  dplyr::mutate(z = depth_surf) %>%
  dplyr::select(-c(depth_surf, depth_refp_corr))

# InvNo with _no_ known z-value:
inv.1952.z %>%
  dplyr::filter(is.na(z))

# InvNo with known z-values:
inv.1952.z <- inv.1952.z %>%
  dplyr::filter(!is.na(z))

xlsx::write.xlsx(inv.1952.z %>% as.data.frame(), 
                 file = "output/lithics1952_depth_comp.xlsx", 
                 row.names = F)

# LITHICS ----

# check depths:
li.1952 %>%
  dplyr::left_join(inv.1952.z, by = "InvNo") %>%
  dplyr::select(c(ID.Museum.Ind, InvNo, X.depth., depth, z)) # %>% View()


# m square (studied in detail)
li.1952.m.z <- li.1952 %>%
  dplyr::left_join(inv.1952.z, by = "InvNo") %>%
  dplyr::filter(!is.na(z)) %>%
  dplyr::arrange(desc(z))

li.1952.m.z$z <- as.integer(-100 * li.1952.m.z$z) # convert into positive meters

# li.1952.m.z$z <- li.1952.m.z$z + 26 # correct depths for 1952 (M) surface being 0.26 cm below 2018 (1a-d) surface

li.1952.m.z$z.class <- cut(li.1952.m.z$z, breaks = z.bins)

# classify size (<2cm vs. >2cm)
li.1952.m.z[li.1952.m.z$length == "0-2","size.class"] <- "< 2 cm"
li.1952.m.z[li.1952.m.z$length == "2-4" | li.1952.m.z$length == "4-8", "size.class"] <- "2-8 cm"

li.1952.m.z$length <- sub(",", ".", li.1952.m.z$length) # convert comma decimal separator
li.1952.m.z$length <- as.numeric(li.1952.m.z$length)

li.1952.m.z[!is.na(li.1952.m.z$length) & li.1952.m.z$length < 2, "size.class"] <- "< 2 cm"
li.1952.m.z[!is.na(li.1952.m.z$length) & li.1952.m.z$length >= 2 & li.1952.m.z$length < 8, "size.class"] <- "2-8 cm"
li.1952.m.z[!is.na(li.1952.m.z$length) & li.1952.m.z$length >= 8, "size.class"] <- ">= 8 cm"

li.1952.m.z.sum <- li.1952.m.z %>%
  reshape2::dcast(z.class ~ size.class)

li.1952.m.z.sum <- cbind(
  li.1952.m.z.sum, 
  z = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", li.1952.m.z.sum$z.class)) - 10) # extract upper break point as representation for lower end of the class (cf. system in 2018 excavation in spits)

li.1952.m.z.sum <- li.1952.m.z.sum %>%
  reshape2::melt(id.vars = c("z.class", "z"))

# Lithics depth ----

li.1952.m.z.sum.max <- li.1952.m.z.sum %>%
  dplyr::group_by(z) %>%
  dplyr::summarise(n = sum(value))

li.1952.m.z.sum$variable <- factor(li.1952.m.z.sum$variable, levels = c(">= 8 cm", "2-8 cm", "< 2 cm"))

plt.li.1952.qty <- ggplot(li.1952.m.z.sum, 
         aes(x = z, weight = value, fill = variable)) + 
  geom_bar(color = "black", linewidth = .25) + 
  scale_fill_manual("", values = c("#00255f", "#619cff", "#d9e6fa"), 
                    guide = guide_legend(reverse = TRUE)) + 
  scale_x_reverse("", 
                  breaks = c(seq(680, 0, -20)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  scale_y_continuous("Quantity", 
                     expand = c(0, 0), 
                     limits = c(0, 1.05*max(li.1952.m.z.sum.max$n))) + 
  scale_alpha_manual(values = c(.25, 1)) + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

# lithics refiting ----

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

plt.li.1952.refit <- ggplot(li.1952.refit.edges %>% dplyr::mutate(type = factor(type, levels = c("Sequential", "Direct", "Modern"))), 
       aes(x = 1, xend = 2, 
           y = z1, yend = z2, 
           linetype = type)) +
  geom_segment(color = "#619cff") + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) + 
  scale_y_reverse("", 
                  limits = c(680, 0),
                  breaks = seq(680, 0, -20), 
                  expand = c(0, 0)) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        legend.position = "left", 
        legend.title = element_blank())



# plot only those with different z values:
ggplot(li.1952.refit.edges %>% 
         dplyr::filter(z.delta != 0 & type == "Sequential") %>%
         tibble::rownames_to_column("x") %>% 
         dplyr::arrange(z1) %>%
         dplyr::mutate(x = factor(x, levels = x)), 
       aes(x = x, xend = x, 
           y = z1, yend = z2)) + 
  geom_segment(color = "#619cff", size = 2) + 
  scale_y_reverse("", 
                  limits = c(680, 0),
                  breaks = seq(680, 0, -20), 
                  expand = c(0, 0)) + 
  theme_light() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())




# lithic raw material distribution ----

li.1952.rawmaterial.z <- data.table::fread("input/MUK52_lithicrawmaterial.csv", encoding = "UTF-8") %>%
  dplyr::mutate_at("QTY", as.numeric) %>%
  dplyr::mutate_at("INVNO", as.character) %>%
  dplyr::rename(InvNo = INVNO) %>%
  dplyr::left_join(li.1952.m.z %>% dplyr::select(c(InvNo, z)) %>% dplyr::distinct(InvNo, z), 
                   by = "InvNo") %>%
  dplyr::arrange(RAWMATERIAL, z)

xlsx::write.xlsx2(li.1952.rawmaterial.z, "output/lithics1952_rawmaterial_depth.xlsx", 
                  row.names = FALSE)

li.1952.rawmaterial.z.sum <- li.1952.rawmaterial.z %>%
  dplyr::group_by(RAWMATERIAL, z) %>%
  dplyr::summarise(n = sum(QTY))

#li.1952.rawmaterial.z.sum$z <- as.integer(-100 * li.1952.rawmaterial.z.sum$z) # convert into postive meter
li.1952.rawmaterial.z.sum$z.class <- cut(li.1952.rawmaterial.z.sum$z, breaks = z.bins)

li.1952.rawmaterial.z.sum <- cbind(
  li.1952.rawmaterial.z.sum, 
  z2 = as.integer( sub("[^,]*,([^]]*)\\]", "\\1", li.1952.rawmaterial.z.sum$z.class)) - 10) # extract upper break point as representation for lower end of the class (cf. system in 2018 excavation in spits)

ggplot(li.1952.rawmaterial.z.sum, 
       aes(x = z2, weight = n)) + 
  geom_bar(width = 18) + 
  scale_x_reverse("", 
                  breaks = c(seq(700, 0, -100)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  coord_flip() + 
  geom_vline(xintercept = c(seq(680, 0, -20)), color = "lightgrey", linetype = "dotted") + 
  facet_grid(. ~ RAWMATERIAL, 
             scales = "free") + 
  theme_minimal_hgrid() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#ggsave("output/lithics1952_rawmaterial_depth.jpg", width = 16, height = 4, bg = "white")

ggplot(li.1952.rawmaterial.z.sum %>%
         dplyr::group_by(RAWMATERIAL, z2) %>%
         dplyr::summarise(n = sum(n, na.rm = T)), 
       aes(x = RAWMATERIAL, y = z2, size = n)) +
  geom_hline(yintercept = c(seq(680, 20, -20)), color = "lightgrey", linetype = "dotted") + 
  geom_point(shape = 21, fill = "grey25") + 
  scale_x_discrete(position = "top") + 
  scale_y_reverse("", 
                  breaks = c(seq(700, 0, -100)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  scale_size("Quantity", 
             range = c(2, 15), 
             breaks = c(1, 5, 10, 50, 150)) + 
  guides(size = guide_legend(nrow = 1)) + 
  theme_minimal_hgrid() + 
  theme(legend.position = "none", 
        #legend.justification = "center", 
        axis.title = element_blank(), 
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# seriation ----
library(seriation)
li.1952.rawmaterial.wide <- li.1952.rawmaterial.z.sum %>%
  dplyr::group_by(RAWMATERIAL, z2) %>%
  dplyr::summarise(n = sum(n, na.rm = T)) %>%
  reshape2::dcast(RAWMATERIAL ~ z2, value.var = "n", fun.aggregate = sum) %>%
  tibble::column_to_rownames("RAWMATERIAL")

library(seriation)

row_orders <- seriation::seriate(as.matrix(li.1952.rawmaterial.wide), 
                                 method = "CA", margin = 1) %>%
  get_order(1) %>% 
  sort()

#seriation::bertinplot(as.matrix(li.1952.rawmaterial.wide),
#                      order = row_orders)

row_order_lst_manual <- c("H", "M", "I", "C", "L", "O", "K", "P", "D", "T", "Q", "E", "G", "N", "R", "S", "J", "A", "F", "B")

li.1952.rawmaterial.wide %>% 
  dplyr::select(-"NA") %>%
  tibble::rownames_to_column("RAWMATERIAL") %>%
  reshape2::melt() %>%
  dplyr::mutate(value = dplyr::na_if(value, 0)) %>%
  #dplyr::na_if(0) %>% 
  dplyr::mutate(RAWMATERIAL = factor(RAWMATERIAL, levels = rev(row_order_lst_manual))) %>%
  dplyr::mutate(variable = as.numeric(as.character(variable))) %>%
  ggplot(aes(x = RAWMATERIAL, y = variable, size = value)) +
  geom_point(shape = 21, fill = "grey25") + 
  scale_y_reverse("", 
                  #breaks = c(seq(680, 0, -20)), 
                  #limits = c(680, 0),
                  expand = c(0, 0)) + 
  scale_size("Quantity", 
             range = c(2, 15), 
             breaks = c(1, 5, 10, 50, 150)) + 
  guides(size = guide_legend(nrow = 1)) + 
  theme_minimal_hgrid() + 
  theme(legend.position = "top", 
        legend.justification = "center")
#ggsave("output/lithics1952_rawmaterial_depth1_seriate.jpg", width = 8, height = 9, bg = "white")

ggplot(li.1952.rawmaterial.z.sum, 
       aes(x = z2, weight = n)) + 
  geom_bar(width = 18) + 
  scale_x_reverse("", 
                  breaks = c(seq(680, 0, -20)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  coord_flip() + 
  facet_grid(. ~ RAWMATERIAL, 
             scales = "free", space = "free") + 
  theme_minimal_hgrid()
#ggsave("output/lithics1952_rawmaterial_depth2.jpg", width = 16, height = 9, bg = "white")


# correspondance analysis ----
li.1952.rawmaterial.ca.res <- li.1952.rawmaterial.wide %>% 
  dplyr::select(-"NA") %>%
  FactoMineR::CA(graph = F)

factoextra::fviz_contrib(li.1952.rawmaterial.ca.res)

factoextra::fviz_ca(li.1952.rawmaterial.ca.res, 
                    repel = T)

# POTTERY  ----

po.1952 <- data.table::fread("input/MUK52_pottery.csv", encoding = "UTF-8")
po.1952$N[is.na(po.1952$N)] <- 1
po.1952 <- dplyr::filter(po.1952, Trench == "II" & Quadrant != "B")
#po.1952 <- dplyr::filter(po.1952, Quadrant == "M")

# po.1952$z <- li.1952.m.z$z + 26 # correct depths for 1952 (M) surface being 0.26 cm below 2018 (1a-d) surface
# po.1952$Depth..cm. <- po.1952$Depth..cm. + 26

po.1952$z.class <- cut(po.1952$Depth..cm., breaks = z.bins)

po.1952.z.sum <- po.1952 %>% 
  dplyr::group_by(z.class) %>%
  dplyr::summarise(n = sum(N))


po.1952.z.sum <- cbind(
  po.1952.z.sum, 
  z = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", po.1952.z.sum$z.class)) - 10) # see above

plt.po.1952.qty <- ggplot(po.1952.z.sum, 
       aes(x = z, 
           weight = n)) + 
  geom_bar(fill = "#f8766d", 
           color = "black", 
           width = 18) + 
  scale_x_reverse("", 
                  breaks = c(seq(680, 0, -20)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  scale_y_continuous("Quantity", 
                     expand = c(0, 0), 
                     limits = c(0, 1.1*max(po.1952.z.sum$n))) + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

# charcoals ----

ch.1952 <- data.frame(
  InvNo = c("60768", "60741", "60704", "60775", "60712", "60743(J-F)*", "60764*", "60774(J-F)*"), 
  DEPTH = c(-4.64, -1.91, -4.95, -1.09, -1.97, -1.95, -4.13, -1.5), # attention: these values are un-corrected!!!
  CLASS = c(0, 1, 1, 0, 0, 0, 0, 0))

ch.1952$DEPTH <- (ch.1952$DEPTH * 100)*-1

plt.1952.ch <- ggplot(ch.1952, 
       aes(x = 0, 
           y = DEPTH, 
           color = CLASS)) + 
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = InvNo), 
                           min.segment.length = 0) + 
  scale_y_reverse("Depth [cm]", 
                  limits = c(680, 0),
                  breaks = seq(680, -20, -20), 
                  expand = c(0, 0)) + 
  theme_light() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

# c14 ----
c14 <- data.table::fread("input/MUK_C14.csv") %>%
  dplyr::filter(LABNR != '/' & YEAR == 2018) %>%
  dplyr::select(DEPTH, LABNR, C14AGE, C14STD) %>%
  dplyr::rename(c14age = C14AGE, 
                c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = c("calprobdistr", 
                                   "calrange"), 
                       sigma = 2)

c14.calprobdist <- c14 %>% 
  tidyr::unnest(cols = c("calprobdistr"))

# normalize to 100% for max Age
labnr <- unique(c14$LABNR)
calprobdist.lst <- list()
for(i in 1:length(labnr)){
  calprobdist.sel <- c14.calprobdist %>% 
    dplyr::filter(LABNR == labnr[[i]])
  
  calprobdist.sel$density <- calprobdist.sel$density / max(calprobdist.sel$density, na.rm = TRUE)
  calprobdist.lst[[i]] <- calprobdist.sel
}
c14.18.calprobdist.norm <- do.call(rbind, calprobdist.lst)

#c14.calrange <- c14 %>%
#  tidyr::unnest(cols = c("calrange"))

c14 <- data.table::fread("input/MUK_C14.csv") %>%
  dplyr::filter(LABNR != '/' & YEAR == 1952) %>%
  dplyr::select(DEPTH, LABNR, C14AGE, C14STD) %>%
  dplyr::rename(c14age = C14AGE, 
                c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = c("calprobdistr", 
                                   "calrange"), 
                       sigma = 2)

c14.calprobdist <- c14 %>% 
  tidyr::unnest(cols = c("calprobdistr"))

# normalize to 100% for max Age
labnr <- unique(c14$LABNR)
calprobdist.lst <- list()
for(i in 1:length(labnr)){
  calprobdist.sel <- c14.calprobdist %>% 
    dplyr::filter(LABNR == labnr[[i]])
  
  calprobdist.sel$density <- calprobdist.sel$density / max(calprobdist.sel$density, na.rm = TRUE)
  calprobdist.lst[[i]] <- calprobdist.sel
}
c14.52.calprobdist.norm <- do.call(rbind, calprobdist.lst)


plt.c14 <- ggplot(c14.52.calprobdist.norm) + 
  ggridges::geom_ridgeline(data = c14.18.calprobdist.norm, 
                           aes(x = (-calage + 1950)/1000, 
                               y = DEPTH-2, 
                               height = density, 
                               group = LABNR), 
                           scale = 15, fill = "#f5f5f5", color = "grey") + 
  ggridges::geom_ridgeline(aes(x = (-calage + 1950)/1000, 
                               y = DEPTH, 
                               height = density, 
                               group = LABNR), 
                           scale = 15) + 
  scale_x_continuous(breaks = c(seq(-40, 2, 5)),
                     limits = c(-41.25, 2),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  scale_y_reverse(limits = c(680, 0), # 40 
                  breaks = c(seq(0, 680, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 
  annotate("rect", ymin = 66, ymax = Inf, xmin = 1.452, xmax = 1.624, alpha = .15) + 
  annotate("rect", ymin = 66-10, ymax = 66+10, xmin = -Inf, xmax = 1.624, alpha = .15) + 
  annotate("rect", ymin = 144, ymax = Inf, xmin = -3.483, xmax = -3.098, alpha = .15) + 
  annotate("rect", ymin = 144-10, ymax = 144+10, xmin = -Inf, xmax = -3.098, alpha = .15) + 
  annotate("rect", ymin = 445, ymax = Inf, xmin = -36.443, xmax = -34.315, alpha = .15) + 
  annotate("rect", ymin = 445-10, ymax = 445+10, xmin = -Inf, xmax = -34.315, alpha = .15) + 
  
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

# combined plot ----

plt <- cowplot::plot_grid(plt.c14, #plt.1952.ch,
                          plt.li.1952.qty, 
                          plt.li.1952.refit,
                          plt.po.1952.qty, 
                          labels = LETTERS[1:4], 
                          nrow = 1, 
                          align = 'h', axis = "tb", 
                          rel_widths = c(1.5, 2, 0.5, 1))
#ggsave("output/Fig_MUK1952.jpg", plt, width = 16, height = 10)

ggplot(li.1952.m.z.sum, 
                          aes(x = z, weight = value, fill = variable)) + 
  geom_bar(data = po.1952.z.sum, 
           aes(x = z, 
               weight = n), 
           fill = "#f8766d",
           color = "black", 
           width = 18) + 
  geom_bar(color = "black") + 
  scale_fill_manual("", values = c("#00255f", "#619cff", "#d9e6fa"), 
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_reverse("", 
                  breaks = c(seq(680, 0, -20)), 
                  limits = c(680, 0),
                  expand = c(0, 0)) + 
  scale_y_continuous("Quantity", 
                     expand = c(0, 0), 
                     limits = c(0, 1.05*max(li.1952.m.z.sum.max$n))) + 
  scale_alpha_manual(values = c(.25, 1)) + 
  coord_flip() + 
  theme_light() + 
  theme(#axis.text.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank())

# horizontal distribution ----

muk1952_trenches <- geojsonsf::geojson_sf("gis/MUK1952_trenches.geojson")
st_crs(muk1952_trenches) = 32733

# create artificial layers where to cut the vertical distribution
spits <- seq(0, 700, 50)

# LITHICS
li.1952.coll$SQUARE[li.1952.coll$SQUARE == "x"] <- "M" # attribute all x's (unknown squares) to square M

li.1952.coll[!is.na(li.1952.coll$Z..m.) & is.na(li.1952.coll$DEPTH),]$DEPTH <- li.1952.coll[!is.na(li.1952.coll$Z..m.) & is.na(li.1952.coll$DEPTH),]$Z..m.*-100 - 42 # obfl in quadrant M laut Bequaert bei 0,42 unter messpkt

as.data.frame(tapply(filter(li.1952.coll, !is.na(li.1952.coll$DEPTH))$DEPTH, filter(li.1952.coll, !is.na(li.1952.coll$DEPTH))$SQUARE, max))

li.1952.coll$spit <- cut(li.1952.coll$DEPTH, spits)

li.1952.z.sqr <- reshape2::dcast(
  li.1952.coll[,c("spit", "SQUARE", "N")], 
  spit ~ SQUARE, 
  value.var = "N",
  fun.aggregate = sum)

li.1952.z.sqr <- reshape2::melt(li.1952.z.sqr, id.vars = "spit")
li.1952.z.sqr$TYPE <- "LITHICS"

# POTTERY ----


po.1952$spit <- cut(po.1952$Depth..cm., spits)

po.1952.z.sqr <- reshape2::dcast(
  po.1952[,c("spit", "Quadrant", "N")], 
  spit ~ Quadrant, 
  value.var = "N",
  fun.aggregate = sum)
po.1952.z.sqr <- reshape2::melt(po.1952.z.sqr, id.vars = "spit")
po.1952.z.sqr$TYPE <- "POTTERY"

z.1952.sqr <- rbind(li.1952.z.sqr, po.1952.z.sqr)
z.1952.sqr <- z.1952.sqr[!z.1952.sqr$value == 0,] # remove rows where sum was 0
z.1952.sqr <- z.1952.sqr[!is.na(z.1952.sqr$spit),] # remove NA's in spit -- why are there some? should not be the case ( __TODO__ )

excav.1952.vdist <- merge(x = muk1952_trenches,
                          y = z.1952.sqr, 
                          by.x = "code",
                          by.y = "variable",
                          all.x = TRUE)
excav.1952.vdist

li.1952.z.sqr <- li.1952.z.sqr[!li.1952.z.sqr$value == 0,] # remove rows where sum was 0
li.1952.z.sqr <- li.1952.z.sqr[!is.na(li.1952.z.sqr$spit),] # remove NA's in spit -- why are there some? should not be the case ( __TODO__ )

li.1952.vdist <- merge(x = muk1952_trenches,
                          y = li.1952.z.sqr, 
                          by.x = "code",
                          by.y = "variable",
                          all.x = TRUE)

po.1952.z.sqr <- po.1952.z.sqr[!po.1952.z.sqr$value == 0,] # remove rows where sum was 0
po.1952.z.sqr <- po.1952.z.sqr[!is.na(po.1952.z.sqr$spit),] # remove NA's in spit -- why are there some? should not be the case ( __TODO__ )

po.1952.vdist <- merge(x = muk1952_trenches,
                       y = po.1952.z.sqr, 
                       by.x = "code",
                       by.y = "variable",
                       all.x = TRUE)

# BEQUAERT 1956 p. 35f.
bequaert1956p35f <- data.table::fread(
  "input/bequaert1956p35f.csv", 
  encoding = "UTF-8")

bequaert1956p35f$z <- (bequaert1956p35f$from + bequaert1956p35f$to) / 2

bequaert1956p35f <- cbind(
  bequaert1956p35f[,c(11:22)],
  bequaert1956p35f[,c("z")]
)

bequaert1956p35f.lng <- bequaert1956p35f %>% 
  reshape2::melt(id.vars = "z") %>%
  dplyr::filter(value == "x")

bequaert1956p35f.lng$variable <- gsub("N.", "N'", bequaert1956p35f.lng$variable)


bequaert1956p35f.lng$z.class <- cut(bequaert1956p35f.lng$z*100, breaks = spits)

# merging with centeroids of trenches:
bequaert1956p35f.sf <- muk1952_trenches %>%
  sf::st_centroid() %>%
  dplyr::left_join(
    bequaert1956p35f.lng %>%
      dplyr::rename(code = variable)) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::rename(spit = z.class)

# plot
logscale_breaks = c(1, 10, 100, 1000)


# define at what depth only square M was excavated? +/- 350 cm it seems

cutoff <- 300
muk1952.trenches.upper.lst <- list()
for (i in 1:length(spits[spits < cutoff])) {
  muk1952.trenches.upper.lst[[i]] <- dplyr::filter(muk1952_trenches, !is.na(code)) %>%
    dplyr::mutate(DEPTH = spits[i]+25)
}
muk1952.trenches.upper <- do.call(rbind, muk1952.trenches.upper.lst)

muk1952.trenches.lower.lst <- list()
s <- head(spits[spits >= cutoff],-1)
for (i in 1:length(s)) {
  muk1952.trenches.lower.lst[[i]] <- dplyr::filter(muk1952_trenches, code == "M") %>%
    dplyr::mutate(DEPTH = s[i]+25)
}
muk1952.trenches.lower <- do.call(rbind, muk1952.trenches.lower.lst)

muk1952.trenches.sf <- rbind(
  muk1952.trenches.upper, 
  muk1952.trenches.lower)

muk1952.trenches.sf$spit <- cut(muk1952.trenches.sf$DEPTH, spits)




ggplot() + 
  geom_sf(data = muk1952.trenches.sf, fill = "white") + 
  
  geom_sf(data = na.omit(li.1952.vdist), aes(fill = value)) + 
  #scale_fill_distiller(palette = "Blues", direction = 1) +  
  scale_fill_gradient("qty", trans = "log", 
                      low = "white", high = colorspace::darken("#619cff", .2), 
                      breaks = logscale_breaks, labels = logscale_breaks, 
                      name = "Lithics") + 
  ggnewscale::new_scale_fill() + 
  
  geom_sf(data = na.omit(po.1952.vdist), aes(fill = value)) + 
  #scale_fill_distiller(palette = "Reds", direction = 1) +  
  scale_fill_gradient("qty", trans = "log", 
                      low = "white", high = "#f8766d", 
                      breaks = logscale_breaks, labels = logscale_breaks, 
                      name = "Pottery") + 
  
  #geom_sf(data = na.omit(excav.1952.vdist), aes(fill = value)) + 
  #scale_fill_gradientn("qty", trans = "log", 
  #                     colours = viridis(10, option = "plasma"),
  #                     #low = "black", high = "yellow", 
  #                     breaks = logscale_breaks, labels = logscale_breaks) + 
  geom_sf(data = bequaert1956p35f.sf, shape = 21, fill = "white", size = 4) + 
  geom_sf(data = dplyr::filter(muk1952_trenches, code == "M"), fill = NA, linewidth = 1) + 
  facet_wrap(~spit, ncol = 6) + 
  theme_void() + 
  theme(legend.position = "bottom", 
        #axis.title = element_blank(), 
        #axis.text = element_blank(), 
        #panel.grid = element_blank(), 
        plot.background = element_rect(fill = "white", color = NA))
#ggsave("output/Fig_MUK1952_horizontal-find-distr-per-square.jpg", width = 9, height = 7)


# finer resolution (20 cm) barchart ----
# create artificial layers where to cut the vertical distribution
spits20 <- seq(0, 700, 20)
li.1952.coll$spit <- cut(li.1952.coll$DEPTH, spits20)


li.1952.coll.20 <- li.1952.coll %>% 
  dplyr::filter(!grepl("-", SQUARE)) %>%
  dplyr::mutate(
    dplyr::across(
      'SQUARE', 
      str_replace, "N'", 'N')) %>%
  dplyr::group_by(SQUARE, spit) %>%
  dplyr::summarise(n = sum(N)) %>% 
  dplyr::mutate(depth = sub("^.*?,", "", spit)) %>%
  dplyr::mutate(depth = as.numeric(sub("]", "", depth))) %>%
  dplyr::mutate(SQUARE = factor(SQUARE, 
                                levels = c("L", "H", "I", "C", "D", "E", "G", "M", "F", "K", "N", "J")))

ggplot(li.1952.coll.20) + 
  aes(x = depth, y = n) + 
  geom_bar(
    stat="identity"
  ) + 
  geom_vline(data = li.1952.max.depth %>%
               tibble::rownames_to_column("SQUARE") %>%
               dplyr::filter(SQUARE %in% li.1952.coll.20$SQUARE) %>%
               dplyr::mutate(SQUARE = factor(SQUARE, 
                                             levels = c("L", "H", "I", "C", "D", "E", "G", "M", "F", "K", "N", "J"))), 
             aes(xintercept = max.depth)) + 
  facet_wrap(SQUARE ~ ., 
             ncol = 3) + 
  scale_x_reverse() + 
  coord_flip()
  






# Lithics measurments ----

li.1952 %>%
  dplyr::filter(!grepl("-", length)) %>%
  dplyr::mutate(length = as.numeric(gsub(",", ".", gsub("\\.", "", length)))) %>%
  ggplot(aes(length)) + 
  geom_histogram()


# FOR TEXT ----

# Collection:

length(unique(c(li.1952.coll$Numéro.EBS, 
                po.1952$InvNo)))

sum(li.1952.coll$N)

coll.n.sum <- rbind(li.1952.coll %>% 
        dplyr::group_by(Numéro.EBS) %>% 
        dplyr::summarise(sum = sum(N)) %>%
        dplyr::rename(InvNo = Numéro.EBS), 
      po.1952 %>%
        dplyr::group_by(InvNo) %>%
        dplyr::summarise(sum = sum(N))) %>%
  dplyr::group_by(InvNo) %>%
  dplyr::summarise(sum = sum(sum))

sum(coll.n.sum$sum)

coll.n.sum %>% summary()

(coll.n.sum %>% dplyr::filter(sum <= 1) %>% nrow()) / (coll.n.sum %>% nrow())

# LITHICS ----

li.1952.m.z.sum %>%
  dplyr::group_by(variable) %>% 
  dplyr::summarise(value = sum(value)) %>%
  dplyr::mutate(pct = value / sum(value) * 100)

# POTTERY ----

# Anzahl Scherben:
sum(po.1952$N)

# Anteile je Quadrant:
print(po.1952 %>%
  dplyr::group_by(Quadrant) %>%
  dplyr::summarise(N = sum(N)) %>%
  dplyr::mutate(total = sum(N)) %>%
  dplyr::group_by(Quadrant) %>%
  dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
  dplyr::select(-total))


# Anteile je Tiefe:
print(po.1952 %>%
       dplyr::group_by(z.class) %>%
       dplyr::summarise(N = sum(N)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(z.class) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Anteile je Typ:
print(po.1952 %>%
       dplyr::group_by(Typ) %>%
       dplyr::summarise(N = sum(N)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Typ) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Anteile je Fabric:
print(po.1952 %>%
       dplyr::group_by(Fabric) %>%
       dplyr::summarise(N = sum(N)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Fabric) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

print(po.1952 %>%
       dplyr::group_by(Color) %>%
       dplyr::summarise(N = sum(N)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Color) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Shapes
print(po.1952 %>%
       dplyr::group_by(Form) %>%
       dplyr::summarise(N = sum(N)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Form) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Decoration
print(po.1952 %>%
       dplyr::group_by(Dekor) %>%
       dplyr::summarise(N = sum(N)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Dekor) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# GEODETIC MEASUREMENTS ----

muk1952.geodetic.surv <- readxl::read_excel("input/MUK52_Vermesssung.xls")

ggplot(muk1952.geodetic.surv, 
         aes(x = X, y = Y, label = Beschr)) + 
  geom_text() +
  coord_equal()

muk1952.geodetic.surv %>%
  dplyr::filter(!is.na(X)) %>%
  sf::st_as_sf(coords = c("X", "Y"), remove = F, crs = 32733) %>%
  sf::st_write("gis/MUK52_Vermesssung.geojson", delete_dsn = T)
