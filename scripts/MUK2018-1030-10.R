#############################
# MUK 2018/1030/10
#############################

library(c14bazAAR)
library(cowplot)
library(dplyr)
library(geojsonsf)
library(ggplot2)
library(ggridges)
library(readxl)
library(sf)
library(tidyr)

excav.2018 <- geojson_sf("gis/MUK2018_excav.geojson")

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
  dplyr::filter(LABNR != '/' & YEAR != 2018) %>%
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


plt.c14 <- ggplot(c14.18.calprobdist.norm) + 
  ggridges::geom_ridgeline(data = c14.52.calprobdist.norm, 
                           aes(x = (-calage + 1950)/1000, 
                               y = DEPTH, 
                               height = density, 
                               group = LABNR), 
                           scale = 15, fill = "grey", color = NA) + 
  ggridges::geom_ridgeline(aes(x = (-calage + 1950)/1000, 
                               y = DEPTH-2, 
                               height = density, 
                               group = LABNR), 
                           scale = 15, fill = "black", color = NA) + 
  scale_x_continuous(breaks = c(seq(-40, 2, 5)),
                     limits = c(-41.25, 2),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  scale_y_reverse("Depth [cm]", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 

  annotate("rect", ymin = 642, ymax = 658, xmin = -Inf, xmax = -40.5,
           alpha = .1) + 
  annotate("rect", ymin = 642, ymax = Inf, xmin = -39.1, xmax = -40.5,
           alpha = .15) + 
  annotate("rect", ymin = 302, ymax = 358, xmin = -Inf, xmax = -12.570,
           alpha = .1) + 
  annotate("rect", ymin = 302, ymax = Inf, xmin = -12.031, xmax = -12.570,
           alpha = .15) + 
  #annotate("text", y = 330, x = -13500, label = "VIII") + 
  annotate("rect", ymin = 222, ymax = 298, xmin = -Inf, xmax = -9.355,
           alpha = .1) + 
  annotate("rect", ymin = 222, ymax = Inf, xmin = -9.890, xmax = -9.355,
           alpha = .15) + 
  #annotate("text", y = 270, x = -13500, label = "VII") + 
  #annotate("text", y = 220, x = -13500, label = "VI") + 
  annotate("rect", ymin = 162, ymax = 178, xmin = -Inf, xmax = -3.014,
           alpha = .1) + 
  annotate("rect", ymin = 162, ymax = Inf, xmin = -3.341, xmax = -3.014,
           alpha = .15) + 
  #annotate("text", y = 180, x = -13500, label = "V") + 
  annotate("rect", ymin = 102, ymax = 158, xmin = -Inf, xmax = -1.453,
           alpha = .1) + 
  annotate("rect", ymin = 102, ymax = Inf, xmin = -1.771, xmax = -1.453,
           alpha = .15) + 
  #annotate("text", y = 130, x = -13500, label = "IV") + 
  #annotate("text", y = 90, x = -13500, label = "III") + 
  annotate("rect", ymin = 62, ymax = 98, xmin = -Inf, xmax = .116,
           alpha = .1) + 
  annotate("rect", ymin = 62, ymax = Inf, xmin = -.352, xmax = .116,
           alpha = .15) + 
  #annotate("text", y = 70, x = -13500, label = "II") + 
  annotate("rect", ymin = 42, ymax = 58, xmin = -Inf, xmax = 1.633,
           alpha = .1) + 
  annotate("rect", ymin = 42, ymax = Inf, xmin = 1.326, xmax = 1.633,
           alpha = .15) + 
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

# pottery ----

po.2018 <- data.table::fread("input/MUK2018_1030_10_Pottery.csv", encoding = "UTF-8") %>%
  dplyr::mutate(Act = as.character(Act))

po.2018.geom <- merge(x = po.2018, by.x = "Act",
                      y = excav.2018, by.y = "act")



po.2018.wgt <- po.2018.geom %>%
  dplyr::group_by(z) %>%
  dplyr::summarise(Pottery = sum(Wgt))

plt.po.2018.wgt <- ggplot(po.2018.wgt, aes(x = (z-10), y = Pottery)) + 
  geom_bar(stat = "identity", fill = "#f8766d", color = "black") + 
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) +
  scale_y_continuous("Weight [g]", #breaks = c(seq(0, 15000, 2000)),
                     limits = c(0, 1220),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  geom_vline(xintercept = 60, linetype="dashed") + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# pottery refits ----

po.2018.refit <- data.table::fread("input/MUK2018_1030_10_PotteryRefit.csv", encoding = "UTF-8")

po.2018.refit$FROM <- gsub("-", "", po.2018.refit$FROM)
po.2018.refit$FROM <- gsub(":.", "", po.2018.refit$FROM)
po.2018.refit$TO <- gsub("-", "", po.2018.refit$TO)
po.2018.refit$TO <- gsub(":.", "", po.2018.refit$TO)

excav.2018.cent <- do.call(rbind, st_geometry(st_centroid(excav.2018))) %>%
  as_tibble() %>% 
  setNames(c("lon","lat"))
excav.2018.cent2 <- cbind(excav.2018, excav.2018.cent)


po.2018.refit.m1 <- merge(x = po.2018.refit, by.x = "FROM",
                          y = excav.2018.cent2, by.y = "act")

po.2018.refit.m2 <- merge(x = po.2018.refit.m1, by.x = "TO",
                          y = excav.2018.cent2, by.y = "act")

po.2018.refit.m2$z.x <- po.2018.refit.m2$z.x - 10
po.2018.refit.m2$z.y <- po.2018.refit.m2$z.y - 10

po.2018.refit.m2$lon.x <- jitter(po.2018.refit.m2$lon.x)
po.2018.refit.m2$lon.x <- jitter(po.2018.refit.m2$lon.x)
po.2018.refit.m2$lat.x <- jitter(po.2018.refit.m2$lat.x)
po.2018.refit.m2$lat.y <- jitter(po.2018.refit.m2$lat.y)
po.2018.refit.m2$z.x <- jitter(po.2018.refit.m2$z.x)
po.2018.refit.m2$z.y <- jitter(po.2018.refit.m2$z.y)

plt.po.2018.refit <- ggplot(data = po.2018.refit.m2, 
       aes(x = lon.x, y = z.x,
           xend = lon.y, yend = z.y)) + 
  geom_segment(color = "#f8766d") + 
  scale_x_reverse("Refit", breaks = NULL) + 
  scale_y_reverse("", limits = c(660, 0), #40
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 
  geom_hline(yintercept = 60, linetype="dashed") + 
  theme_light() + 
  theme(axis.text.y = element_blank())

# lithics ----
li.2018.qty <- data.table::fread(
  "input/MUK2018_1030_10_lithics_size_qty.csv", 
  encoding = "UTF-8")

li.2018.qty %>% 
  dplyr::mutate(sum = rowSums(dplyr::across(3:7))) %>%
  dplyr::select(SQUARE, DEPTH, sum) %>%
  reshape2::dcast(DEPTH ~ SQUARE, 
                  value.var = "sum")

li.2018.qty <- li.2018.qty %>%
  dplyr::filter(DEPTH > 40 & !(SQUARE %in% c("1a", "1b") & DEPTH == 60))

li.2018.qty[,"< 2 cm"] <- rowSums(li.2018.qty[,c(5:7)]) # sum class for larger then 2 cm
li.2018.qty[,"> 2 cm"] <- rowSums(li.2018.qty[,c(3:4)]) # sum class for larger then 2 cm

li.2018.qty <- li.2018.qty[,c(2, 8:9)] %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(DEPTH) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(), 
      list(sum))) # sum per unit/depth

li.2018.qty <- reshape2::melt(li.2018.qty, id.vars = "DEPTH")

li.2018.qty <- li.2018.qty %>%
  dplyr::mutate(variable = gsub("_1","", variable)) %>%
  dplyr::mutate(variable = factor(variable, levels = c("> 2 cm", "< 2 cm")))

plt.li.2018.qty <- ggplot(li.2018.qty, 
       aes(
         x = (DEPTH-10), 
         y = value, 
         fill = variable)) + 
  #geom_rect(xmin = 0, xmax = -40, ymin = 0, ymax = 130, fill = "#d3d3d3") + 
  geom_bar(stat = "identity", color = "black", linewidth = .25) + 
  scale_fill_manual("", values = c("#619cff", "#d9e6fa"), 
                    guide = guide_legend(reverse = TRUE)) + 
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) +
  scale_y_continuous("Quantity", #breaks = c(seq(0, 15000, 2000)),
                     limits = c(0, 80),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  coord_flip() + 
  theme_light() + 
  theme(legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(
    ncol = 1, 
    reverse = TRUE))

# charcoal ----

ch.2018 <- data.table::fread("input/MUK2018_1030_10_CharcoalMeta.csv", encoding = "UTF-8")

#ch.2018 <- filter(ch.2018, depth <= 360 & depth >= 60 & !grepl('/', ch.2018$Square))
ch.2018 <- filter(ch.2018, depth >= 60 & !grepl('/', ch.2018$Square))
ch.2018[ch.2018$depth == 175, 4] <- 180 # add 175 cm to 180 cm
ch.2018 <- group_by(ch.2018, depth)
ch.2018.wgt <- summarise(ch.2018,
                         V = sum(soil_volume_cm3),
                         Charcoal = sum(as.numeric(charcoal_wood_g), na.rm = T),
                         Endocarp = sum(as.numeric(charcoal_endo_g), na.rm = T))
ch.2018.wgt[ch.2018.wgt == 0] <- NA
ch.2018.wgt[ch.2018.wgt$depth == 180,2] <- 900000
ch.2018.wgt$V <- ch.2018.wgt$V / 1000
ch.2018.wgt[,3:4] <- ch.2018.wgt[,3:4] / ch.2018.wgt$V

ch.2018.wgt <- ch.2018.wgt[,c("depth", "Endocarp", "Charcoal")]

ch.2018.wgt <- reshape2::melt(ch.2018.wgt, id.vars = "depth")

plt.ch.2018.wgt <- ggplot(ch.2018.wgt, aes(x = (depth-10), y = value, fill = variable)) + 
  geom_bar(stat = "identity", color = "black") + 
  scale_fill_manual("", values = c("grey75", "grey25"), 
                    guide = guide_legend(reverse = TRUE)) + 
  scale_x_reverse("", limits = c(660, 0), #40
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) +
  scale_y_continuous("Weight per liter \n sediment [g/l]", #breaks = c(seq(0, 15000, 2000)),
                     limits = c(0, 0.55),
                     #minor_breaks = NULL,
                     expand = c(0, 0)) + 
  geom_vline(xintercept = 60, linetype="dashed") + 
  geom_vline(xintercept = 80, linetype="dashed") + 
  geom_vline(xintercept = 100, linetype="dashed") + 
  geom_vline(xintercept = 160, linetype="dashed") + 
  geom_vline(xintercept = 200, linetype="dashed") + 
  geom_vline(xintercept = 240, linetype="dashed") + 
  geom_vline(xintercept = 300, linetype="dashed") + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom", 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  guides(fill=guide_legend(ncol = 1))

# charcoal types / seriation / heatmap ----

ch.2018.spe <- data.table::fread("input/MUK2018_1030_10_CharcoalSpecies.csv", 
                                 encoding = "UTF-8") %>%
  dplyr::filter(!(Type_Nr %in% c("endocarp", "indet"))) %>% 
  dplyr::mutate(Couche = as.numeric(Couche), 
                Type_Nr = as.numeric(Type_Nr))

# how often have the speicies been identified
ch.2018.spe.n <- ch.2018.spe %>% 
  dplyr::group_by(Type_Nr) %>%
  dplyr::summarise(n = length(Cluster)) %>%
  dplyr::arrange(desc(n))

ch.2018.spe.n.order <- ch.2018.spe.n %>%
  dplyr::pull(Type_Nr)
  
ch.2018.spe.n %>%
  dplyr::mutate(Type_Nr = factor(Type_Nr, levels = ch.2018.spe.n.order)) %>%
  ggplot(aes(x = Type_Nr, y = n)) + 
  geom_bar(stat = "identity")

# visual inspection sets threshold at 5

ch.2018.spe.n.thresh <- ch.2018.spe.n %>%
  dplyr::filter(n > 5) %>%
  dplyr::pull(Type_Nr)

plt.ch.2018.species <- ch.2018.spe %>% 
  reshape2::dcast(Couche ~ Type_Nr, 
                  fun.aggregate = length, 
                  value.var = "Cluster") %>%
  dplyr::arrange(Couche) %>%
  reshape2::melt(id.vars = "Couche") %>%
  dplyr::filter(value != 0 & variable %in% ch.2018.spe.n.thresh) %>%
  ggplot(aes(x = variable, 
             y = Couche - 10, # to keep dot in the center of the spit 
             size = value)) + 
    geom_point(shape = 21, fill = "grey25") + 
    scale_y_reverse("", limits = c(660, 0), # 40 
                    breaks = c(seq(0, 660, 20)),
                    minor_breaks = NULL,
                    expand = c(0, 0)) +
    scale_x_discrete("Charcoal Species (encoded) [n]", 
                     limits = rev) + 
    scale_size("quantity", breaks = c(1, 5, 20, 40))  +
    guides(size = guide_legend(ncol = 1)) + 
    geom_hline(yintercept = 60, linetype="dashed") + 
    annotate("text", x = 1.5, y = 50, label = "I") + 
    geom_hline(yintercept = 100, linetype="dashed") + 
    annotate("text", x = 2, y = 80, label = "II") + 
    geom_hline(yintercept = 140, linetype="dashed") + 
    annotate("text", x = 1.5, y = 120, label = "III") + 
    #geom_hline(yintercept = 160, linetype="dashed") + 
    annotate("text", x = 1.5, y = 160, label = "IV") + 
    geom_hline(yintercept = 200, linetype="dashed") + 
    annotate("text", x = 1.5, y = 220, label = "V") + 
    geom_hline(yintercept = 240, linetype="dashed") + 
    annotate("text", x = 1.25, y = 270, label = "VI") + 
    geom_hline(yintercept = 300, linetype="dashed") + 
    annotate("text", x = 2, y = 330, label = "VII") + 
    theme_light() + 
    theme(axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom", 
          legend.title = element_blank())

# grain size ----

gs.2018 <- read.csv("input/MUK2018_1030_10_GrainSizes.csv")
#gs.2018 <- filter(gs.2018, X <= 360)

gs.2018 <- reshape2::melt(gs.2018[,c("X", "Sand..", "Silt..", "Clay..")], id.vars = "X")

gs.2018$variable <- factor(gs.2018$variable , levels=c("Clay..", "Silt..", "Sand..") )

plt.gs.2018 <- ggplot(gs.2018, aes(x = X, y = value, fill = variable)) + 
  geom_area(color = "black") +
  scale_x_reverse("", limits = c(660, 0), # 40 
                  breaks = c(seq(0, 660, 20)),
                  minor_breaks = NULL,
                  expand = c(0, 0)) + 
  scale_y_continuous("Soil Texture [%]", 
                     limits = c(0, 100), 
                     expand = c(0, 0)) + 
  #scale_fill_viridis(option="plasma", discrete=TRUE) + 
  scale_fill_manual("", values = c("#fdfa72", "#fffdaf", "#ffffe0"),
                    labels = c("Clay", "Silt", "Sand")) + 
  #scale_fill_brewer("", palette = "Accent", 
  #                  labels = c("Clay", "Silt", "Sand")) + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_blank(), 
        legend.position = "bottom") +
  guides(fill=guide_legend(ncol = 1))


gs.2018 %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(min = min(value), 
                   mean = mean(value),
                   max = max(value))

# more detailed version ----

gs.2018 <- data.table::fread(
  "input/MUK2018_1030_10_GrainSizes_detailed.csv", 
  encoding = "UTF-8") %>%
  dplyr::mutate(
    Depth = as.numeric(sub("-.*", "", .$Depth))) %>%
  dplyr::select(-c("Code", "V3")) %>%
  tibble::column_to_rownames("Depth")

gs.2018[gs.2018 == "<1"] <- 0

gs.2018 <- gs.2018 %>%
  dplyr::mutate_if(is.character, as.integer)

cols.sel <- gs.2018 %>%
  colSums() %>%
  as.data.frame() %>%
  dplyr::filter(`.` > 0) %>%
  tibble::rownames_to_column("CLASS") %>%
  dplyr::pull("CLASS")

gs.2018 <- gs.2018 %>%
  dplyr::select(tidyr::all_of(cols.sel))

names(gs.2018) <- data.frame(CLASS = names(gs.2018)) %>%
  dplyr::mutate(NEW = paste0(CLASS, " µm")) %>%
  dplyr::pull(NEW)

gs.2018 <- gs.2018 %>%
  tibble::rownames_to_column("Depth") %>%
  reshape2::melt("Depth") %>%
  dplyr::mutate_at("Depth", as.numeric)

# area plot
plt.gs.2018 <- ggplot(gs.2018, 
       aes(
         x = Depth+10, 
         y = value,
         fill = variable)) + 
  geom_area() + 
  scale_x_reverse(
    "", 
    #limits = c(360, 0),
    breaks = seq(0, 660, 20), 
    expand = c(0, 0)) +
  scale_y_continuous(
    "Soil texture [%]", 
    expand = c(0, 0)) + 
  scale_fill_manual(
    values = c("navajowhite4",
               "navajowhite3",
               "navajowhite2",
               "navajowhite1",
               "navajowhite",
               "moccasin",
               "lightsteelblue3", 
               "lightsteelblue2", 
               "lightsteelblue1", 
               "plum2")) + 
  coord_flip(xlim = c(660, 0)) + 
  theme_light() +
  theme(axis.text.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  guides(
    fill = guide_legend(
      ncol = 2, 
      override.aes = list(colour = "black")))

# bar plot, following comment F. Mees:
plt.gs.2018 <- ggplot(gs.2018, 
       aes(
         x = Depth+10, 
         y = value,
         fill = variable)) + 
  geom_bar(stat = "identity", width = 20, color = "black") + 
  #geom_area() + 
  scale_x_reverse(
    "", 
    #limits = c(360, 0),
    breaks = seq(0, 660, 20), 
    expand = c(0, 0)) +
  scale_y_continuous(
    "Soil texture [%]", 
    expand = c(0, 0)) + 
  scale_fill_manual(
    values = c("navajowhite4",
               "navajowhite3",
               "navajowhite2",
               "navajowhite1",
               "navajowhite",
               "moccasin",
               "lightsteelblue3", 
               "lightsteelblue2", 
               "lightsteelblue1", 
               "plum2")) + 
  coord_flip(xlim = c(660, 0)) + 
  theme_light() +
  theme(axis.text.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank()) +
  guides(
    fill = guide_legend(
      ncol = 2, 
      override.aes = list(colour = "black")))


# separate plot + soil color (cf profile drawing) ---

cowplot::plot_grid(
  ggplot() + 
    annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 10, fill = munsell::mnsl("10YR 4/2")) + 
    #annotate("rect", xmin = 1.75, xmax = 2, ymin = 10, ymax = 70, fill = munsell::mnsl("10YR 5/2")) +
    annotate("rect", xmin = 1, xmax = 2, ymin = 10, ymax = 25, fill = munsell::mnsl("10YR 3/4")) +
    annotate("rect", xmin = 1, xmax = 2, ymin = 25, ymax = 70, fill = munsell::mnsl("10YR 4/4")) + 
    annotate("rect", xmin = 1, xmax = 2, ymin = 70, ymax = 90, fill = munsell::mnsl("10YR 5/4")) + 
    annotate("rect", xmin = 1, xmax = 2, ymin = 90, ymax = 200, fill = munsell::mnsl("10YR 5/8")) + 
    annotate("rect", xmin = 1, xmax = 2, ymin = 200, ymax = 360, fill = munsell::mnsl("10YR 6/8")) +
    
    annotate("rect", xmin = 1, xmax = 2, ymin = 360, ymax = 380, fill = munsell::mnsl("10YR 5/6")) +
    annotate("rect", xmin = 1, xmax = 2, ymin = 380, ymax = 500, fill = munsell::mnsl("10YR 4/6")) +
    
    annotate("rect", xmin = 1, xmax = 2, ymin = 500, ymax = 660, fill = munsell::mnsl("10YR 3/6")) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_reverse("Depth [cm]", 
                    breaks = seq(0, 660, 20), 
                    expand = c(0, 0)) + 
    theme_light() + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()),  
  ggplot(gs.2018, 
         aes(
           x = Depth+10, 
           y = value,
           fill = variable)) + 
    geom_bar(stat = "identity", width = 20, color = "black") + 
    #geom_area() + 
    scale_x_reverse(
      "", 
      #limits = c(360, 0),
      breaks = seq(0, 660, 20), 
      expand = c(0, 0)) +
    scale_y_continuous(
      "Soil texture [%]", 
      expand = c(0, 0)) + 
    scale_fill_manual(
      "Grain size",
      values = c("#d9e581",
                 "#c7e491",
                 "#b8e1a0",
                 "#afdead",
                 "#abd9b8",
                 "#acd3c0",
                 "lightpink3", 
                 "lightpink2", 
                 "lightpink1", 
                 "powderblue")) + 
      #values = c("navajowhite4",
      #           "navajowhite3",
      #           "navajowhite2",
      #           "navajowhite1",
      #           "navajowhite",
      #           "moccasin",
      #           "lightsteelblue3", 
      #           "lightsteelblue2", 
      #           "lightsteelblue1", 
      #           "plum2")) + 
    coord_flip(xlim = c(660, 0)) + 
    theme_light() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "right", 
          axis.text.y = element_blank()),
  align = 'h', axis = "tb", 
  rel_widths = c(1,4), 
  labels = "AUTO"
)
ggsave("output/Fig_6_MUK2018_1030_10_grainsizes.jpg", width = 8/1.25, height = 6/1.25)
ggsave("output/fig_6_muk2018_1030_10_grainsizes.pdf", width = 8/1.25, height = 6/1.25)


# combined figure ----

plt <- cowplot::plot_grid(plt.c14, 
                          plt.ch.2018.wgt, 
                          plt.ch.2018.species, 
                          plt.li.2018.qty + theme(axis.text.y = element_blank(), axis.title.y = element_blank()), 
                          plt.po.2018.wgt, 
                          #plt.po.2018.refit,
                          plt.gs.2018,
                          labels = c("A", "B", "C", "D", "E", "F", "G"),
                          nrow = 1, rel_widths = c(2, 1, 2, 1, 1, 1.5),
                          align = 'h', axis = "tb")
#ggsave("output/Fig_MUK2018_1030_10.jpg", plt, width = 16, height = 10)


plt <- cowplot::plot_grid(plt.li.2018.qty, 
                          plt.po.2018.wgt,
                          plt.c14 + theme(axis.text.y = element_blank(), axis.title.y = element_blank()), 
                          plt.ch.2018.wgt, 
                          plt.ch.2018.species, 
                          #plt.po.2018.refit,
                          plt.gs.2018,
                          labels = c("A", "B", "C", "D", "E", "F", "G"),
                          nrow = 1, rel_widths = c(1, 1, 2, 1, 2, 1.5),
                          align = 'h', axis = "tb")
#ggsave("output/Fig_MUK2018_1030_10_alt.jpg", plt, width = 16, height = 10)


# FOR TEXT ----

# POTTERY ----
po.2018.geom <- po.2018.geom %>%
  replace_na(list(n = 1))

# Anzahl Scherben:
sum(po.2018.geom$n)

# Anteile je Square:
print(po.2018.geom %>%
       dplyr::group_by(square) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(square) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Anteile je Tiefe:
print(po.2018.geom %>%
       dplyr::group_by(z) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(z) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Anteile je Fragmentgröße:
print(po.2018.geom %>%
       dplyr::group_by(Typ) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Typ) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

po.2018.geom <- po.2018.geom %>%
  dplyr::mutate(Wgt.n = Wgt / n)

summary(po.2018.geom$Wgt.n)

ggplot(po.2018.geom, aes(x = Wgt.n)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black")

# Anteile je Typ:
print(po.2018.geom %>%
       dplyr::group_by(Size) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Size) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Verteilung Wandungsstärke:
ggplot(po.2018.geom, aes(x = Wall)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black")

summary(po.2018.geom$Wall)
  
# Anteile je Fabric:
print(po.2018.geom %>%
       dplyr::group_by(Fabric) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Fabric) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Anteile je Farbe:
print(po.2018.geom %>%
       dplyr::group_by(Color) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Color) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

# Anteile je Form:
print(po.2018.geom %>%
       dplyr::group_by(Morph) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Morph) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))


# Anteile je Verzierungen:
print(po.2018.geom %>%
       dplyr::group_by(Decor) %>%
       dplyr::summarise(N = sum(n)) %>%
       dplyr::mutate(total = sum(N)) %>%
       dplyr::group_by(Decor) %>%
       dplyr::mutate(per = paste0(round(100 * N / total, 2), "%")) %>%
       dplyr::select(-total))

