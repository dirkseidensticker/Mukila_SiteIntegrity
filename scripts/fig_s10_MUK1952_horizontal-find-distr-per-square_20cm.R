
source("scripts/MUK1952_GiteIIB.R")

muk1952_trenches <- geojsonsf::geojson_sf("gis/MUK1952_trenches.geojson")
st_crs(muk1952_trenches) = 32733

# create artificial layers where to cut the vertical distribution
spits <- seq(0, 700, 20)

# LITHICS ----

# data from analysis KJ for square M
li.1952.m.z.new <- li.1952.m.z %>%
  dplyr::group_by(z.class) %>%
  dplyr::summarise(value = n()) %>%
  dplyr::rename(spit = z.class) %>%
  dplyr::mutate(variable = "M", 
                TYPE = "LITHICS") %>%
  dplyr::select(spit, variable, value, TYPE)

# data from collection RMCA
li.1952.coll$SQUARE[li.1952.coll$SQUARE == "x"] <- "M" # attribute all x's (unknown squares) to square M

li.1952.coll[!is.na(li.1952.coll$Z..m.) & is.na(li.1952.coll$DEPTH),]$DEPTH <- li.1952.coll[!is.na(li.1952.coll$Z..m.) & is.na(li.1952.coll$DEPTH),]$Z..m.*-100 - 42 # obfl in quadrant M laut Bequaert bei 0,42 m unter messpkt

as.data.frame(tapply(filter(li.1952.coll, !is.na(li.1952.coll$DEPTH))$DEPTH, filter(li.1952.coll, !is.na(li.1952.coll$DEPTH))$SQUARE, max))

li.1952.coll$spit <- cut(li.1952.coll$DEPTH, spits)

li.1952.z.sqr <- reshape2::dcast(
  li.1952.coll[,c("spit", "SQUARE", "N")], 
  spit ~ SQUARE, 
  value.var = "N",
  fun.aggregate = sum)

li.1952.z.sqr <- reshape2::melt(li.1952.z.sqr, id.vars = "spit")
li.1952.z.sqr$TYPE <- "LITHICS"

# replace data from collection with those obtained by KJ
li.1952.z.sqr <- li.1952.z.sqr %>% 
  dplyr::filter(variable != "M") %>%
  rbind(li.1952.m.z.new)

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
z.1952.sqr <- z.1952.sqr[!is.na(z.1952.sqr$spit),]

excav.1952.vdist <- merge(x = muk1952_trenches,
                          y = z.1952.sqr, 
                          by.x = "code",
                          by.y = "variable",
                          all.x = TRUE)
excav.1952.vdist

li.1952.z.sqr <- li.1952.z.sqr[!li.1952.z.sqr$value == 0,] # remove rows where sum was 0
li.1952.z.sqr <- li.1952.z.sqr[!is.na(li.1952.z.sqr$spit),]

li.1952.vdist <- merge(x = muk1952_trenches,
                       y = li.1952.z.sqr, 
                       by.x = "code",
                       by.y = "variable",
                       all.x = TRUE)

po.1952.z.sqr <- po.1952.z.sqr[!po.1952.z.sqr$value == 0,] # remove rows where sum was 0
po.1952.z.sqr <- po.1952.z.sqr[!is.na(po.1952.z.sqr$spit),]

po.1952.vdist <- merge(x = muk1952_trenches,
                       y = po.1952.z.sqr, 
                       by.x = "code",
                       by.y = "variable",
                       all.x = TRUE)

# BEQUAERT 1956 p. 35f.
bequaert1956p35f <- data.table::fread("input/bequaert1956p35f.csv", encoding = "UTF-8")

b.sel <- bequaert1956p35f %>% 
  dplyr::filter(Li == "x") %>%
  dplyr::select(1:3, 11:29)

# loop over "lentils"
cnt <- 1
res.lst <- list()
for (i in 1:nrow(b.sel)) {

  # refine depth of "lentil" in spits
  z.sq <- seq(b.sel[i,top], b.sel[i,btm], -.01)
  z.cl <- cut(z.sq*-100 - 42, breaks = spits) %>% unique()
  
  # loop over spits in each "lentil"
  for (j in 1:length(z.cl)) {
    # list of square with finds at particular depth
    a <- b.sel[i] %>% 
      dplyr::select(4:ncol(b.sel)) %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("square") %>%
      dplyr::filter(V1 == "x") %>%
      dplyr::pull("square")

    b <- data.frame(spit = z.cl[j], 
                    square = a)
    
    res.lst[[cnt]] <- b
    cnt <- cnt + 1
  }
}
b.res <- do.call(rbind, res.lst)

# merging with centeroids of trenches:

bequaert1956p35f.sf <- muk1952_trenches %>%
  sf::st_centroid() %>%
  dplyr::left_join(
    b.res %>%
      dplyr::rename(code = square) %>%
      dplyr::mutate(code = replace(code, code == "N.", "N'"), 
                    code = sub("[.]", "-", code)), 
    by = "code") %>% 
  dplyr::filter(!is.na(spit))

# plot
logscale_breaks = c(1, 10, 100, 1000)


# define at what depth only square M was excavated? +/- 350 cm it seems

cutoff <- 300
muk1952.trenches.upper.lst <- list()
for (i in 1:length(spits[spits <= cutoff])) {
  muk1952.trenches.upper.lst[[i]] <- dplyr::filter(muk1952_trenches, !is.na(code)) %>%
    dplyr::mutate(DEPTH = spits[i])
}
muk1952.trenches.upper <- do.call(rbind, muk1952.trenches.upper.lst)

muk1952.trenches.lower.lst <- list()
s <- head(spits[spits > cutoff],-1)
for (i in 1:length(s)) {
  muk1952.trenches.lower.lst[[i]] <- dplyr::filter(muk1952_trenches, code == "M") %>%
    dplyr::mutate(DEPTH = s[i])
}
muk1952.trenches.lower <- do.call(rbind, muk1952.trenches.lower.lst)

muk1952.trenches.sf <- rbind(
  muk1952.trenches.upper, 
  muk1952.trenches.lower)

muk1952.trenches.sf$spit <- cut(muk1952.trenches.sf$DEPTH, spits)

sp <- unique(muk1952.trenches.sf$spit)
sp <- sp[!is.na(sp)]
sp <- as.character(sp)

sp.lab <- gsub("(", "", sp, fixed=TRUE)
sp.lab <- gsub(",", "-", sp.lab)
sp.lab <- gsub("]", "", sp.lab)
sp.lab <- paste(sp.lab, "cm")

names(sp.lab) = sp


ggplot() + 
  geom_sf(data = muk1952.trenches.sf %>% dplyr::filter(!is.na(spit)), fill = "white") + 
  geom_sf(data = na.omit(li.1952.vdist), aes(fill = value)) + 
  scale_fill_gradient("qty", trans = "log", 
                      low = "white", high = colorspace::darken("#619cff", .2), 
                      breaks = logscale_breaks, labels = logscale_breaks, 
                      name = "Lithics") + 
  ggnewscale::new_scale_fill() + 
  geom_sf(data = na.omit(po.1952.vdist), aes(fill = value)) + 
  scale_fill_gradient("qty", trans = "log", 
                      low = "white", high = "#f8766d", 
                      breaks = logscale_breaks, labels = logscale_breaks, 
                      name = "Pottery") + 
  geom_sf(data = dplyr::filter(muk1952_trenches, code == "M"), fill = NA, linewidth = 1) + 
  geom_sf(data = bequaert1956p35f.sf, shape = 21, fill = "white", size = 2) + 
  facet_wrap(~spit, ncol = 5, labeller = as_labeller(sp.lab)) + 
  theme_void() + 
  theme(legend.position = "bottom", 
        #axis.title = element_blank(), 
        #axis.text = element_blank(), 
        #panel.grid = element_blank(), 
        plot.background = element_rect(fill = "white", color = NA))
ggsave("output/Fig_S10_MUK1952_horizontal-find-distr-per-square_20cm.jpg", width = 6, height = 10)
ggsave("output/fig_s10_muk1952_horizontal-find-distr-per-square_20cm.pdf", width = 6, height = 10)

# vertical plot



rbind(
  li.1952.vdist %>%
    dplyr::group_by(spit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(depth = as.character(spit)) %>%
    dplyr::mutate(depth = gsub("(.*),.*", "\\1", depth)) %>%
    dplyr::mutate(depth = readr::parse_number(depth)) %>%
    dplyr::mutate(class = "lithics"),
  po.1952.vdist %>%
    dplyr::group_by(spit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(depth = as.character(spit)) %>%
    dplyr::mutate(depth = gsub("(.*),.*", "\\1", depth)) %>%
    dplyr::mutate(depth = readr::parse_number(depth)) %>%
    dplyr::mutate(class = "pottery")
  ) %>%
  ggplot(aes(x = depth+10, y = value, fill = class)) + 
  geom_bar(stat = "identity") + 
  geom_segment(data = bequaert1956p35f %>%
                 dplyr::filter(Li == "x") %>%
                 dplyr::select(from, to) %>%
                 dplyr::mutate(class = NA), 
               aes(x = from*100, xend = to*100, y = -10, yend = -10)) + 
  scale_x_reverse("depth [cm]", breaks = seq(0, 660, 20), limits = c(660, 0)) + 
  scale_y_continuous("qty", limits = c(-20, 700), expand = c(0, 0)) + 
  coord_flip()
