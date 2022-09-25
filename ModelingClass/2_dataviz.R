library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(patchwork)
library(visreg)
library(MASS)
library(nlme)
library(mgcv)
library(terra)

load("outputs/2021-09-20_cleaned-data.rda")

setwd("C:/Users/sophi/Documents/UNH_Docs/ModelingClass")

g1 <- ggplot(sdat2) + 
  aes(x = dist_to_logging, y = secchi) + 
  geom_point() + 
  stat_smooth()
g1

g2 <- ggplot(sdat2) + 
  aes(x = dist_to_logging, y = CB_cover) + 
  geom_point() +
  stat_smooth()

g3 <- ggplot(sdat2) + 
  aes(x = CB_cover, y = pres.topa) + 
  geom_point() + 
  stat_smooth()

gall <- g1 + g2 + g3
gall

ggsave("plot1.png", gall, width = 8, height = 3)

tm_shape(sdat2) + 
  tm_symbols(col = "pres.topa", size = 0.2)

tland <- tm_shape(land) + 
  tm_fill()

tland + 
  tm_shape(sdat2) + 
  tm_symbols(col = "pres.topa", size = 0.2) +
  tm_scale_bar(position = c("right", "top"))


# Modeling
sdat2$log10_dist_logging <- log10(sdat2$dist_to_logging) #Using log as covariate in model. This is necause we're expecting polluiton effects to be most concentrated near ponds

#Normal gaussian GLM
glm1_gaus <- glm(pres.topa ~ log10_dist_logging + flow, data = sdat2)
par(mfrow = c(2,2))
plot(glm1_gaus)

#NORMAL DISTRIBUTION NOT APPROPORATE. LOOK AT POISSON

glm1_pois <- glm(pres.topa ~ log10_dist_logging + flow, data = sdat2,
                 family = "poisson")
par(mfrow = c(2,2))
plot(glm1_pois)

#POSSION NOT GREAT. LOOK AT NEGATIVE BINOMLIA FOR OVERDISPERSION
glm1_nb <- glm.nb(pres.topa ~ log10_dist_logging + flow, data = sdat2)
par(mfrow = c(2,2))
plot(glm1_nb)

AIC(glm1_gaus, glm1_pois, glm1_nb)

summary(glm1_nb)
#spatial AC can be endogenous or exogenous.. An example of endogenous AC is that caused by species dispersal dynamics. An example of exogenous AC is that caused by environmental drivers that we haven't modelled.

#Spatial Autocorrelation
sdat2$resid_glm_pois <- resid(glm1_pois)

tland + 
  tm_shape(sdat2) + 
  tm_symbols(col = "resid_glm_pois", size = 0.5)
#some clustering in residuals. Let's check out the semivariance function
#semivariance is a measure of the (weighted) pairwise deviations between residual values at different distances from each other. Low åand increasing semivariance means high correlations between sites that are nearby that weaken with distance. So in this plot we are looking for an increasing trend that flattens out (at what's called the 'sill'):


source("data/semivariance.R")
site_distmat <- st_distance(sdat2)/1000
dim(site_distmat)

glm1_pois_semivar <- semivariance(site_distmat, sdat2$resid_glm_pois, ncats = 15)

ggplot(glm1_pois_semivar) + 
  aes(x = distances, y = semivar) + 
  geom_point() + 
  stat_smooth()

glm_nb_plots <- plot_spatial_AC(sdat2, glm1_nb, site_distmat)
tland + glm_nb_plots[[1]]

glm_nb_plots[[2]]


visreg(glm1_nb)
#Note the partial effects of each covariate (ie effect of that covariate while holding other covars constant, plotted on y-axis) are on log scale, because of the log link. Use visreg(m1, scale = "response") to see results in counts of fish.


#Generalized Least Squares
sdat2$sqrt_topa <- sqrt(sdat2$pres.topa)

m1_gls <- gls(sqrt_topa ~ log10_dist_logging + flow, 
              data = sdat2)

sdat2$x <- st_coordinates(sdat2)[,1]
sdat2$y <- st_coordinates(sdat2)[,2]
cs1Sph <- corSpher(1, form = ~x + y)

m2_gls <- gls(sqrt_topa ~ log10_dist_logging + flow, 
              data = sdat2,
              correlation = cs1Sph)

plot(m2_gls)

summary(m2_gls)

gls_plots <- plot_spatial_AC(sdat2, m2_gls, site_distmat)
gls_plots[[2]]

#Generalized additive model
m1_gam <- gam(pres.topa ~ s(log10_dist_logging) + flow,
              family = "nb",
              data = sdat2)
visreg(m1_gam)

m1_gam_plots <- plot_spatial_AC(sdat2, m1_gam, site_distmat)

tland + m1_gam_plots[[1]]

m1_gam_plots[[2]]

#GAMS with spatial covariates
m2_gam <- gam(pres.topa ~ s(x, y, bs = "gp"),
              family = "nb",
              data = sdat2)
plot(m2_gam, se = FALSE)

m2_gam_plots <- plot_spatial_AC(sdat2, m2_gam, site_distmat)

tland + m2_gam_plots[[1]]

m2_gam_plots[[2]]

m3_gam <- gam(pres.topa ~ s(log10_dist_logging) + 
                s(x, y, bs = "gp"),
              family = "poisson",
              data = sdat2)
plot(m3_gam)

concurvity(m3_gam, full = FALSE)

#Model Viz
g1 <- visreg(m1_gam, xvar = "log10_dist_logging",
             scale = "response", gg = TRUE) + 
  xlab("Distance to log ponds (log10)") + 
  ylab("Topa abundance")
g1

flow_pred <- visreg(m1_gam, xvar = "flow",
                    scale = "response", gg = TRUE, plot = FALSE)

g2 <- ggplot(flow_pred$fit) + 
  aes(x = flow, y = visregFit) + 
  geom_point() + 
  geom_linerange(aes(ymin = visregLwr, ymax = visregUpr)) + 
  xlab("Flow") + 
  ylab("Topa abundance")

gboth <- g1 + g2 + 
  plot_layout(nrow = 1, widths = c(1, 0.5)) + 
  plot_annotation(tag_levels = "A")

gboth

ggsave("images/m1_gam_predictions.png", plot = gboth,
       width = 6, 
       height = 3)

#Generating predictions at sample locations
sdat2$gam1_pred <- predict(m1_gam, type = "response")

tland + 
  tm_shape(sdat2) +
  tm_symbols(col = "gam1_pred", size = 0.3) 

#Generating predictions everywhere
land_terra <- vect(land)
plot(land_terra) #Using a raster file!

kia_crs_terra <- crs(land_terra)

rlogponds <- rast(extent = ext(land_terra), crs = crs(land_terra),
                  res  = 500) #Create new blank raster with same extent and crs as land. using resolution 500x500

xy <- st_coordinates(logponds)
icell <- cellFromXY(rlogponds, xy)
rlogponds[icell] <- 1

#check it worked: 
tland + 
  tm_shape(rlogponds) + 
  tm_raster(palette = "Dark2")

rdist <- distance(rlogponds)

rdist_log10 <- rast(rdist)
rdist_log10[] <- log10(rdist[]/1000)
tm_shape(rdist_log10) + 
  tm_raster(palette = "-YlOrBr", n = 7) + 
  tland 

#Predicting to a raster
icell <- 1:ncell(rdist_log10)
pred <- data.frame(log10_dist_logging = rdist_log10[icell][,1],
                   cells = icell, 
                   x = xFromCell(rdist_log10, icell),
                   y = yFromCell(rdist_log10, icell), 
                   flow = "Mild") 

pred <- filter(pred, is.finite(log10_dist_logging))
head(pred)

pred$topa_pred <- predict(m1_gam, newdata = pred, type = "response")

rpred <- rast(rdist_log10)
rpred[pred$cells] <- matrix(pred$topa_pred, ncol = 1)

tm_shape(rpred) + 
  tm_raster(palette = "Blues",
            title= "Predicted abundance", alpha = 0.8, n=10) + 
  tm_shape(land) +
  tm_fill(col = "black") +
  tm_shape(logponds) + 
  tm_symbols(col = "brown", size = 0.3) + 
  tm_layout(bg.color = "grey20", 
            legend.position = c("LEFT", "bottom"),
            legend.text.color = "white", 
            legend.title.color = "white")













































