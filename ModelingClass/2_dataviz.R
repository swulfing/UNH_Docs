library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(patchwork)
library(visreg)
library(MASS)
library(nlme)
library(mgcv)

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
sdat2$log10_dist_logging <- log10(sdat2$dist_to_logging)

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

#Spatial Autocorrelation
sdat2$resid_glm_pois <- resid(glm1_pois)

tland + 
  tm_shape(sdat2) + 
  tm_symbols(col = "resid_glm_pois", size = 0.5)

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


