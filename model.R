require(INLA)
library(ggregplot)
#remotes::install_github("gfalbery/ggregplot")
inla.setOption(scale.model.default=FALSE)



grid <- st_make_grid(nuts3, what="centers", n = 200)
grid <- grid[nuts3]


grid_sf <- st_as_sf(grid)
grid_sf$prec <- raster::extract(prec, grid_sf)
grid_sf$alt <- raster::extract(alt, grid_sf)
grid_sf$tmin <- raster::extract(tmin, grid_sf)
grid_sf$tmax <- raster::extract(tmax, grid_sf)
grid_sf$wind <- raster::extract(wind, grid_sf)
grid_sf <- st_join(grid_sf, nuts3)
grid_sf[,to_scale] <- scale(st_drop_geometry(grid_sf[,to_scale]),center=mean_covariates, scale=sd_covariates)




cov_names <- c("alt", "prec", "tmin", "tmax", "gdp", "pop_density", "popgdp", "business_size")


mesh <- inla.mesh.2d(loc=coordinates, 
                     max.n = 50, 
                     #max.edge=c(50, 1000),
                     offset=c(1, 1),
                              #loc.domain=borders, offset=c(10, 140), max.edge=c(50, 1000), 
                     )
plot(mesh)
points(coordinates, pch=20, cex=1)

spde <- inla.spde2.matern(mesh=mesh, alpha=2)
spde

n_days <- max(data$time)
i_day <- 5
A_est <- inla.spde.make.A(mesh=mesh,
                          loc=apply(coordinates, 2, rep, times=n_days),
                          group=data$time,
                          n.group=n_days)

dim(A_est)

s_index <- inla.spde.make.index(name="spatial.field",
                                n.spde=spde$n.spde,
                                n.group=n_days)

names(s_index)



stack_est <- inla.stack(data=list(logPM10=data$logPM10),
                        A=list(A_est, 1),
                        effects=list(c(s_index,list(Intercept=1)), list(data[,cov_names])), tag="est")




A_pred <- inla.spde.make.A(mesh=mesh,
                           st_coordinates(grid_sf),
                           group=i_day,  #selected day for prediction
                           n.group=n_days)

stack_pred <- inla.stack(data=list(logPM10=NA),
                         A=list(A_pred,1),
                         effects=list(c(s_index,list(Intercept=1)), list(st_drop_geometry(grid_sf))),
                         tag="pred")



stack <- inla.stack(stack_est, stack_pred)

formula <- logPM10 ~ -1 + Intercept + alt + prec + tmin + tmax + gdp + pop_density + popgdp + business_size +
  f(spatial.field, model=spde,group=spatial.field.group, control.group=list(model="ar1"))

output <- inla(formula,
               data=inla.stack.data(stack, spde=spde),
               family="gaussian",
               control.predictor=list(A=inla.stack.A(stack), compute=TRUE), 
               control.compute=list(dic=TRUE),
               verbose=T)  


summary(output)

output$summary.fixed
output$summary.linear.predictor
nrow(output$summary.fitted.values)



# Fixed effects betas
fixed.out <- round(output$summary.fixed,3)
fixed.out
# Hyperparameters sigma2eps and AR(1) a
rownames(output$summary.hyperpar)

sigma2e_marg <- inla.tmarginal(function(x) 1/x,output$marginals.hyperpar[[1]])
sigma2e_m1 <- inla.emarginal(function(x) x, sigma2e_marg)
sigma2e_m2 <- inla.emarginal(function(x) x^2, sigma2e_marg)
sigma2e_stdev <- sqrt(sigma2e_m2 - sigma2e_m1^2)
sigma2e_quantiles <- inla.qmarginal(c(0.025, 0.5, 0.975), sigma2e_marg)

ar <- output$summary.hyperpar["GroupRho for spatial.field",]
ar
# Spatial parameters sigma2 and range
mod.field <- inla.spde2.result(output, name="spatial.field", spde)

var.nom.marg <- mod.field$marginals.variance.nominal[[1]]
var.nom.m1 <- inla.emarginal(function(x) x, var.nom.marg)
var.nom.m2 <- inla.emarginal(function(x) x^2, var.nom.marg)
var.nom.stdev <- sqrt(var.nom.m2 - var.nom.m1^2)
var.nom.quantiles <- inla.qmarginal(c(0.025, 0.5, 0.975), var.nom.marg)

range.nom.marg <- mod.field$marginals.range.nominal[[1]]
range.nom.m1 <- inla.emarginal(function(x) x, range.nom.marg)
range.nom.m2 <- inla.emarginal(function(x) x^2, range.nom.marg)
range.nom.stdev <- sqrt(range.nom.m2 - range.nom.m1^2)
range.nom.quantiles <- inla.qmarginal(c(0.025, 0.5, 0.975), range.nom.marg)

index_pred <- inla.stack.index(stack,"pred")$data




lp_marginals <- output$marginals.linear.predictor[index_pred]

lp_mean <- unlist(lapply(lp_marginals, function(x) inla.emarginal(exp, x)))
grid_sf$lp_mean <- lp_mean

plot(grid_sf$lp_mean)

ggplot() +
  geom_sf(data=grid_sf, aes(color=lp_mean)) + 
  geom_sf(data=st_as_sf(data), aes(size=value), alpha= 0.1) 


grid_sf %>% 
  mutate(check = lp_mean > 50) %>% 
ggplot() +
  geom_sf(aes(color=check))



grid_sf$verypoor  <- 1-sapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,log(100)))
ggplot(grid_sf) +
  geom_sf(aes(alpha=verypoor), color="#A4122E")

grid_sf$poor  <- 1-sapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,log(50)))-grid_sf$verypoor
ggplot(grid_sf) +
  geom_sf(aes(alpha=poor), color="#FE5050")

grid_sf$moderate  <- 1-sapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,log(40)))-grid_sf$poor
ggplot(grid_sf) +
  geom_sf(aes(alpha=moderate), color="#EEE741")

grid_sf$fair  <- 1-sapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,log(20)))-grid_sf$moderate
ggplot(grid_sf) +
  geom_sf(aes(alpha=fair), color="#51CDAA")

grid_sf$good  <- sapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,log(20)))
ggplot(grid_sf) +
  geom_sf(aes(alpha=good), color="#52F0E6")






grid_tile <- grid_sf
grid_tile$x <- st_coordinates(grid_sf)[,1]
grid_tile$y <- st_coordinates(grid_sf)[,2]
ggplot(grid_tile) + 
  geom_tile(aes(x, y, alpha=good), fill="#52F0E6") +
  geom_tile(aes(x, y, alpha=fair), fill="#51CDAA") +
  geom_tile(aes(x, y, alpha=moderate), fill="#EEE741") +
  geom_tile(aes(x, y, alpha=poor), fill="#FE5050") +
  geom_tile(aes(x, y, alpha=verypoor), fill="#A4122E") +
  coord_fixed(ratio = 1.4) +
  theme_void()+
  scale_alpha(range = c(0, 1))


nuts3 <- st_as_sf(nuts3)

a <- ggplot(grid_tile) + geom_tile(aes(x, y, fill = good)) +
  coord_fixed(ratio = 1.4) +
  scale_fill_gradient(
    name = "Prob",
    low = "black", high = "#52F0E6"
  ) +
  theme_void() +
  ggtitle("Good")+
  scale_alpha(range = c(0, 1)) +
  theme(legend.position = "none")

b <- ggplot(grid_tile) + geom_tile(aes(x, y, fill = fair)) +
  coord_fixed(ratio = 1.4) +
  scale_fill_gradient(
    name = "Prob",
    low = "black", high = "#51CDAA"
  ) +
  theme_void() +
  ggtitle("Fair")+
  scale_alpha(range = c(0, 1))+
  theme(legend.position = "none")


c <- ggplot(grid_tile) + geom_tile(aes(x, y, fill = moderate)) +
  coord_fixed(ratio = 1.4) +
  scale_fill_gradient(
    name = "Prob",
    low = "black", high = "#EEE741"
  ) +
  theme_void() +
  ggtitle("Moderate")+
  scale_alpha(range = c(0, 1))+
  theme(legend.position = "none")


d <- ggplot(grid_tile) + geom_tile(aes(x, y, fill = poor)) +
  coord_fixed(ratio = 1.4) +
  scale_fill_gradient(
    name = "Prob",
    low = "black", high = "#FE5050"
  ) +
  theme_void() +
  ggtitle("Poor")+
  scale_alpha(range = c(0, 1))+
  theme(legend.position = "none")

e <- ggplot(grid_tile) + geom_tile(aes(x, y, fill = verypoor)) +
  coord_fixed(ratio = 1.4) +
  scale_fill_gradient(
    name = "Prob",
    low = "black", high = "#A4122E"
  ) +
  theme_void() +
  ggtitle("Very Poor")+
  scale_alpha(range = c(0, 1)) +
  theme(legend.position = "none")

library(patchwork)
(a | b | c | d | e)

# 
# 
# new_extent <- extent(5, 20, 36, 48)
# class(new_extent)
# 
# r <- raster(ncols=100, nrows=100, xmn=5, xmx=20, ymn=36, ymx=48)
# raster::rasterize(grid_sf$poor, r)
# stars::st_rasterize(grid_sf$poor)
# library(stars)
# 
# raster <- st_rasterize(grid_sf)
# plot(raster)


###############################à
# 
# # Select only points inside Piemonte and set NA to the outer points 
# lp_grid_mean[index_mountains] <- NA
# library(splancs)
# inside_Piemonte <- matrix(inout(Piemonte_grid, borders), 56, 72, byrow=T)
# inside_Piemonte[inside_Piemonte==0] <- NA
# inside_lp_grid_mean <- inside_Piemonte *  lp_grid_mean
# 
# seq.x.grid <- seq(range(Piemonte_grid[,1])[1],range(Piemonte_grid[,1])[2],length=56)
# seq.y.grid <- seq(range(Piemonte_grid[,2])[1],range(Piemonte_grid[,2])[2],length=72)
# 
# # *** Code for Figure 7.9
# print(levelplot(x=inside_lp_grid_mean,
#                 row.values=seq.x.grid,
#                 column.values=seq.y.grid,
#                 ylim=c(4875,5159), xlim=c(309,529),
#                 col.regions=gray(seq(.9,.2,l=100)),
#                 aspect="iso",
#                 contour=TRUE, labels=FALSE, pretty=TRUE, 
#                 xlab="",ylab=""))
# trellis.focus("panel", 1, 1, highlight=FALSE)
# lpoints(borders,col=1,cex=.25)
# 
# lpoints(coordinates$UTMX, coordinates$UTMY,col=1,lwd=2,pch=21)
# trellis.unfocus()
# 
# # ***
# 
# # *** Code for Figure 7.10
# threshold <- log(50)
# prob  <- lapply(X=lp_marginals, FUN=function(x) inla.pmarginal(marginal=x,threshold))
# tailprob_grid <- matrix(1-unlist(prob),56,72, byrow=T)
# 
# tailprob_grid[index_mountains] <- NA
# inside_tailprob_grid <- inside_Piemonte *  tailprob_grid
# 
# print(levelplot(x=inside_tailprob_grid,
#                 row.values=seq.x.grid,
#                 column.values=seq.y.grid,
#                 ylim=c(4875,5159), xlim=c(309,529),
#                 at=seq(0,1,by=.1),
#                 col.regions=gray(seq(.9,.2,l=100)),
#                 aspect="iso",
#                 contour=TRUE, labels=FALSE, pretty=TRUE, 
#                 xlab="",ylab=""))
# trellis.focus("panel", 1, 1, highlight=FALSE)
# lpoints(borders,col=1,cex=.25)
# lpoints(coordinates$UTMX, coordinates$UTMY,col=1,lwd=2,pch=21)
# trellis.unfocus()
# # ***
# 
