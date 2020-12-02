require(INLA)
library(ggregplot)
#remotes::install_github("gfalbery/ggregplot")

grid <- st_make_grid(nuts3, what="centers", n = 100)
grid <- grid[nuts3]


grid_sf <- st_as_sf(grid)
grid_sf$prec <- raster::extract(prec, grid_sf)
grid_sf$alt <- raster::extract(alt, grid_sf)
grid_sf$tmin <- raster::extract(tmin, grid_sf)
grid_sf$tmax <- raster::extract(tmax, grid_sf)
grid_sf$wind <- raster::extract(wind, grid_sf)
grid_sf <- st_join(grid_sf, nuts3)
grid_sf[,to_scale] <- scale(st_drop_geometry(grid_sf[,to_scale]),center=mean_covariates, scale=sd_covariates)




inla.setOption(scale.model.default=FALSE)

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

formula2 <- logPM10 ~ -1 + Intercept + alt + prec +
  f(spatial.field, model=spde,group=spatial.field.group, control.group=list(model="ar1"))

formula3 <- logPM10 ~ -1 + Intercept + alt + prec +
  f(spatial.field, model=spde,group=spatial.field.group)


formula4 <- logPM10 ~ -1 + Intercept + alt + prec


output <- inla(formula,
               data=inla.stack.data(stack, spde=spde),
               family="gaussian",
               control.predictor=list(A=inla.stack.A(stack), compute=TRUE), 
               control.compute=list(dic=TRUE),
               verbose=T)  

output2 <- inla(formula2,
               data=inla.stack.data(stack, spde=spde),
               family="gaussian",
               control.predictor=list(A=inla.stack.A(stack), compute=TRUE),
               control.compute=list(dic=TRUE),
               verbose=T)

output3 <- inla(formula3,
                data=inla.stack.data(stack, spde=spde),
                family="gaussian",
                control.predictor=list(A=inla.stack.A(stack), compute=TRUE),
                control.compute=list(dic=TRUE),
                verbose=T)

output4 <- inla(formula4,
                data=inla.stack.data(stack, spde=spde),
                family="gaussian",
                control.predictor=list(A=inla.stack.A(stack), compute=TRUE),
                control.compute=list(dic=TRUE),
                verbose=T)


summary(output)
summary(output2)
summary(output3)
summary(output4)

models <- list(output, output2, output3, output4)


Efxplot(models)

sapply(models, function(f) f$dic$dic)

INLADICFig(models, ModelNames = c("Tutte covariate", "Solo significative", "Significative + spazio (no tempo)", "Significative (no spazio no tempo)"), Legend=F) + 
  theme_minimal()