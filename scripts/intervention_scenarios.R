# intervention scenarios

econ.incentive <- raster("data/raster_layers/econ_incentive_layer.tif")
plot(econ.incentive)

tribal.wildlife <- raster("data/raster_layers/tribal_wildlife_gov_jf.tif")
plot(tribal.wildlife)
tribal.resist <- 1-tribal.wildlife
plot(tribal.resist)

social.composite <- raster("data/raster_layers/social_composite_layer.tif")

# not sure how to do this part
tribal.scenario <- social.composite-tribal.wildlife
plot(tribal.scenario)
writeRaster(tribal.scenario, "data/raster_layers/tribal_scenario.tif")

econ.scenario <- social.composite-econ.incentive
plot(econ.scenario)
# econ incentive values are v small
