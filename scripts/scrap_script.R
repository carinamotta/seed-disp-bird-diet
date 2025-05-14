# 7. CAPTURE X DIET -----

metabarcoding.plots <- metabarcoding %>%
  filter(final_fate == 1) %>%
  left_join(captures %>% select(sample, plot), by = "sample")

metabarcoding.plots <- metabarcoding.plots %>% 
  select(15, 11, 12, 3)

seed.morpho.plot <- seed.morpho %>% 
  select(3, 10, 11, 5)

colnames(seed.morpho.plot)[c(2:3)] <-  
  c("plant_family", "plant_genus")

all.plants.plots <- rbind(metabarcoding.plots, seed.morpho.plot)

all.plants.plots.u <- all.plants.plots %>%
  distinct(plot, plant_family, plant_genus, sample, .keep_all = T)

genera.traits.new.filtered <- genera.traits %>%
  filter(!is.na(growth_form))

plants.traits.plots <- all.plants.plots.u  %>%
  left_join(genera.traits.new.filtered %>% select(plant_genus, growth_form, origin, fruit_type), by = "plant_genus")

plants.traits.plots.gen <- plants.traits.plots %>%
  filter(!is.na(growth_form))

plants.traits.plots.gen.summary <- plants.traits.plots.gen %>%
  group_by(plot) %>%
  reframe(plant_genus = n_distinct(plant_genus),
          plant_family = n_distinct(plant_family),
          no_trees = sum(growth_form == "tree", na.rm = T),
          no_shrub = sum(growth_form == "shrub", na.rm = T),
          no_climb = sum(growth_form == "climbing", na.rm = T),
          no_herb = sum(growth_form == "herbaceous", na.rm = T),
          no_native = sum(origin == "native", na.rm = T),
          no_exotic = sum(origin == "exotic", na.rm = T),
          no_fleshy = sum(fruit_type == "fleshy", na.rm = T),
          no_dry = sum(fruit_type == "dry", na.rm = T))

plant.plot.summary <- plants.traits.plots.gen.summary %>%
  left_join(plot.summary, by = "plot")

plant.plot.summary$prop.ex <- (plant.plot.summary$no_exotic/plant.plot.summary$plant_genus)

ggplot(plant.plot.summary, aes(x = forest_cover, y = no_exotic)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

gam_exotic_cover <- mgcv::gam(no_exotic ~ s(forest_cover), 
                              family = mgcv::nb(), 
                              data = plant.plot.summary)

summary(gam_exotic_cover)

ggplot(plant.plot.summary, aes(x = age_updated, y = no_exotic)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

gam_exotic_age <- mgcv::gam(no_exotic ~ s(age_updated), 
                            family = mgcv::nb(), 
                            data = plant.plot.summary)

summary(gam_exotic_age)

lm(cbind(no_exotic, no_native) ~ age_updated + forest_cover + 
     no_ind_zoo + no_spp_zoo,
   family = binomial, 
   data = plant.plot.summary )


## SDE X no. genera detected---- 

ggplot(metabarcoding.plot.summary, aes(x = plant_genus, y = SDE_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(metabarcoding.plot.summary$plant_genus) 

shapiro.test(metabarcoding.plot.summary$SDE_all)

cor_SDE_plant_genera <- cor.test(metabarcoding.plot.summary$SDE_all, (metabarcoding.plot.summary$plant_genus), method = "pearson")

print(cor_SDE_plant_genera)

## no. genera exotic X cover----
## 
# 
# proportion of exotic was not signifcant, but the number of exotics is 
# 
# metabarcoding.plot.summary$exotic.prop <- metabarcoding.plot.summary$no_exotic/metabarcoding.plot.summary$plant_genus


ggplot(metabarcoding.plot.summary, aes(x = Mapbiomas_1km_forestcover, 
                                       y = no_exotic)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(metabarcoding.plot.summary$no_exotic) #not normal

metabarcoding.plot.summary$log_no_exotic <- 
  (log10(metabarcoding.plot.summary$no_exotic+1))

shapiro.test(metabarcoding.plot.summary$log_no_exotic)

#now normal after transforming

#I used a linear model, but the relationship does not look linear

lm_cor_exotic_age <- lm(log_no_exotic ~ Mapbiomas_1km_forestcover + age_updated, 
                        data = metabarcoding.plot.summary)

summary(lm_cor_exotic_age)

# tested two non linear models with untransformed data
# 

#library(MASS)

# Fit a negative binomial model
nb_exotic_cover <- glm.nb(no_exotic ~ Mapbiomas_1km_forestcover, 
                          data = metabarcoding.plot.summary)

# Check the summary
summary(nb_exotic_cover)

#library(mgcv)

# Fit a GAM with a smoothing term on age
gam_exotic_cover <- mgcv::gam(no_exotic ~ s(Mapbiomas_1km_forestcover), 
                              family = mgcv::nb(), 
                              data = metabarcoding.plot.summary)

# Summary and plot
summary(gam_exotic_cover)
#plot(gam_model, residuals = TRUE, pch = 16)

AIC(nb_model, gam_model)

# GAM > than NB model, but NB significant 


## no. genera exotic X age----


# 
# metabarcoding.plot.summary$exotic.prop <- metabarcoding.plot.summary$no_exotic/metabarcoding.plot.summary$plant_genus


ggplot(metabarcoding.plot.summary, aes(x = age_updated, 
                                       y = no_exotic)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(metabarcoding.plot.summary$no_exotic) #not normal

metabarcoding.plot.summary$log_no_exotic <- 
  (log10(metabarcoding.plot.summary$no_exotic+1))

shapiro.test(metabarcoding.plot.summary$log_no_exotic)

#now normal after transforming

#I used a linear model, but the relationship does not look linear

lm_cor_exotic_age <- lm(log_no_exotic ~ Mapbiomas_1km_forestcover + age_updated, 
                        data = metabarcoding.plot.summary)

summary(lm_cor_exotic_age)

# tested two non linear models with untransformed data
# 

library(MASS)

# Fit a negative binomial model
nb_exotic_age <- glm.nb(no_exotic ~ age_updated, 
                        data = metabarcoding.plot.summary)

# Check the summary
summary(nb_exotic_age)

library(mgcv)

# Fit a GAM with a smoothing term on age
gam_exotic_age <- mgcv::gam(no_exotic ~ s(age_updated), family = mgcv::nb(), 
                            data = metabarcoding.plot.summary)

# Summary and plot
summary(gam_exotic_age)
#plot(gam_model, residuals = TRUE, pch = 16)

AIC(nb_model, gam_model)

# GAM > than NB model, but NB significant 


## no. genera dry x age----

ggplot(metabarcoding.plot.summary, aes(x = age_updated, y = no_dry)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

## MAY 1, 2025----
## 
## 

#install.packages("betareg")
library(betareg)

n <- metabarcoding.plot.summary$plant_genus

# n is the sample size (you can also use a constant like 0.001 instead)
metabarcoding.plot.summary$exotic.prop_adj <- (metabarcoding.plot.summary$exotic.prop * (n - 1) + 0.5) / n


model <- betareg(exotic.prop_adj ~ Mapbiomas_1km_forestcover + age_updated, data = metabarcoding.plot.summary)
summary(model)

##
##
# If using raw exotic count, include total richness as offset:
glm_model <- glm(no_exotic ~ Mapbiomas_1km_forestcover + age_updated, offset = log(plant_genus), family = poisson, data = metabarcoding.plot.summary)

summary(glm_model)

# Or include total richness as a covariate in a model:
lm_model <- lm(exotic.prop ~ Mapbiomas_1km_forestcover + age_updated + plant_genus, 
               data = metabarcoding.plot.summary)

summary(lm_model)

## no. genera detected X zoo tree abundance-----

ggplot(metabarcoding.plot.summary, aes(x = no_spp_zoo, y = plant_genus)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(metabarcoding.plot.summary$plant_genus) 

cor_SDE_plant_genera <- cor.test(metabarcoding.plot.summary$no_spp_zoo, (metabarcoding.plot.summary$plant_genus), method = "pearson")

print(cor_SDE_plant_genera)

## no. family detected X zoo tree abundance-----

ggplot(metabarcoding.plot.summary, aes(x = no_spp_zoo, y = plant_family)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(metabarcoding.plot.summary$plant_family) 

cor_SDE_plant_family <- cor.test(metabarcoding.plot.summary$no_spp_zoo, (metabarcoding.plot.summary$plant_family), method = "pearson")

print(cor_SDE_plant_family)

# May 5, 2025------

metabarcoding.plot.summary$dry.prop <- metabarcoding.plot.summary$no_dry/metabarcoding.plot.summary$no_fleshy

#SCRAP -----
# ## Spp. comp between sites
# 
# spp.plot.matrix <- captures %>%
#   group_by(plot, species) %>% 
#   summarise(sample = n_distinct(sample)) %>% # this just combines all the quantities from the observations into one value per family
#   arrange(plot) %>% #sinc I imagine you will want to visualize this in order of different insect orders, this and the next line of code make sure that happens
#   pivot_wider(names_from = species, #makes this in to a matrix with this column name
#               values_from = sample) %>% #and values in cells filled with this column
#   column_to_rownames(var = "plot")
# 
# spp.plot.matrix[is.na(spp.plot.matrix)] <- 0
# 
# dissimilarity <- vegdist(spp.plot.matrix, method = "bray")
# 
# # Step 3: Check for homogeneity of dispersion
# dispersion_test <- betadisper(dissimilarity, spp.plot.matrix$plot)
# anova(dispersion_test)
# 
# permanova_result <- adonis2(species ~ plot, data = site_data, method = "bray")
# print(permanova_result)