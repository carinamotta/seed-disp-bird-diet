# Created: March 27, 2025
# Updated: May 14, 2025
# 
# Author: Carina Isabella Motta
# 
# Seed Dispersal 

# 1. LOAD PACKAGES -------------------------------------------------------------

# a vector listing package names needed 

package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #species accumulation curves
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "ggplot2", #plots
                  "car", #check for collinearity between predictor variables
                  "effect.lndscp", #seed dispersal effectiveness (SDE) 
                  "ggpubr", #arrange multiple graphs
                  "glmmTMB" #zi model to test for differences 
)

#creating another list of new packages (if there are any)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]

#installing the packages if they aren't already on the computer
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

# 2. LOAD DATA  ----------------------------------------------------------------
## 2.1 Capture data----

# captures
captures <- readr::read_csv(here::here("data", "raw_data",
                                       "bird_captures_14MAY2025.csv"))

# corresponding family names 
bird.spp.fam <- readr::read_csv(here::here("data", "raw_data",
                                           "bird_spp_fam_14MAY2025.csv"))

## 2.2 Droppings and seed sample data------

#seed count and identification 
seeds <- readr::read_csv(here::here("data", "raw_data",
                                    "droppings_seeds_14MAY2025.csv"))

#seed morphospecies 
morpho <- readr::read_csv(here::here("data", "raw_data",
                                     "droppings_seeds_morphotypes_14MAY2025.csv"))

## 2.3 Bird trait data----

#load EltonTraits
elton.traits <- readr::read_tsv(here::here("data", "external_data", 
                                           "EltonTraits_14MAY2025.txt"))

## 2.4 Plot data----

#plot survey data
plots <- readr::read_csv(here::here("data", "external_data",
                                    "newfor_database_14MAY2025.csv"))

#plot metadata
plot.metadata <- readr::read_csv(here::here("data", "external_data",
                                            "newfor_plots_metadata_14MAY2025_2.csv"))

## 2.5 Tree dispersal data----

#whether the tree is zoochorous or not 
dispersal <- readr::read_csv(here::here("data", "external_data",
                                        "dispersal_14MAY2025.csv"))

# 3. CLEAN AND PREP DATA ####
## 3.1 Capture data ------

#transform into a tibble to facilitate using packages like dyplr 
captures <- as_tibble(captures)

#cut excess rows
captures <- captures[-(492:999),]

#cut excess columns
captures <- captures[,-(28:36)]

#subset captures to only include certain columns: capture date, session, day, plot, time the net was opened and time net was closed, species, sample number, whether feces were collected, if the bird is a recap  (y = 1/n = 0), and if the bird is frugivorous
subset.caps <- captures %>% 
  select(1:6, 11:12, 19, 23, 25, 26)

#cut excess column
bird.spp.fam <- bird.spp.fam[,-(3)]

#add to the capture dataset
subset.caps <- subset.caps %>%
  left_join(bird.spp.fam, by = "bird_species") %>%
  relocate(bird_family, .after = nets_closed) 

## 3.2 Sample data----

#join seed count and morphospecies data 

#transform into a tibble to facilitate using packages like dyplr 
morpho <- as_tibble(morpho)

#cut excess rows
morpho <- morpho[-(42:987),]

#select columns of interest
subset.morpho <- morpho %>% 
  select(1:5)

#select columns of interest
subset.seeds <- seeds %>% 
  select(1:7)

#merge seed and morphospp data 
seed.morpho <- merge(x=subset.seeds, y=subset.morpho,
                     by="seed_morpho", all.x =T)

## 3.3 Merge capture and sample data-----

#append seed data to subset.caps
subset.caps$seeds <- ifelse(subset.caps$sample %in% seed.morpho$sample, 1, 0)


seeds.sample <- seed.morpho %>%
  group_by(sample) %>%
  summarise(no_seeds = sum(no_seeds),
            no_morpho_spp = max(no_morpho))

captures.seeds <-  subset.caps %>%
  left_join(seeds.sample, by = "sample")

#fill NAs with 0s
captures.seeds$no_seeds[is.na(captures.seeds$no_seeds)] <- 0

#fill NAs with 0s
captures.seeds$no_morpho_spp[is.na(captures.seeds$no_morpho_spp)] <- 0

#reorder the columns
captures.seeds <- captures.seeds[,c(1:9, 11:16, 10)]


## 3.4 Bird diet and trait data----

#select the columns that interest us: family Latin name, species Latin name, 
#% of diet that is fruit, diet category
subset.traits <- elton.traits %>% select(8, 16, 20, 26, 27, 28, 29 ,30, 36)

#cut excess rows
subset.traits <- subset.traits[-(9994:9995),]

colnames(subset.traits)[c(1:9)] <-  
  c("bird_species", "frug_degree", "guild", "fs_ground", "fs_understory",
    "fs_midhigh", "fs_canopy", "fs_aerial", "mass_elton")

#correct species names using updated names from Cornell Lab of Ornithology, Clements Checklist 2024 (https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2024/2024-citation-checklist-downloads/)

subset.traits <- subset.traits %>% 
  mutate(bird_species = str_replace_all(bird_species, c("Veniliornis passerinus"= 
                                                "Dryobates passerinus" , 
                                              "Hylocryptus rectirostris"=
                                                "Clibanornis rectirostris",
                                              "Tangara cayana" =
                                                "Stilpnia cayana", 
                                              "Basileuterus flaveolus"=
                                                "Myiothlypis flaveola",       
                                              "Tiaris fuliginosus" =
                                                "Asemospiza fuliginosa",
                                              "Antilophia galeata" =
                                                "Chiroxiphia galeata",
                                              "Troglodytes aedon" =
                                                "Troglodytes musculus")))


#add the hybrid Basileuterus flaveolus x Basileuterus culicivorus and 
#genus level classification of Sporophila species to elton traits so it can
#match up with our list

#weight is average from our captures
subset.traits <- rbind(subset.traits, list('Basileuterus culicivorus x Myiothlypis flaveola', 20, "Invertebrate", 0, 0, 0, 0, 0, 13.13))

subset.traits <- rbind(subset.traits, list('Sporophila sp.', 0, "PlantSeed", 0, 0, 0, 0, 0, 12.28))

filtered.traits.data <- subset.traits %>%
  filter(bird_species %in% subset.caps$bird_species)



## 3.5 Plot data----

#plot survey data as tibble
plots <- as_tibble(plots)

#subset
plot.spps <- plots %>% 
  select(1, 8, 10:14)

#correct column names  
colnames(plot.spps)[c(1:7)] <-  
  c("date", "plot", "tag", "spp_code", "family", "species", "origin")

#plot metadata as tibble
plot.metadata <- as_tibble(plot.metadata)


#update plot age by adding 9 years (data originally collected in 2015/2016)
plot.metadata$age_updated <- (plot.metadata$age + 9)

## 3.6 Tree dispersal syndrome data ----

#correct column names  
colnames(dispersal)[c(1:5)] <-  
  c("spp_code", "family", "species", "zoo_disp", "disp_agent")

## 3.7 Append dispersal syndrome data to plot data----
#add whether species is dispersed by zoo
plot.spps$zoo_disp <- ifelse(plot.spps$species %in% dispersal$species, 1, 0)

# 4. INDIVIDUAL-LEVEL ANALYSES ####

## 4.1 Captures, feces samples, & seeds collected------

#filter out the no capture days
caps.summary <- captures.seeds[!grepl("no_captures", 
                                      captures.seeds$bird_species),]

caps.total.summary <- caps.summary %>%
  summarise(n_caps = n_distinct(sample),
            n_recaps = sum(recapture == 1, na.rm = T),
            n_caps_frug = sum(frugivorous == 1, na.rm = T),
            n_spp_birds = n_distinct(bird_species),
            n_spp_frug = n_distinct(bird_species[frugivorous == 1]),
            n_samples = sum(feces_collected == "y", na.rm = T),
            n_samples_frug = sum(feces_collected == "y" & frugivorous == 1),
            n_samples_seeds = sum(seeds == 1, na.rm = T),
            n_seeds = sum(no_seeds)
            
  )

## 4.2 Net hours-----

net.hours <- captures.seeds %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(
    nets_opened = as.POSIXct(nets_opened, format = "%H:%M:%S"), 
    nets_closed = as.POSIXct(nets_closed, format = "%H:%M:%S"),
    net_hours = as.numeric(difftime(nets_closed, nets_opened, 
                                    units = "hours")) * 5
  )

# sum total net hours
net.hours.total <- sum(net.hours$net_hours)

# Print result
print(net.hours.total) #2763

caps.total.summary$net_hours <- sum(net.hours$net_hours)

caps.total.summary$cap_rate <- caps.total.summary$n_caps/caps.total.summary$net_hours

caps.total.summary$cap_rate_frug <- caps.total.summary$n_caps_frug/caps.total.summary$net_hours

# 5. SPECIES-LEVEL ANALYSES ####

## 5.1 Captures, feces samples, & seeds collected----- 

#create a list of species captured, with the number of captures, number of samples, and the mass of the species 
caps.spp.summary <- caps.summary %>%
  group_by(bird_species, bird_family, frugivorous) %>%
  summarise(
    no_caps = n(),  # Counts the number of captures for each species
    no_samples = sum(feces_collected == "y", na.rm = T), 
    mass_caps = mean(mass, na.rm = TRUE)
  ) %>%
  ungroup()

#summarize samples with seeds and number of seeds per species to create a species-level summary table 
seeds.summary <- subset.seeds %>%
  group_by(bird_species) %>%
  summarise(
    no_samples_seeds = n_distinct(sample),  
    no_seed_total = sum(no_seeds) 
  ) %>%
  ungroup()

#merge seeds summary and species capture summary 
caps.spp.seed.summary <- merge(x=caps.spp.summary, y=seeds.summary,
                               by="bird_species", all.x =T)

#fill NAs with 0s
caps.spp.seed.summary[is.na(caps.spp.seed.summary)] <- 0

#reorder the columns
species.summary <- caps.spp.seed.summary[,c(2, 1, 4, 5, 7, 8, 3, 6)]

## 5.2 Capture rate-----

#calculate the capture rate by dividing the number of captures by the total number of net hours 
species.summary$cap_rate <- (species.summary$no_caps/2763) 

## 5.3 Seed dispersal frequency (SDF)------

#calculate seed dispersal frequency (SDF) on a species level --> number of feces with seeds / total number of feces collected per species 
species.summary$SDF <- (species.summary$no_samples_seeds/species.summary$no_samples)

#substitute infinite values for 0
species.summary$SDF[!is.finite(species.summary$SDF)] <- 0

## 5.4 Seed dispersal effectiveness (SDE)----

#calculate seed dispersal effectiveness by multiplying SDF by the capture rate
species.summary$SDE <- species.summary$SDF*species.summary$cap_rate

#filter bird species that actually had seeds and a SDE value 
species.summary.seeds <- species.summary %>%
  filter(no_samples_seeds >= 1)

error_SDF <- captures.seeds %>%
  group_by(bird_species) %>%
  summarize(SDF_se = sd(seeds) / sqrt(n()))

species.summary.seeds <- merge(x=species.summary.seeds, y=error_SDF,
                                by="bird_species", all.x =T)

species.summary.seeds[is.na(species.summary.seeds)] <- 0

net.hours.day <- captures.seeds %>%
  distinct(day, .keep_all = TRUE) %>%
  mutate(
    nets_opened = as.POSIXct(nets_opened, format = "%H:%M:%S"), 
    nets_closed = as.POSIXct(nets_closed, format = "%H:%M:%S"),
    net_hours = as.numeric(difftime(nets_closed, nets_opened, 
                                    units = "hours")) * 5
  )

all_dates <- unique(captures.seeds$day)
all_species <- unique(captures.seeds$bird_species)

full_grid <- expand.grid(day = all_dates, bird_species = all_species)

capture_summary <- captures.seeds |> 
  group_by(day, bird_species) |> 
  summarise(captures = n(), .groups = "drop") # or count only unique individuals

full_data_cap <- full_grid |> 
  left_join(capture_summary, by = c("day", "bird_species")) |> 
  mutate(captures = replace_na(captures, 0))

full_data_cap <- full_data_cap |> 
  left_join(net.hours.day  %>% select(day, net_hours),
            by = "day")

full_data_cap <- full_data_cap |> 
  mutate(capture_rate = captures / net_hours)

capture_rate_summary <- full_data_cap |> 
  group_by(bird_species) |> 
  summarise(
    mean_rate = mean(capture_rate),
    se_rate = sd(capture_rate) / sqrt(n())
  )

species.summary.seeds <- merge(x=species.summary.seeds, y=capture_rate_summary,
                               by="bird_species", all.x =T)


#plot SDE landscape-------
SDE_spp <- effectiveness_plot(species.summary.seeds$mean_rate, 
                              species.summary.seeds$SDF, 
                              q2.error = species.summary.seeds$SDF_se,
                              q1.error = species.summary.seeds$se_rate,
                              label = species.summary.seeds$bird_species,  
                              myxlab = 
                                "capture rate (no. captures/total net hours)", 
                              myylab = 
                                "frequency of seeds found in feces")

SDE_spp

ggsave(
  filename = here::here("results", "SDE_spp.svg"),
  plot = SDE_spp,
  width = 30, 
  height = 20, 
  units = "cm", 
  dpi = 720)

graphics.off()

## 5.5 SDE X Bird traits----

#merge bird traits dataset onto the species.summary 
species.summary.traits <- merge(x=species.summary.seeds, y=filtered.traits.data,
                                by="bird_species", all.x =T)


cor(species.summary.traits[, c("no_seed_total", "SDF")])

### 5.5.1 bird body mass-----

#start by visualizing the relationship between SDE and bird body mass

ggplot(species.summary.traits, aes(x = mass_elton, y = SDE)) +
  geom_point() +
  theme_minimal()

#test for normality
shapiro.test(species.summary.traits$SDE)
#not normal

#transform
species.summary.traits$SDE_log <- log10(species.summary.traits$SDE)

#test for normality again
shapiro.test(species.summary.traits$SDE_log)
#now it's normal

#test for normality
shapiro.test(species.summary.traits$mass_elton)
#normal

#simple linear regression
lm_SDE_bodymass <- lm(SDE_log ~ mass_elton, data = species.summary.traits)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm_SDE_bodymass)
dev.off()

summary(lm_SDE_bodymass)

SDE_bodymass <- ggplot(species.summary.traits,
                       aes(y = SDE_log,
                           x = mass_elton)) +
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") +
  geom_point(size =2, alpha = 1) +
  labs(y = "log(Species-level Seed Dispersal Effectiveness)",
       x = "Body Mass (g)")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 10),
         axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 10, 
                                     colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

SDE_bodymass


### 5.5.2 bird % diet frugivory----- 

#start by visualizing the relationship between SDE(untransformed) and % frugivory 

SDE.frugdegree <- ggplot(species.summary.traits, aes(x = frug_degree, y = SDE))+
  geom_point() +
  #geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

SDE.frugdegree

#test for normality
shapiro.test(species.summary.traits$frug_degree)
#normal

#simple linear regression with log transformed SDE 
lm.SDE.frugdegree <- lm(SDE_log ~ frug_degree, data = species.summary.traits)

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm_SDE_frugdegree)
dev.off()

summary(lm.SDE.frugdegree)

SDE_log.frugdegree <- ggplot(species.summary.traits,
                             aes(y = SDE_log,
                                 x = frug_degree)) +
  geom_smooth(method = "lm", alpha =0.51, color = "blue", se = T) +
  geom_text(aes(label = bird_species), vjust = -1, size = 3) +  # Add labels
  geom_point(size =2, alpha = 1) +
  labs(y = "log(Species-level Seed Dispersal Effectiveness)",
       x = "Frugivory Degree (%)")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 10),
         axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 10, 
                                     colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

SDE_log.frugdegree

SDE_log.frugdegree <- ggplot(species.summary.traits,
                             aes(y = SDE_log,
                                 x = frug_degree,
                                 color = frug_degree)) +  # color by a numeric column
  geom_smooth(method = "lm", alpha = 0.51, color = "blue") +
  geom_point(size = 2, alpha = 1) +
  geom_text(aes(label = bird_species), vjust = -1, size = 3) +  # Add labels
  scale_color_gradient(low = "#6B2C2C", high = "#FF3300") +  # adjust colors here
  labs(y = "log(Species-level Seed Dispersal Effectiveness)",
       x = "Frugivory Degree (%)",
       color = "Body Mass (g)") +
  theme(
    legend.position = "none",  # ← this hides the legend
    axis.text.x = element_text(face = "bold", colour = "black", size = 10),
    axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
    axis.title.x = element_text(face = "bold", size = 12, colour = "black"),
    axis.title.y = element_text(face = "bold", size = 10, colour = "black"),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.text = element_text(size = 9, face = "bold")
  )
SDE_log.frugdegree

# 6. PLOT-LEVEL ANALYSES ####

## 6.1 Captures, feces samples, & seeds collected----

#number of captures, recaps per plot, number of frugivorous captures, number of samples, number of samples from frugivores, number of samples with seeds
caps.plot.summary <- caps.summary %>% #use cap.summary, that already filtered out no capture days
  group_by(plot) %>%
  summarise(no_captures = n_distinct(sample),
            no_recaps = sum(recapture == 1, na.rm = T),
            no_caps_frug = sum(frugivorous == 1, na.rm = T),
            no_spp_birds = n_distinct(bird_species),
            no_spp_frug = n_distinct(bird_species[frugivorous == 1]),
            no_samples = sum(feces_collected == "y", na.rm = T),
            no_samples_frug = sum(feces_collected == "y" & frugivorous == 1),
            no_samples_seeds = sum(seeds == 1, na.rm = T)
            
  )

# append to plot.summary
plot.summary <- merge(x=plot.metadata, y=caps.plot.summary,
                      by="plot", all.x =T) 

## 6.2 Net hours----

# calculate net hours per plot
net.hours.plot <- net.hours %>%
  group_by(plot) %>%  # Group by plot
  summarise(total_net_hours = round(sum(net_hours)))  # Sum net hours per plot  

# append to plot.summary 
plot.summary <- merge(x=plot.summary, y=net.hours.plot,
                      by="plot", all.x =T) 

## 6.3 Capture rate-----

#all captures
plot.summary$cap_rate <- (plot.summary$no_caps/plot.summary$total_net_hours)

#frugivorous captures
plot.summary$cap_rate_frug <- (plot.summary$no_caps_frug/plot.summary$total_net_hours)

cor(plot.summary[, c("cap_rate", "cap_rate_frug")])

## 6.4 Seed dispersal frequency (SDF)-----

#calculate seed dispersal frequency (SDF) on a species level --> number of feces with seeds / total number of feces collected per species 

plot.summary$SDF_all <- (plot.summary$no_samples_seeds/plot.summary$no_samples)

plot.summary$SDF_frug <- (plot.summary$no_samples_seeds/plot.summary$no_samples_frug)

cor(plot.summary[, c("SDF_frug", "SDF_all")])

## 6.5 Seed dispersal effectiveness (SDE)------

plot.summary$SDE_all <- plot.summary$SDF_all*plot.summary$cap_rate

plot.summary$SDE_frug <- plot.summary$SDF_frug*plot.summary$cap_rate_frug

cor(plot.summary[, c("SDE_frug", "SDE_all")])

SDE_plot_all <- effectiveness_plot(plot.summary$cap_rate, 
                                   plot.summary$SDF_all, 
                                   label = plot.summary$plot,  
                                   myxlab = "capture rate (no. birds captured/total net hours)", 
                                   myylab = "frequency of seeds found in feces")

SDE_plot_all

SDE_plot_frug <- effectiveness_plot(plot.summary$cap_rate_frug, 
                                    plot.summary$SDF_frug, 
                                    label = plot.summary$plot,  
                                    myxlab = "capture rate, frugivores only (no. birds captured/total net hours)", 
                                    myylab = "frequency of seeds found in feces, frugivores only")

SDE_plot_frug


## 6.6 SDE X Habitat features-----

### CALCUATE NUMBER OF IND., SPP. and FRUG. SPP PER PLOT

plot.spp.abun.div <- plot.spps %>%
  group_by(plot) %>%
  summarise(no_ind_tree = n_distinct(tag),
            no_spp_tree = n_distinct(species),
            no_ind_zoo = n_distinct(tag[zoo_disp == 1]),
            no_spp_zoo = n_distinct(species[zoo_disp == 1]))

### MERGE INTO ONE TABLE

plot.summary <- merge(x=plot.summary, y=plot.spp.abun.div,
                      by="plot", all.x =T)

# Fit a multiple linear regression model (response variable doesn't matter for VIF calculation)
model.habitat.features <- lm(SDE_all ~ age_updated + no_ind_zoo + no_spp_zoo + forest_cover, data = plot.summary)

# Check VIF values for collinearity 
car::vif(model.habitat.features)

#all <2, no collinearity 

# Check normality of SDE
shapiro.test(plot.summary$SDE_all)  # normal!

### 6.6.1 plot age-----

ggplot(plot.summary, aes(x = age_updated, y = SDF_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of age
shapiro.test(plot.summary$SDF_all)  # normal!

cor_age_SDF <- cor.test(plot.summary$SDF_all, plot.summary$age_updated, 
                        method = "pearson")

print(cor_age_SDF)

ggplot(plot.summary, aes(x = age_updated, y = cap_rate)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

cor_age_cap_rate <- cor.test(plot.summary$cap_rate, plot.summary$age_updated, 
                        method = "pearson")

print(cor_age_cap_rate)

# Check normality of age
shapiro.test(plot.summary$cap_rate)  # normal!

#start by visualizing the relationship between SDE(untransformed) and % frugivory 

ggplot(plot.summary, aes(x = age_updated, y = SDE_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of age
shapiro.test(plot.summary$SDE_all)  # normal!

# p-values for significance testing
cor_age <- cor.test(plot.summary$SDE_all, plot.summary$age_updated, 
                    method = "pearson")

print(cor_age)

age.SDE <- ggplot(plot.summary, aes(y = SDE_all, x = age_updated)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Seed Dispersal Effectiveness", 
       x = "Sampling area age")+ 
  theme( axis.text.x = element_text(face = "bold", colour = "black", size = 12),          axis.text.y = element_text(face = "bold", size = 12, color = "black"),          axis.title.x = element_text(face = "bold", size = 14, color = "black"),          axis.title.y = element_text(face = "bold", size = 14, color = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

age.SDE


### 6.6.2 zoochorous tree species abundance----- 

ggplot(plot.summary, aes(x = no_ind_zoo, y = SDE_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of zoochorous tree species abundance
shapiro.test(plot.summary$no_ind_zoo)  # normal!

cor_zoo_abundance <- cor.test(plot.summary$SDE_all, plot.summary$no_ind_zoo, 
                              method = "pearson")

print(cor_zoo_abundance)

zoo.abun.SDE <- ggplot(plot.summary, aes(y = SDE_all, x = no_ind_zoo)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Seed Dispersal Effectiveness", 
       x = "Zoochoric Tree Species Abundance")+ 
  theme( axis.text.x = element_text(face = "bold", colour = "black", size = 12),          axis.text.y = element_text(face = "bold", size = 12, color = "black"),          axis.title.x = element_text(face = "bold", size = 14, color = "black"),          axis.title.y = element_text(face = "bold", size = 14, color = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

zoo.abun.SDE

### 6.6.3 zoochorous tree species diversity-----

ggplot(plot.summary, aes(x = no_spp_zoo, y = SDE_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of zoochorous tree species diversity
shapiro.test(plot.summary$no_spp_zoo)  # normal!

cor_zoo_diversity <- cor.test(plot.summary$SDE_all, plot.summary$no_spp_zoo, method = "pearson")

print(cor_zoo_diversity)

zoo.div.SDE <- ggplot(plot.summary, aes(y = SDE_all, x = no_spp_zoo)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Seed Dispersal Effectiveness", 
       x = "Zoochoric Tree Species Diversity")+ 
  theme( axis.text.x = element_text(face = "bold", colour = "black", size = 12),          axis.text.y = element_text(face = "bold", size = 12, color = "black"),          axis.title.x = element_text(face = "bold", size = 14, color = "black"),          axis.title.y = element_text(face = "bold", size = 14, color = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

zoo.div.SDE

### 6.6.4 plot connectivity----- 

ggplot(plot.summary, aes(x = forest_cover, y = SDF_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(plot.summary$forest_cover)  # normal!

cor_zoo_connectivity <- cor.test(plot.summary$SDE_all, (plot.summary$forest_cover), 
                                 method = "pearson")

print(cor_zoo_connectivity)

conn.SDE <- ggplot(plot.summary, aes(y = SDE_all, x = forest_cover)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Seed Dispersal Effectiveness", 
       x = "Sampling area connectivity")+ 
  theme( axis.text.x = element_text(face = "bold", colour = "black", size = 12),          axis.text.y = element_text(face = "bold", size = 12, color = "black"),          axis.title.x = element_text(face = "bold", size = 14, color = "black"),          axis.title.y = element_text(face = "bold", size = 14, color = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

conn.SDE


### 6.6.5 combine plots into one figure----
SDE.plots.all <- ggarrange(age.SDE, conn.SDE, zoo.abun.SDE, 
                              zoo.div.SDE, 
                              labels = c("A", "B", "C", "D"),
                              ncol = 2, nrow = 2)

SDE.plots.all

ggsave(
  filename = here::here("results", "SDE_habitat-feat.png"),
  plot = SDE.plots.all,
  width = 30, 
  height = 20, 
  units = "cm", 
  dpi = 720)

graphics.off()

### 6.6.6 linear model with species-level data conserved----

species.captures <- caps.summary %>%
  group_by(plot, bird_species) %>%
  summarize(total_captures = n(), 
            total_samples = sum(feces_collected == "y", na.rm = T),
            n_samples_seeds = sum(seeds == 1, na.rm = T),
            .groups = "drop")

species.captures <- species.captures %>%
  left_join(net.hours.plot, by = "plot") %>%
  mutate(capture_rate = total_captures / total_net_hours)

species.captures$SDF <- (species.captures$n_samples_seeds / species.captures$total_samples)

species.captures$SDF[!is.finite(species.captures$SDF)] <- 0

(hist(species.captures$SDF))

species.captures$SDE <- (species.captures$SDF*species.captures$capture_rate)

(hist(species.captures$SDE))

species.captures <- merge(x=species.captures, y=plot.spp.abun.div,
                          by="plot", all.x =T)

species.captures <- species.captures %>%
  left_join(plot.summary %>% select(plot, age_updated, forest_cover), by = "plot")


#library(glmmTMB)

model_zi <- glmmTMB(SDE ~ age_updated + no_ind_zoo + no_spp_zoo + forest_cover + (1 | bird_species), 
                    data = species.captures, 
                    ziformula = ~ 1,  # Models the probability of excess zeros
                    family = beta_family())  # Use 'beta_family' if SDF is a proportion
summary(model_zi)
