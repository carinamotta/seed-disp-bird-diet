# Created: March 27, 2025
# Updated:
# 
# Author: Carina Isabella Motta
# 
# Seed Dispersal 

################################################################################                                1. LOAD PACKAGES     ################################################################################

# a vector listing package names needed 

package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #species accumulation curves
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "ggplot2", #plots
                  "car", #check for collinearity between predictor variables
                  "effect.lndscp" #seed dispersal effectiveness (SDE) landscapes
)

#creating another list of new packages (if there are any)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]

#installing the packages if they aren't already on the computer
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

################################################################################                                 2. LOAD DATA                                ################################################################################
## 2.1 capture data----

# captures
captures <- readr::read_csv(here::here("data", "captures_01APRIL2025_2.csv"))
  
# corresponding family names 
bird.spp.fam <- readr::read_csv(here::here("data", "bird_spp_fam.csv"))

## 2.2 feces and seed sample data------

#seed count and identification 
seeds <- readr::read_csv(here::here("data","seeds_28MAR2025_2.csv"))

#seed morphospecies 
morpho <- readr::read_csv(here::here("data","morpho_28MAR2025.csv"))

## 2.3 bird trait data----

#load EltonTraits
elton.traits <- readr::read_tsv(here::here("data","BirdFuncDat.txt"))


## 2.4 plot data----
 
#plot survey data
plots <- readr::read_csv(here::here("data","newfor_database_filtered2.csv"))
  
#plot metadata
plot.metadata <- readr::read_csv(here::here("data", "plots_metadata4.csv"))

## 2.5 tree dispersal data----
  
#whether the tree is zoochorous or not 
dispersal <- readr::read_csv(here::here("data","dispersal.csv"))
  
# 3 -----
## 3.1 CAPTURE DATA ------

#transform into a tibble to facilitate using packages like dyplr 
captures <- as_tibble(captures)

#cut excess rows
captures <- captures[-(492:999),]

#cut excess columns
captures <- captures[,-(28:35)]

#subset captures to only include certain columns: capture date, session, day, plot, time the net was opened and time net was closed, species, sample number, whether feces were collected, if the bird is a recap  (y = 1/n = 0), and if the bird is frugivorous
subset.caps <- captures %>% 
  select(1:6, 11:12, 19, 23, 25, 26)

#cut excess column
bird.spp.fam <- bird.spp.fam[,-(3)]

#add to the capture dataset
subset.caps <- subset.caps %>%
  left_join(bird.spp.fam, by = "species") %>%
  relocate(family, .after = nets_closed) 

## 3.2 SAMPLE DATA----

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
  select(1:4, 8:11)

#merge seed and morphospp data 
seed.morpho <- merge(x=subset.seeds, y=subset.morpho,
                     by="morpho_spp", all.x =T)

## 3.3 MERGE CAPTURE AND SAMPLE DATA-----

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


## 3.4 BIRD DIET & TRAIT DATA----

#select the columns that interest us: family Latin name, species Latin name, 
#% of diet that is fruit, diet category
subset.traits <- elton.traits %>% select(8, 16, 20, 26, 27, 28, 29 ,30, 36)

#cut excess rows
subset.traits <- subset.traits[-(9994:9995),]

colnames(subset.traits)[c(1:9)] <-  
  c("species", "frug_degree", "guild", "fs_ground", "fs_understory",
    "fs_midhigh", "fs_canopy", "fs_aerial", "mass_elton")

#correct species names using updated names from Cornell Lab of Ornithology, Clements Checklist 2024 (https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2024/2024-citation-checklist-downloads/)

subset.traits <- subset.traits %>% 
mutate(species = str_replace_all(species, c("Veniliornis passerinus"= 
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
  filter(species %in% subset.caps$species)



## 3.5 PLOT DATA----

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

#correct column names  
colnames(plot.metadata)[c(3,4, 9)] <-  
  c("lat", "long", "1km_avg_forest_cover_Cesar")

#update plot age by adding 9 years (data originally collected in 2015/2016)
plot.metadata$age_updated <- (plot.metadata$age + 9)

## 3.6 TREE DISPERSAL DATA ----

#correct column names  
colnames(dispersal)[c(1:5)] <-  
  c("spp_code", "family", "species", "zoo_disp", "disp_agent")

## 3.7 APPEND DISPERSAL DATA TO PLOT DATA----
#add whether species is dispersed by zoo
plot.spps$zoo_disp <- ifelse(plot.spps$species %in% dispersal$species, 1, 0)


## 4.1 TOTALS OF CAPTURES, FECES SAMPLES, & SEEDS COLLECTED------

#filter out the no capture days
caps.summary <- captures.seeds[!grepl("no_captures", captures.seeds$species),]

caps.total.summary <- caps.summary %>%
  summarise(n_caps = n_distinct(sample),
          n_recaps = sum(recapture == 1, na.rm = T),
          n_caps_frug = sum(frugivorous == 1, na.rm = T),
          n_spp_birds = n_distinct(species),
          n_spp_frug = n_distinct(species[frugivorous == 1]),
          n_samples = sum(feces_collected == "y", na.rm = T),
          n_samples_frug = sum(feces_collected == "y" & frugivorous == 1),
          n_samples_seeds = sum(seeds == 1, na.rm = T),
          n_seeds = sum(no_seeds)
          
)
## 4.2 TOTAL NET HOURS-----

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



#########################################################################################################   5 SPECIES-LEVEL ANALYSES  ########################## ################################################################################

## 5.1 CAPTURES, FECES SAMPLES, & SEEDS COLLECTED----- 


#create a list of species captured, with the number of captures, number of samples, and the mass of the species 
caps.spp.summary <- caps.summary %>%
  group_by(species, family, frugivorous) %>%
  summarise(
    no_caps = n(),  # Counts the number of captures for each species
    no_samples = sum(feces_collected == "y", na.rm = T), 
    mass_caps = mean(mass, na.rm = TRUE)
  ) %>%
  ungroup()

#summarize samples with seeds and number of seeds per species to create a species-level summary table 
seeds.summary <- subset.seeds %>%
  group_by(species) %>%
  summarise(
    no_samples_seeds = n_distinct(sample),  
    no_seed_total = sum(no_seeds) 
  ) %>%
  ungroup()

#merge seeds summary and species capture summary 
caps.spp.seed.summary <- merge(x=caps.spp.summary, y=seeds.summary,
                               by="species", all.x =T)

#fill NAs with 0s
caps.spp.seed.summary[is.na(caps.spp.seed.summary)] <- 0

#reorder the columns
species.summary <- caps.spp.seed.summary[,c(2, 1, 4, 5, 7, 8, 3, 6)]

## 5.2 CAPTURE RATE-----

#calculate the capture rate by dividing the number of captures by the total number of net hours 
species.summary$cap_rate <- (species.summary$no_caps/2763) 

## 5.3 SEED DISPERSAL FREQUENCY (SDF)------

#calculate seed dispersal frequency (SDF) on a species level --> number of feces with seeds / total number of feces collected per species 
species.summary$SDF <- (species.summary$no_samples_seeds/species.summary$no_samples)

#substitute infinite values for 0
species.summary$SDF[!is.finite(species.summary$SDF)] <- 0

## 5.4 SEED DISPERSAL EFFECTIVENESS (SDE)----

#calculate seed dispersal effectiveness by multiplying SDF by the capture rate
species.summary$SDE <- species.summary$SDF*species.summary$cap_rate

#filter bird species that actually had seeds and a SDE value 
species.summary.seeds <- species.summary %>%
  filter(no_samples_seeds >= 1)

#plot SDE landscape
SDE_spp <- effectiveness_plot(species.summary.seeds$cap_rate, 
                              species.summary.seeds$SDF, 
                              label = species.summary.seeds$species,  
                              myxlab = 
                                "capture rate (no. captures/total net hours)", 
                              myylab = 
                                "frequency of seeds found in feces")

SDE_spp

## 5.5 SDE X BIRD TRAITS----

#merge bird traits dataset onto the species.summary 
species.summary.traits <- merge(x=species.summary.seeds, y=filtered.traits.data,
                         by="species", all.x =T)


cor(species.summary.traits[, c("no_seed_total", "SDF")])

### 5.5.1 BIRD BODY MASS-----

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


### 5.5.2 BIRD % DIET FRUGIVORY----- 

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
  geom_smooth(method = "lm", alpha =0.51, color = "blue") +
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

###########################################################################################################   6 PLOT-LEVEL ANALYSES  ########################### ################################################################################

## 6.1 CAPTURES, FECES SAMPLES, & SEEDS COLLECTED----

#number of captures, recaps per plot, number of frugivorous captures, number of samples, number of samples from frugivores, number of samples with seeds
caps.plot.summary <- caps.summary %>% #use cap.summary, that already filtered out no capture days
  group_by(plot) %>%
  summarise(no_captures = n_distinct(sample),
            no_recaps = sum(recapture == 1, na.rm = T),
            no_caps_frug = sum(frugivorous == 1, na.rm = T),
            no_spp_birds = n_distinct(species),
            no_spp_frug = n_distinct(species[frugivorous == 1]),
            no_samples = sum(feces_collected == "y", na.rm = T),
            no_samples_frug = sum(feces_collected == "y" & frugivorous == 1),
            no_samples_seeds = sum(seeds == 1, na.rm = T)
            
  )

# append to plot.summary
plot.summary <- merge(x=plot.metadata, y=caps.plot.summary,
                       by="plot", all.x =T) 

## 6.2 NET HOURS----

# calculate net hours per plot
net.hours.plot <- net.hours %>%
  group_by(plot) %>%  # Group by plot
  summarise(total_net_hours = round(sum(net_hours)))  # Sum net hours per plot  

# append to plot.summary 
plot.summary <- merge(x=plot.summary, y=net.hours.plot,
                       by="plot", all.x =T) 

## 6.3 CAPTURE RATE-----

#all captures
plot.summary$cap_rate <- (plot.summary$no_caps/plot.summary$total_net_hours)

#frugivorous captures
plot.summary$cap_rate_frug <- (plot.summary$no_caps_frug/plot.summary$total_net_hours)

cor(plot.summary[, c("cap_rate", "cap_rate_frug")])

## 6.4 SEED DIPSERSAL FREQUENCY (SDF)-----

#calculate seed dispersal frequency (SDF) on a species level --> number of feces with seeds / total number of feces collected per species 

plot.summary$SDF_all <- (plot.summary$no_samples_seeds/plot.summary$no_samples)

plot.summary$SDF_frug <- (plot.summary$no_samples_seeds/plot.summary$no_samples_frug)

cor(plot.summary[, c("SDF_frug", "SDF_all")])

## 6.5 SEED DISPERSAL EFFECTIVENESS (SDE)------

plot.summary$SDE_all <- plot.summary$SDF_all*plot.summary$cap_rate

plot.summary$SDE_frug <- plot.summary$SDF_frug*plot.summary$cap_rate_frug

cor(plot.summary[, c("SDE_frug", "SDE_all")])

SDE_plot_all <- effectiveness_plot(plot.summary$cap_rate, 
                                   plot.summary$SDF_all, 
                                   label = plot.summary$plot,  
                                   myxlab = "capture rate (no. birds captured/total 
                              net hours)", 
                                   myylab = "frequency of seeds found in feces")

SDE_plot_all

SDE_plot_frug <- effectiveness_plot(plot.summary$cap_rate_frug, 
                                    plot.summary$SDF_frug, 
                                    label = plot.summary$plot,  
                                    myxlab = "capture rate, frugivores only (no. birds captured/total net hours)", 
                                    myylab = "frequency of seeds found in feces, frugivores only")

SDE_plot_frug


## 6.6 SDE X HABITAT FEATURES-----

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
model.habitat.features <- lm(SDE_all ~ age_updated + no_ind_zoo + no_spp_zoo + Mapbiomas_1km_forestcover, data = plot.summary)

# Check VIF values for collinearity 
car::vif(model.habitat.features)

#all <2, no collinearity 

# Check normality of SDE
shapiro.test(plot.summary$SDE_all)  # normal!

### 6.6.1 PLOT AGE-----

ggplot(plot.summary, aes(x = age_updated, y = SDF_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of age
shapiro.test(plot.summary$SDF_all)  # normal!

ggplot(plot.summary, aes(x = age_updated, y = cap_rate)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

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
cor_age <- cor.test(plot.summary$SDE_frug, plot.summary$age_updated, method = "pearson")

print(cor_age)

### 6.6.2 ZOOCHOROUS TREE SPECIES ABUNDANCE----- 

ggplot(plot.summary, aes(x = no_ind_zoo, y = SDE_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of zoochorous tree species abundance
shapiro.test(plot.summary$no_ind_zoo)  # normal!

cor_zoo_abundance <- cor.test(plot.summary$SDE_all, plot.summary$no_ind_zoo, method = "pearson")

print(cor_zoo_abundance)

### 6.6.3 ZOOCHOROUS TREE SPECIES DIVERSITY-----

ggplot(plot.summary, aes(x = no_spp_zoo, y = SDE_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of zoochorous tree species diversity
shapiro.test(plot.summary$no_spp_zoo)  # normal!

cor_zoo_diversity <- cor.test(plot.summary$SDE_all, plot.summary$no_spp_zoo, method = "pearson")

print(cor_zoo_diversity)

### 6.6.4 PLOT CONNECTIVITY----- 

ggplot(plot.summary, aes(x = Mapbiomas_1km_forestcover, y = SDF_all)) +
  geom_text(aes(label = plot), vjust = -0.5, hjust = 0.5) +  
  geom_point() +
  theme_minimal()

# Check normality of connectivity 
shapiro.test(plot.summary$Mapbiomas_1km_forestcover)  # normal!

cor_zoo_connectivity <- cor.test(plot.summary$SDE_all, log10(plot.summary$Mapbiomas_1km_forestcover), method = "pearson")

print(cor_zoo_connectivity)

### LINEAR MODEL WITH SPECIES-LEVEL DATA CONSERVED----

species.captures <- caps.summary %>%
  group_by(plot, species) %>%
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
  left_join(plot.summary %>% select(plot, age_updated, Mapbiomas_1km_forestcover), by = "plot")


library(glmmTMB)

model_zi <- glmmTMB(SDE ~ age_updated + no_ind_zoo + no_spp_zoo + Mapbiomas_1km_forestcover + (1 | species), 
                    data = species.captures, 
                    ziformula = ~ 1,  # Models the probability of excess zeros
                    family = beta_family())  # Use 'beta_family' if SDF is a proportion
summary(model_zi)
