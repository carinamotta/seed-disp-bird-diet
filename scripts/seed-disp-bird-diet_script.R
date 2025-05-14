# Created: March 27, 2025
# Updated:
# 
# Author: Carina Isabella Motta
# 
# Seed Dispersal & Bird Diet

##############################################################################################################   1 LOAD PACKAGES     ########################### ################################################################################

# a vector listing package names needed 

package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #species accumulation curves
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "ggplot2", #plots
                  "effect.lndscp" #seed dispersal effectiveness (SDE) landscapes
)

#creating another list of new packages (if there are any)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]

#installing the packages if they aren't already on the computer
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

##############################################################################################################   2 LOAD DATA     ############################### ################################################################################

## 2.1 LOAD BIRD DATA----

### 2.1.1 LOAD CAPTURE DATA----
captures <- readr::read_csv(here::here("data","captures_01APRIL2025_2.csv"))

### 2.1.2 LOAD SPECIES AND FAMILY NAMES-----
bird.spp.fam <- readr::read_csv(here::here("data", "bird_spp_fam.csv"))

### 2.1.3 LOAD DIET DATA----
#load EltonTraits
elton.traits <- readr::read_tsv(here::here("data","BirdFuncDat.txt"))


## 2.2 LOAD SAMPLE DATA----

### 2.2.1 LOAD SEED COUNT AND IDENTIFICATION----
seeds <- readr::read_csv(here::here("data","seeds_28MAR2025_2.csv"))

### 2.2.2 LOAD SEED MORPHOSPECIES----
morpho <- readr::read_csv(here::here("data","morpho_28MAR2025.csv"))

### 2.2.3 LOAD DNA EXTRACTION & PCR RESULTS---- 
dna.ext.pcr <- readr::read_csv(here::here("data","extraction&PCR.csv"))

### 2.2.4 LOAD METABARCODING RESULTS----
metabarcoding <- readr::read_csv(here::here("data","metabarcoding_results.csv"))


## 2.3 LOAD PLOT AND PLANT DATA----

### 2.3.1 LOAD PLOT SURVEY DATA----
plots <- readr::read_csv(here::here("data","newfor_database_filtered2.csv"))

### 2.3.2 LOAD PLOT METADATA----
plot.metadata <- readr::read_csv(here::here("data", "plots_metadata4.csv"))

### 2.3.3 LOAD PLANT TRAIT DATA----
dispersal <- readr::read_csv(here::here("data","dispersal.csv"))



########################################################################################################   3 DATA CLEANING & PREP  ############################# ################################################################################

## 3.1 BIRD DATA----

### 3.1.1 CAPTURE DATA----

#transform into a tibble to facilitate using packages like dyplr 
captures <- as_tibble(captures)

#cut excess rows
captures <- captures[-(492:999),]

#cut excess columns
captures <- captures[,-(28:35)]

#subset captures to only include certain columns: capture date, session, day, plot, time the net was opened and time net was closed, species, sample number, whether feces were collected, if the bird is a recap  (y = 1/n = 0), and if the bird is frugivorous
subset.caps <- captures %>% 
  select(1:6, 11:12, 19, 23, 25, 26)

### 3.1.2 BIRD FAMILIES----

#cut excess column
bird.spp.fam <- bird.spp.fam[,-(3)]

#add to the capture dataset
subset.caps <- subset.caps %>%
  left_join(bird.spp.fam, by = "species") %>%
  relocate(family, .after = nets_closed)  # Moves `family` after `species`

### 3.1.2 DIET & TRAIT DATA----

#select the columns that interest us: family Latin name, species Latin name, 
#% of diet that is fruit, diet category
subset.traits <- elton.traits %>% select(8, 16, 20, 26, 27, 28, 29 ,30, 36)

colnames(subset.traits)[1] ="species"

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

### 3.1.3 MERGE INTO BIRD MASTER SHEET (caps.traits)----

#left join the two to match species with diet
caps.traits <- merge(x=subset.caps, y=subset.traits,
                     by="species", all.x =T)

#rename columns 
colnames(caps.traits)[c(14:21)] <-  
  c("fruit_%", "guild", "fs.ground", "fs.understory",
    "fs.midhigh", "fs.canopy", "fs.aerial", "mass_elton")

#reorder the columns
caps.traits <- caps.traits[,c(2, 3, 4, 6, 7, 5, 8, 1, 9, 12, 11, 13:20, 10, 21)]

## 3.2 SAMPLE DATA----

### 3.2.1 JOIN SEED COUNT AND MORPHOSPP DATA-----
 
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

#append seed data to caps.traits
caps.traits$seeds <- ifelse(caps.traits$sample %in% seed.morpho$sample, 1, 0)

#reorder the columns
caps.traits <- caps.traits[,c(1:10, 22, 11:21)]

subset.seed.morpho <- seed.morpho %>%
  select(sample, no_seeds)

caps.traits <-  caps.traits %>%
  left_join(subset.seed.morpho, by = "sample")

#fill NAs with 0s
caps.traits$no_seeds[is.na(caps.traits$no_seeds)] <- 0

### 3.2.2 DNA EXTRACTION AND METABARCODING DATA----

#transform into a tibble to facilitate using packages like dyplr 
dna.ext.pcr <- as_tibble(dna.ext.pcr)

#cut excess rows
dna.ext.pcr <- dna.ext.pcr[-(179:987),]

#cut excess columns
dna.ext.pcr <- dna.ext.pcr[,-(12:28)]

#transform into a tibble to facilitate using packages like dyplr 
metabarcoding <- as_tibble(metabarcoding)

#cut excess rows
metabarcoding <- metabarcoding[-(561:1226),]

#cut excess columns
metabarcoding <- metabarcoding[,-(15:32)]

## 3.3 PLOT DATA----

### 3.3.1 PLOT DATA----

#as tibble
plots <- as_tibble(plots)

#subset
plot.spps <- plots %>% 
  select(1, 8, 10:14)

#correct column names  
colnames(plot.spps)[c(1:7)] <-  
  c("date", "plot", "tag", "spp_code", "family", "species", "origin")

### 3.3.2 PLOT METADATA----

#as tibble
plot.metadata <- as_tibble(plot.metadata)

#correct column names  
colnames(plot.metadata)[c(3,4, 9)] <-  
  c("lat", "long", "1km_avg_forest_cover_Cesar")

#update plot age by adding 9 years (data originally collected in 2015/2016)
plot.metadata$age_updated <- (plot.metadata$age + 9)

### 3.3.3 DISPERSAL DATA ----

#correct column names  
colnames(dispersal)[c(1:5)] <-  
  c("spp_code", "family", "species", "zoo_disp", "disp_agent")

### 3.3.4 APPEND DISPERSAL DATA TO plot.spps----

#add whether species is dispersed by zoo
plot.spps$zoo_disp <- ifelse(plot.spps$species %in% dispersal$species, 1, 0)


### 3.3.5 CALCUATE NUMBER OF IND., SPP. and FRUG. SPP PER PLOT-----

plot.spp.ind.frug <- plot.spps %>%
  group_by(plot) %>%
  summarise(no_ind_tree = n_distinct(tag),
            no_spp_tree = n_distinct(species),
            no_ind_zoo = n_distinct(tag[zoo_disp == 1]),
            no_spp_zoo = n_distinct(species[zoo_disp == 1]))

### 3.3.6 MERGE INTO ONE TABLE-----

plot.metadata <- merge(x=plot.metadata, y=plot.spp.ind.frug,
                               by="plot", all.x =T)


# 4 CAPTURES AND SAMPLE METRICS-------------------------------------------------


## 4.1 TOTAL------
 
### 4.1.1 TOTALS OF CAPTURES, FECES SAMPLES, & SEEDS COLLECTED-----

#filter out the no capture days
caps.summary <- caps.traits[!grepl("no_captures", caps.traits$species),]

cap.summary <- cap.summary %>%
  left_join(seed.morph, by = "sample")

#summarize the number of captures, number of recaps, number of frugivorous captures, number of species, number of frugivorous species, number of samples, number of frugivorous samples, and number of samples with seeds across all plots
caps.total.summary <- caps.summary %>%
  summarise(n_caps = n_distinct(sample),
            n_recaps = sum(recapture == 1, na.rm = T),
            n_caps_frug = sum(frugivorous == 1, na.rm = T),
            n_spp_birds = n_distinct(species),
            n_spp_frug = n_distinct(species[frugivorous == 1]),
            n_samples = sum(feces_collected == "y", na.rm = T),
            n_samples_frug = sum(feces_collected == "y" & frugivorous == 1),
            n_samples_seeds = sum(seeds == 1, na.rm = T),
            n_seeds = sum
            
  )

### 4.4.1 TOTAL NET HOURS-----

net.hours <- caps.traits %>%
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


## 4.1 INDIVIDUALS AND SPECIES CAPTURED, TOTAL AND PER PLOT----- 



### 4.1.2 TOTALS OF INDIVIDUALS AND SPECIES CAPTURED, PER PLOT------ 

#filter out the no capture days
caps.sample.summary <- caps.traits[!grepl("no_captures", caps.traits$species),]

#number of captures, recaps per plot, number of frugivorous captures, number of samples, number of samples from frugivores, number of samples with seeds
caps.sample.summary <- caps.sample.summary %>%
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

## 4.3 SPECIES CAPTURED LIST----- 

#filter out the no capture days
caps.spp <- caps.traits[!grepl("no_captures", caps.traits$species),]

#create a list of species captured, with the number of individuals, calcuate the average mass and include the mass from elton as well 
caps.spp.summary <- caps.spp %>%
  group_by(species, family, frugivorous, `fruit_%`) %>%
  summarise(
    no_caps = n(),  # Counts the number of captures for each species
    no_samples = sum(feces_collected == "y", na.rm = T), 
    mass_caps = mean(mass, na.rm = TRUE), # Calculates the mean mass per species
    mass_elton = max(mass_elton)
  ) %>%
  ungroup()


seeds.summary <- subset.seeds %>%
  group_by(species) %>%
  summarise(
    no_samples_seeds = n_distinct(sample),  
    no_seed_total = sum(no_seeds) 
  ) %>%
  ungroup()

caps.spp.seed.summary <- merge(x=caps.spp.summary, y=seeds.summary,
                       by="species", all.x =T)

#fill NAs with 0s
caps.spp.seed.summary[is.na(caps.spp.seed.summary)] <- 0

#reorder the columns
caps.spp.seed.summary <- caps.spp.seed.summary[,c(2, 1, 5, 6, 3, 4, 9, 10, 8, 7)]

## 4.4 NET HOURS----



# calculate net hours per plot
net.hours.plot <- net.hours %>%
  group_by(plot) %>%  # Group by plot
  summarise(total_net_hours = round(sum(net_hours)))  # Sum net hours per plot  

# append to plot.metadata 
plot.metadata <- merge(x=plot.metadata, y=net.hours.plot,
                     by="plot", all.x =T) 

## 4.5 CAPTURE RATES-----

#merge plot metadata and capture data 

plot.captures <- merge(x=plot.metadata, y=caps.sample.summary,
                       by="plot", all.x =T)

### 4.5.1 CAPTURE RATES PER PLOT-----

#all captures
plot.captures$cap_rate <- (plot.captures$no_caps/plot.captures$total_net_hours)

#frugivorous captures
plot.captures$cap_rate_frug <- (plot.captures$no_caps_frug/plot.captures$total_net_hours)

plot.summary <- plot.captures

### 4.5.2 CAPTURE RATES PER SPECIES----

caps.spp.seed.summary$cap_rate <- (caps.spp.seed.summary$no_caps/2763) 

species.summary <- caps.spp.seed.summary

# 5 SEED DISPERSAL EFFECTIVENESS (SDE)------
 
## 5.1 SDE SPECIES LEVEL

#calculate seed dispersal frequency (SDF) on a species level --> number of feces with seeds / total number of feces collected per species 

species.summary$SDF <- (species.summary$no_samples_seeds/species.summary$no_samples)

species.summary$SDF[!is.finite(species.summary$SDF)] <- 0

species.summary$SDE <- species.summary$SDF*species.summary$cap_rate

#filter spp that actually had seeds and a SDE value 

species.summary.seeds <- species.summary %>%
  filter(no_samples_seeds >= 1)


SDE_spp <- effectiveness_plot(species.summary.seeds$cap_rate, 
                              species.summary.seeds$SDF, 
                   label = species.summary.seeds$species,  
                   myxlab = "capture rate (no. ind captured/total net hours)", 
                   myylab = "frequency of seeds found in feces")

SDE_spp




## 5.2 SDE PLOT LEVEL----

#calculate seed dispersal frequency (SDF) on a species level --> number of feces with seeds / total number of feces collected per species 

plot.summary$SDF_all <- (plot.summary$no_samples_seeds/plot.summary$no_samples)

plot.summary$SDF_frug <- (plot.summary$no_samples_seeds/plot.summary$no_samples_frug)

plot.summary$SDE_all <- plot.summary$SDF_all*plot.summary$cap_rate

plot.summary$SDE_frug <- plot.summary$SDF_frug*plot.summary$cap_rate_frug

#filter spp that actually had seeds and a SDE value 


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
                                   myxlab = "capture rate, frugivores only (no. 
                                   birds captured/total net hours)", 
                                   myylab = "frequency of seeds found in feces, 
                                   frugivores only")

SDE_plot_frug
