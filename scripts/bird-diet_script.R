# 1. LOAD PACKAGES-----
# a vector listing package names needed 

package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #species accumulation curves
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "ggplot2" #plots
                  )


#creating another list of new packages (if there are any)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]

#installing the packages if they aren't already on the computer
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}

# 2. LOAD DATA----

## 2.1 Feces and seed sample data------
  
#seed count and identification 
seeds <- readr::read_csv(here::here("data", "raw_data",
                                    "droppings_seeds_14MAY2025.csv"))
  
#seed morphospecies 
morpho <- readr::read_csv(here::here("data", "raw_data",
                                     "droppings_seeds_morphotypes_14MAY2025_2.csv"))

## 2.2 Metabarcoding----

#diet data 
metabarcoding <- readr::read_csv(here::here("data","raw_data",
                                            "metabarcoding_14MAY2025.csv"))
  
genera.traits <- readr::read_csv(here::here("data", "processed_data", 
                                            "genera_traits_14MAY2025.csv"))

# 3. DATA CLEANING AND PREP----

## 3.1 Feces and seed sample data----

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

## 3.3 Metabarcoding----

#transform into a tibble to facilitate using packages like dyplr 
metabarcoding <- as_tibble(metabarcoding)

#cut excess rows
metabarcoding <- metabarcoding[-(561:1226),]

#cut excess columns
metabarcoding <- metabarcoding[,-(15:32)]

metabarcoding <- metabarcoding %>%
filter(bird_species != "Stilpnia cayana")

#transform into a tibble to facilitate using packages like dyplr 
genera.traits <- as_tibble(genera.traits)


#cut excess columns
genera.traits <-genera.traits[,c(2:6)]

metabarcoding.genera <- metabarcoding %>%
  filter(final_fate == 1) %>%
  group_by(plant_family) %>%
  reframe(plant_genus = unique(plant_genus))

n_distinct(metabarcoding.genera$plant_family)

n_distinct(metabarcoding.genera$plant_genus)

metabarcoding.genera.traits <- merge(x=metabarcoding.genera, y=genera.traits,
                     by="plant_genus", all.y =F)

write.csv(metabarcoding.genera.traits, here::here( "results",
                                 "genera_traits_XXXX.csv"))



#confirm number of unique samples that were sent for sequencing 
unique_samples <- metabarcoding %>% 
  filter(!grepl("blank", sample)) %>%
  reframe(n = n_distinct(sample)) #103 samples sent for sequencing
#2 test samples
#30 the first round
#36 the second round 
#36 the third round 

#check number of samples successfully sequences (resulted in OTUs)
unique_samples_successful <- metabarcoding %>% 
  filter(final_fate ==1) %>%
  reframe(unique(sample)) #74 samples successfully sequenced and 
#not cut due to insufficient reads


# 4.DATA SUMMARY----
# 4.1 SEEDS-----
n_distinct(seed.morpho$unique)

n_distinct(seed.morpho$plant_family)

n_distinct(seed.morpho$plant_genus)

n_distinct(seed.morpho$plant_genus_morpho)

n_distinct(seed.morpho %>%
             filter(!plant_genus %in% c("Fabaceae_1", "Amaranthaceae", 
                                        "unknown_2", "unknown_1", "unknown_5", 
                                        "Myrtaceae_1","unknown_3", "unknown_4",
                                        "Myrtaceae_2")) %>%
             pull(plant_genus)
)

sum(seed.morpho$no_seeds)

# 4.2 METABARCODING----
# calculate 
  # number of samples
   # samples successfully sequenced, filtered out bc contamination etc.  
  # number of samples / bird species
  # number of reads 
  # number of OTUs
  # number of plant genera 
  
  summary.plants <- metabarcoding.genera %>%
  summarise(genera = n_distinct(plant_genus), families = n_distinct(plant_family))


# 5 METABARCODING SUMMARY STATISTICS----
## 5.1 FATE SUMMARIES----

# final fates of each OTU
# 0 means it didn't make the read threshold,
# 1 means is was successful and kept
# 2 means it didn't make the match threshold
# 3 means it wasn't successfully sequenced 
# 4 means it was excluded due to suspected contamination 
# 5 means that it was a blank sample that generated an OTU

sample.fate <- metabarcoding %>%
  filter(!grepl("blank", sample)) %>%
  group_by(sample) %>%
   summarise(
     final_fate = ifelse(any(final_fate == 1), "successful",
                         ifelse(any(final_fate == 0), "insufficient_reads",
                                ifelse(any(final_fate == 2), "insufficient_match",
                                       ifelse(any(final_fate == 3),"unsuccessful_seq", 
                                              ifelse(any(final_fate == 4), "contamination"))))
     ))

#here we summarize, samples only fell into three categories:
#   successful, unsuccessfully sequenced, or had insufficient reads
#   while individual OTUs were deleted due to insufficient match or 
#   contamination, those two were not motives for excluding samples
sample.fate.summary <- sample.fate %>%
  group_by(final_fate) %>%
  summarise(total_samples = n())

# now we summarize the OTU fates

OTU.fate <- metabarcoding %>%
  filter(!grepl("blank", sample)) %>%
  group_by(OTU) %>%
  summarise(
    final_fate = ifelse(any(final_fate == 1), "successful",
                        ifelse(any(final_fate == 0), "insufficient_reads",
                               ifelse(any(final_fate == 2), "insufficient_match",
                                      ifelse(any(final_fate == 3),"unsuccessful_seq", 
                                             ifelse(any(final_fate == 4), "contamination", NA))))     ), 
    no_reads_OTU = max(no_reads_OTU))%>%
  mutate(no_reads_OTU = replace_na(no_reads_OTU, 0))

# create summary of OTU fates
OTU.fate.summary <- OTU.fate %>%
  group_by(final_fate) %>%
  summarise(total_OTUs = n_distinct(OTU), n_reads = sum(no_reads_OTU))

## 5.2 READS & OTUs before quality filtering ---- 
t.reads <- metabarcoding %>%
  filter(!grepl("blank", sample),!is.na(no_reads_OTU))  %>%
  group_by(OTU) %>%
  summarise(t_reads = sum(no_reads_OTU))

#the sum of reads
sum(t.reads$t_reads)

#the mean number of reads per OTU
mean(t.reads$t_reads)

#the standard deviation of reads
sd(t.reads$t_reads)

# determine the number of OTUs recovered per sample
t.OTU.sample <- metabarcoding %>%
  filter(!grepl("blank", sample),!is.na(no_reads_OTU))  %>%
  group_by(sample) %>%
  summarise(n_OTU = n_distinct(OTU))

#mean number of OTUs recovered per sample
mean(t.OTU.sample$n_OTU)

#SD of number of OTUs recovered per sample
sd(t.OTU.sample$n_OTU)

## 5.3 READS & OTUs after quality filtering ----- 
t.reads.filtered <- metabarcoding %>%
  filter(final_fate == 1)  %>%
  group_by(OTU) %>%
  summarise(t_reads_filtered = sum(no_reads_OTU))

#the sum of reads
sum(t.reads.filtered$t_reads_filtered)

#the mean number of reads per OTU
mean(t.reads.filtered$t_reads_filtered)

#the standard deviation of reads
sd(t.reads.filtered$t_reads_filtered)

#reads per dropping sample
t.reads.sample.filtered <- metabarcoding %>%
  filter(final_fate == 1)  %>%
  group_by(sample) %>%
  summarise(t_reads_sample_filtered = sum(no_reads_OTU))

mean(t.reads.sample.filtered$t_reads_sample_filtered)

sd(t.reads.sample.filtered$t_reads_sample_filtered)

#OTUs per dropping sample
t.OTU.sample.filtered <- metabarcoding %>%
  filter(final_fate == 1)  %>%
  group_by(sample) %>%
  summarise(n_OTU_filtered = n_distinct(OTU))

mean(t.OTU.sample.filtered$n_OTU_filtered)

sd(t.OTU.sample.filtered$n_OTU_filtered)

## 5.3 PLANT GENERA----


n.OTU <- metabarcoding %>%
  filter(!grepl("blank", sample), !is.na(no_reads_OTU)) %>%
  filter(final_fate == 1)  

n_distinct(n.OTU$OTU)

n_distinct(n.OTU %>%
             filter(!plant_genus %in% c("Serjania/Paullinia", "Lippia/Lantana")) %>%
             pull(OTU)
)

n.OTU %>%
  filter(plant_genus %in% c("Serjania/Paullinia")) %>%
  summarise(n_OTUs = n_distinct(OTU))


n.plants <- metabarcoding %>%
  filter(!grepl("blank", sample), !is.na(OTU)) %>%
  filter(final_fate == 1)  %>%
  group_by(plant_family, plant_genus) %>%
  summarise(n_plant_fam = n_distinct(sample), 
            n_plant_genera = n_distinct(sample))

n_distinct(n.plants$plant_family)

n_distinct(n.plants$plant_genus)

n_distinct(n.plants %>%
             filter(!plant_genus %in% c("Serjania/Paullinia", "Lippia/Lantana")) %>%
             pull(plant_genus)
)



## DATA VIZ 2.0 ----
meta.sum <- metabarcoding %>%
  filter(final_fate == 1) %>%
  select(2, 3, 12) %>%
  mutate(Method = "metabarcoding", Detected = 1)


seed.sum <- seed.morpho %>%
  select(4, 5, 10) %>%
  mutate(Method = "seed", Detected = 1)


# Bind the datasets and account for both methods
combined_interaction <- bind_rows(seed.sum, meta.sum)

# Ensure that bird_species and plant_genus are characters
summary_interaction <- combined_interaction %>%
  mutate(
    bird_species = as.character(bird_species),
    plant_genus = as.character(plant_genus)
  )

# Now check if both methods (seed and metabarcoding) exist for each bird/plant pair
summary_interaction <- summary_interaction %>%
  group_by(bird_species, plant_genus) %>%
  mutate(
    Method = ifelse(
      "seed" %in% Method & "metabarcoding" %in% Method, "both",  # If both methods are present
      ifelse("seed" %in% Method, "seed", "metabarcoding")     # Else, assign the present method
    )
  ) %>%
  ungroup()

# Create a complete grid, filling missing combinations with no_seed and Detected = 0
full_data <- summary_interaction %>%
  complete(bird_species, plant_genus, fill = list(Method = "no_detection", Detected = 0))


# Ensure unique bird_species / plant_genus / sample combinations
unique_data <- full_data %>%
  distinct(bird_species, plant_genus, sample, .keep_all = TRUE)


summary_interaction <- full_data %>%
  group_by(bird_species, plant_genus) %>%
  summarise(
    n_samples = sum(!is.na(sample)),  # Count non-NA samples
    Method = first(Method),
    .groups = "drop"
  )

n_distinct(summary_interaction$plant_genus)

detection_colors <- c(
  "both" = "#F19E14",               # teal
  "seed" = "#3B5708",         # mediumseagreen
  "metabarcoding" = "#9F4147",# maroon
  "no_detection" = "white"         # empty grid
)


# Reorder bird_species from most to fewest plant detections
summary_interaction <- summary_interaction %>%
  group_by(bird_species) %>%
  mutate(total_detections_bird = sum(n_samples > 0, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bird_species = fct_reorder(bird_species, total_detections_bird, .desc = F))

# Reorder plant_genus from most to fewest detections across birds
summary_interaction <- summary_interaction %>%
  group_by(plant_genus) %>%
  mutate(total_detections_plant = sum(n_samples > 0, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(plant_genus = fct_reorder(plant_genus, total_detections_plant, .desc = TRUE))



ggplot(summary_interaction, aes(x = plant_genus, y = bird_species)) +
  geom_tile(aes(fill = Method), color = "black", lwd = 0.3, linetype = 1) +
  
  # Apply color palette for detection methods
  scale_fill_manual(
    values = detection_colors,  # Use the colors defined for each detection type
    name = "Detection",
    guide = "legend", 
    breaks = names(detection_colors),  # Use the names of the colors (detection types)
    labels = c("Both methods", "Seed detected", "Metabarcoding detected", "Not detected")  # Custom labels
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    ) +
  coord_fixed(ratio = 1) +
  labs(x = "Plant Genus", y = "Bird Species")


## GENERA TRAITS -----
## 

# Ensure unique bird_species / plant_genus / sample combinations
unique_genera <- summary_interaction %>%
  distinct(plant_genus, .keep_all = F)

all.genera.traits <- merge(x=unique_genera, y=genera.traits,
                               by="plant_genus", all.x =T)

write.csv(all.genera.traits, here::here( "results",
                                                   "genera_traits_new_2.csv"))

#seed count and identification 
genera.traits.new <- readr::read_csv(here::here("data","genera_traits_new_2.csv"))


genera.traits.new.filtered <- genera.traits %>%
  filter(!is.na(growth_form))

origin.summary <- genera.traits.new.filtered %>%
  group_by(origin) %>%
  summarise(n_genera = n_distinct(plant_genus), .groups = "drop")

growth.form.summary <- genera.traits.new.filtered %>%
  group_by(growth_form) %>%
  summarise(n_genera = n_distinct(plant_genus), .groups = "drop")

fruit.type.summary <- genera.traits.new.filtered %>%
  group_by(fruit_type) %>%
  summarise(n_genera = n_distinct(plant_genus), .groups = "drop")

## DATA VISUALIZATION-----


  
interaction_matrix <- metabarcoding %>%
  filter(final_fate == 1) %>%
  group_by(plant_genus, bird_species) %>% 
  summarise(sample = n_distinct(sample)) %>% # this just combines all the quantities from the observations into one value per family
  arrange(plant_genus) %>% #sinc I imagine you will want to visualize this in order of different insect orders, this and the next line of code make sure that happens
  pivot_wider(names_from = bird_species, #makes this in to a matrix with this column name
              values_from = sample) %>% #and values in cells filled with this column
  column_to_rownames(var = "plant_genus")  #then, we set the row names to our flower visitor families

interaction_matrix[is.na(interaction_matrix)] <- 0

interaction_matrix_2 <- interaction_matrix %>%
  tibble::rownames_to_column(var = "plant_genus") %>%
  pivot_longer(-plant_genus, names_to = "bird_species", values_to = "n") %>%
  group_by(plant_genus, bird_species) %>%
  top_n(n = 3)

bird_order <- interaction_matrix_2 %>%
  group_by(bird_species)%>%
  summarise(diet = sum(n))

bird_order <- bird_order %>%
  arrange(diet)

bird_order <- bird_order$bird_species

bird_order

plant_order <- interaction_matrix_2 %>%
  group_by(plant_genus)%>%
  summarise(det = sum(n))

plant_order <- plant_order %>%
  arrange(-det)

plant_order <- plant_order$plant_genus

cols <- c("0" = "white", "1" = "#C3E747", "2" = "#607729", "3" = "#192609",
          "4" = "#001800", "5" = "#000800")

interaction_matrix_2$n <- as.character(interaction_matrix_2$n)

content <- ggplot(interaction_matrix_2, aes(x = factor(plant_genus, level = plant_order),
                                    y = factor(bird_species, level = bird_order),
                                    fill = n)) +
  geom_tile(color = "black",
            lwd = 0.3,
            linetype = 1) +
  coord_fixed()+
  scale_fill_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

content

content + theme_void() 

## DONUT CHARTS -----

sample.summary <- metabarcoding %>%
  filter(final_fate == 1) %>%
  group_by(plant_genus) %>%
  reframe(n = n_distinct(sample),
          )

sample.summary.traits <- merge(x=genera.traits, y=sample.summary,
                 by="plant_genus", all.x =T)

growth.form <- sample.summary.traits %>%
  dplyr::group_by(growth_form) %>%
  dplyr::summarise(n = n()) 

growth.form$fraction <- growth.form$n / sum(growth.form$n)

# Compute the cumulative percentages (top of each rectangle)
growth.form$ymax = cumsum(growth.form$fraction)

# Compute the bottom of each rectangle
growth.form$ymin = c(0, head(growth.form$ymax, n=-1))

growth.form.plot <- ggplot(growth.form, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = growth_form)) +
  geom_rect(color = "black") +
  scale_fill_manual(values= c("#71B481","#3B5708", "#95582D","#75774E" )) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()

growth.form.plot

origin <- sample.summary.traits %>%
  dplyr::group_by(origin) %>%
  dplyr::summarise(n = n())

origin$fraction <- origin$n / sum(origin$n)

# Compute the cumulative percentages (top of each rectangle)
origin$ymax = cumsum(origin$fraction)

# Compute the bottom of each rectangle
origin$ymin = c(0, head(origin$ymax, n=-1))

origin.plot <- ggplot(origin, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = origin)) +
  geom_rect(color = "black") +
  scale_fill_manual(values= c("#9F4147", "#607729")) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()

origin.plot

fruit.type <- sample.summary.traits %>%
  dplyr::group_by(fruit_type) %>%
  dplyr::summarise(n = n())

fruit.type$fraction <- fruit.type$n / sum(fruit.type$n)

# Compute the cumulative percentages (top of each rectangle)
fruit.type$ymax = cumsum(fruit.type$fraction)

# Compute the bottom of each rectangle
fruit.type$ymin = c(0, head(fruit.type$ymax, n=-1))

fruit.type.plot <- ggplot(fruit.type, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = fruit_type)) +
  geom_rect(color = "black") +
  scale_fill_manual(values= c("darkgray", "orange")) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()

fruit.type.plot

## accumulation curve ------

sample.summary.2 <- metabarcoding %>%
  filter(final_fate == 1) %>%
  group_by(plant_genus, sample) %>%
  reframe(n = n_distinct(sample),
  )

dna_colcurve <- sample.summary.2 %>%
  pivot_wider(names_from = plant_genus, 
              values_from = n) %>%
  column_to_rownames(var = "sample")

dna_colcurve[is.na(dna_colcurve)] <- 0


dna_colcurve.plot <- specaccum(dna_colcurve, method="rarefaction")

cummspp <- plot(dna_colcurve.plot, ci.type="poly", col="#6A0207", lwd=3, ci.lty=0, 
                ci.col=alpha("#6A0207", 0.5),
                xlab = "No. Samples Sequenced", 
                ylab = "Cummulative No. Plant OTUs Detected",
                cex.lab = 1.2, cex.axis =1.2) 
