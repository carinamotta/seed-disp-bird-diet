
# 
# September 29. 2023
# Carina Isabella Motta

#1 LOAD PACKAGES----------------------------------------------------------------

# a vector listing package names needed 

package.list <- c("here", #so I don't have to deal with setting a WD
                  "vegan", #
                  "tidyverse", #data cleaning
                  "dplyr", #data cleaning
                  "ggplot2", 
                  # "psych",
                  # "bipartite",
                  # "lme4",
                  # "scales",
                  # "forcats",
                  # "ggpubr",
                   "effect.lndscp"
                  # "gridExtra"
)


#installing the packages if they aren't already on the computer
  new.packages <- package.list[!(package.list %in% installed.packages()
                                 [,"Package"])]
  
  if(length(new.packages)) install.packages(new.packages)
  
#and loading the packages into R with a for loop
  for(i in package.list){library(i, character.only = T)}


#2 LOAD DATA--------------------------------------------------------------------

##2.1 LOAD CAPTURE DATA----
  
  #data collected from captures stored on Google Drive
  #https://tinyurl.com/2xp5dt45
  captures <- readr::read_csv(here::here("dados","caps_30jan2024_new.csv"))
  
##2.2 LOAD SEED DATA----
  
  #seed count and identification
  seeds <- readr::read_csv(here::here("dados","seeds_26JAN2024.csv"))
  
  #seed morphospeices
  morpho <- readr::read_csv(here::here("dados","morpho.csv"))
  
##2.3 LOAD FRUGIVORE DATA----
  frug <- readr::read_csv(here::here("dados","frugs_26JAN2024.csv"))
  
##2.4 LOAD DIET DATA----

  #Load elton traits
  elton.traits <- readr::read_tsv(here::here("dados","BirdFuncDat.txt"))


##2.5 LOAD PLOT DATA----
  
  #calculate abundance and diversity
  plots <- readr::read_csv(here::here("dados","newfor_database.csv"))
  plots <- as_tibble(plots)
  
  #plot age, coordinates, forest cover (mapBiomas) 
  plot_metadata <- readr::read_csv(here::here("dados", "plots_metadata.csv"))
  plot_metadata <- as_tibble(plot_metadata)

##2.6 LOAD DIPSERAL DATA----
  
  #dispersal data of trees in plots
  dispersal <- readr::read_csv(here::here("dados","dispersal.csv"))



#3 DATA CLEANING AND PREP-------------------------------------------------------

##3.1 CAPTURES----
  #transform into a tibble to facilitate using packages like dyplr 
  captures <- as_tibble(captures)
  
  #cut excess rows
  captures <- captures[-(492:999),]
  
  #cut excess columns
  captures <- captures[,-(27:34)]
  
  #subset captures to only include certain columns: capture date, session, day, 
  #plot har (time the net was opened) and hfr (time net was closed), species,
  #sample number, whether feces were collected (fc), if seeds were observed in the
  #feces (y = 1/n = 0) and if the bird is a recap  (y = 1/n = 0)
  subset.caps <- captures %>% 
    select(1:6, 11:12, 23, 25)

##3.2 SEEDS----
  subset.seeds <- seeds %>% 
    select(2, 3:4, 10)
  
  subset.seeds$no_seeds <- as.numeric(subset.seeds$no_seeds)
  
  subset.seeds$amostra <- as.character(subset.seeds$amostra)
  
  seed.sum.ind <- subset.seeds %>%                             
    group_by(amostra) %>%
    summarise(t_seeds = sum(no_seeds))
  
  
  seed.sum <- merge(x=seed.sum.ind, y=seeds,
                                     by="amostra", all.x =T) 
   
  
  
  final.seed <- seed.sum %>% 
    select(1:2, 7)
  
  
  correct.spp2 <- merge(x=subset.caps, y=final.seed,
                    by="amostra", all.x =T) 
  
  correct.spp2[is.na(correct.spp2)] <- 0


list.frug <- distinct(frug, especie, 
                     .keep_all = F)

list.frug$frug <- 1

correct.spp3 <- merge(x=correct.spp2, y=list.frug,
                      by="especie", all.x =T) 

correct.spp3[is.na(correct.spp3)] <- 0

##3.2 TRAITS-----

  #select the columns that interest us: family Latin name, species Latin name, 
  #% of diet that is fruit, diet category
  subset.traits <- elton.traits %>% select(4, 8, 16, 20, 26, 27, 28, 29 ,30, 36)
  
  colnames(subset.traits)[2] ="especie"
  
  correct.spp <- subset.traits %>% 
    mutate(especie = str_replace_all(especie, c("Veniliornis passerinus"= 
                                                  "Dryobates passerinus" , 
                                                "Hylocryptus rectirostris"=
                                                "Clibanornis rectirostris",
                                                "Tangara cayana" =
                                                "Stilpnia cayana", 
                                                "Basileuterus flaveolus"=
                                                "Myiothlypis flaveola",                                                
                                                "Tiaris fuliginosus" =
                                                  "Asemospiza fuliginosa"
                                                  )))
  
  #add the hybrid Basileuterus flaveolus x Basileuterus culicivorus and 
  #genus level classification of Sporophila species to elton traits so it can
  #match up with our list
  subset.traits <- rbind(correct.spp, list('Parulidae', 
                                         'Basileuterus flaveolus x Basileuterus culicivorus', 
                                         20, "Invertebrate", 0, 0, 0, 0, 0, 0))
  
  subset.traits <- rbind(subset.traits, list('Emberizidae', 'Sporophila sp.', 
                                         0, "PlantSeed", 0, 0, 0, 0, 0, 0))
  
  #rename second column as "especie" so you can merge with capture data 
  #colnames(subset.traits)[2] ="especie"

##3.3 PLOTS ------

  subset.plots <- plots %>% select(2, 7, 8, 11:13)
  
  colnames(subset.plots)[2] ="forest_type"
  
  plots_pasture <- filter(subset.plots, forest_type %in% 
                            "Passive restoration former pasturelands")
  
  colnames(plots_pasture)[c(3, 4)] =  c("parcela", "spp_code")
  
  class(plots_pasture$parcela)
  
  plots_pasture$parcela <- as.numeric(plots_pasture$parcela)
  
  my_plots <- filter(plots_pasture, parcela %in%
                            c(20, 22, 56, 64, 84, 87, 88, 90,
                              206, 207))
  
  #Mapbiomas 1km forest cover
  colnames(plot_metadata)[c(1, 5)] =  c("parcela", "one_km")
  
  plots.meta <- merge(x=my_plots, y=plot_metadata,
                       by="parcela", all.x =T)
  
  subset.dispersal <- dispersal %>% select(1:3)
  
  colnames(subset.dispersal)[c(1)] =  "spp_code"
  
  subset.dispersal$zoo_disp <- 1
  
  plots_final <- merge(x=plots.meta, y=subset.dispersal,
                      by="spp_code", all.x =T)
  
  plots_final <- plots_final %>% select(1:11, 14)
  
  plots_final$zoo_disp[is.na(plots_final$zoo_disp)] <- 0
  
  zoo_arv <- filter(plots_final, zoo_disp == 1)
  
  colnames(zoo_arv)[c(5, 6)] = c("family","species")
  
  plots_summary <- 
    zoo_arv %>%                             
    group_by(parcela) %>%
    summarise(p.age = max(age), p.abun = n(), p.div = n_distinct(species),
              p.cover1 = max(one_km), p.year = max(Year), p.altitude = max(altitude))
  
  plots_summary$pcover.avg <-c(0.192, 0.0966, 0.2513, 0.0997, 0.2452, 0.1875, 0.1383, 0.1922, 0.3138, 0.1699)
  
  plots_summary$pcover.10 <-c(13, 11, 10, 11, 12, 13, 12, 13, 10, 10)
  
  plots_summary$pcover.rez <- c(0.181454434, 0.177834571, 0.323241102, 0.360276251,
                               0.107927786, 0.141670249, 0.148447211, 0.129551932,
                               0.309754024, 0.1417475)
  
  plots_summary$p.age <- as.numeric(plots_summary$p.age)
  
  plots_summary$p.new_age <- (5 + plots_summary$p.age)
  
  write.csv(plots_summary, here::here( "resultados",
                                       "plots_summary.csv"))
  


##3.4 MERGE INTO ONE SPREADSHEET-----

  #left join the two to match species with diet
  data.traits <- merge(x=correct.spp3, y=subset.traits,
                      by="especie", all.x =T)
  
  #rename columns 
  colnames(data.traits)[c(10, 14, 15, 16, 17, 18, 19, 20, 21, 22)] =  
    c("recap", "familia", "fruit", "guild", "fs.ground", "fs.understory",
      "fs.midhigh", "fs.canopy", "fs.aerial", "body_mass")
  
  #add column defining fruit consuming or not, binary
  data.traits <- data.traits %>%  
    mutate(cons_fruto = case_when(`fruit`> 0 ~ 1,
                                  `fruit` == 0 ~ 0,))
  
  #frugivores <- filter(correct.spp2, genetic == 1)
  
  master <- data.traits
  
  master <- master[,c(3, 4, 5, 7, 8, 6, 14, 1, 2, 9, 10, 12, 11, 13, 23, 16, 15,
                      22, 17:21)]

#4 SPECIES LIST-----------------------------------------------------------------

#Create a list of species captured with data about diet type 
#(frugivorous or not), as well as the number of fezes samples collected per
#species 

#delete no capture days
caps.traits <- master[!grepl("no_captures", master$especie),]


#filter to make a list of all unique species with the family and whether the 
#relative importance of fruit to their diet 
list.spp <- distinct(caps.traits, especie, familia, frug, 
                      .keep_all = FALSE)

#filter captures with feces samples collected
amostras <- filter(caps.traits, fc == "y")


#summarize the number of samples per species 
list.amostra <- 
  amostras  %>%  
  group_by(especie)  %>%  
  summarise(amostras = n())

amostras.frug <- filter(amostras, frug == 1)

list.amostra.frug <- 
  amostras.frug  %>%  
  group_by(especie)  %>%  
  summarise(amostras = n())

#merge to create a list of species captured with number of fezes collected
#from each
caps.amostras <- merge(x=list.spp, y=list.amostra,
                    by="especie", all.x =T)

#fill NAs with 0s, NAs created are due to absence on list because no samples
#were collected
caps.amostras[is.na(caps.amostras)] <- 0

#write csv
write.csv(caps.amostras, here::here( "resultados",
                                 "caps_amostras.csv"))

#5 SUMMARY TABLE BY PLOT--------------------------------------------------------

#In order to create a summary table by plot, we will start by calculating the 
#net hours. Net hours are the number of hours we had the nets open times the 
#number of nets we had out (5)

#I decided to calculate the number of net hours for days with captures and without
#captures separately, since calculating them together resulted in a lot of bugs
#in code I had previously written. It's messy, but it gets the job done, yay!


net_hours <- master %>% 
  select(1:6)

net_hours <- net_hours %>% 
    distinct(data, .keep_all = T)

net.hours.caps <- net_hours %>%  
  mutate(rede_hora = ((haf - har)/3600)*5)


nh <- 
  net.hours.caps %>%                            
  group_by(parcela) %>%
  summarise(rede_hora = sum(rede_hora))


#define recap and seeds columns as numeric to allow for calcuations 
#(previously defined as characters)
caps.traits$recap <- as.numeric(caps.traits$recap)
caps.traits$seeds <- as.numeric(caps.traits$seeds)

#summarize the number of species, number of samples, recaps, samples with seeds
#and samples from frugivores from each plot
all.parcelas <- 
  caps.traits %>%                            
  group_by(parcela) %>%
  summarise(especies = n_distinct(especie), 
            capturas = n_distinct(amostra), 
            recap = sum(recap),
            a_c_seeds = sum(seeds),
            frug = sum(frug))

seed.sum.par <- subset.seeds %>%                             
  group_by(parcela) %>%
  summarise(seeds_parcela = sum(no_seeds))

all.parcelas$t_seeds <- seed.sum.par$seeds_parcela

all.parcelas$rede_hora <- nh$rede_hora

#define net hours as numeric, instead of as time, to allow for calculations
all.parcelas$rede_hora <- as.numeric(all.parcelas$rede_hora)

#make net hours an integer, just to make it pretty 
all.parcelas$rede_hora<- as.integer(all.parcelas$rede_hora)

#now we look at just captures which we successfully collected feces samples from
#to summarize how many species we collected samples from, how many samples we 
#collected and how many samples we collected from frugivores
parcela.c.amostra <- 
  amostras %>%                             
  group_by(parcela) %>%
  summarise(spp_c_amostra = n_distinct(especie), 
            n_amostras = n_distinct(amostra), 
            frug_c_amostra = sum(frug))

amostras.frug <- filter(amostras, frug == 1)

parcela.c.amostra_frug <- 
  amostras.frug %>%                             
  group_by(parcela) %>%
  summarise(spp_c_amostra_frug = n_distinct(especie))

#combine the two tables 
plot.sum <- merge(x=all.parcelas, y=parcela.c.amostra,
                       by="parcela", all.x =T)

plot.sum2 <- merge(x=plot.sum, y=parcela.c.amostra_frug,
                  by="parcela", all.x =T)


#calculate capture rate for each plot (number of captures divided by the number
#of net hours)
all.sum <- 
  plot.sum2 %>%                            
  mutate(taxa_cap = (capturas/(rede_hora)))

all.sum$taxa_cap_frug <- (all.sum$frug/all.sum$rede_hora)

#frugivorous spp

frug_only <- filter(master, frug == 1)

frug_spp <- 
  frug_only %>%                             
  group_by(parcela) %>%
  summarise(spp_frug = n_distinct(especie))

frug_spp2 <- 
  frug_only %>%                             
  group_by(especie) %>%
  summarise(amos_seeds = sum(seeds), sem = sum(t_seeds))

frug_spp3 <- 
  frug_only %>%                             
  group_by(parcela, especie) %>%
  summarise(amos_seeds = sum(seeds), sem = sum(t_seeds))

caps.frug <- merge(x=caps.amostras, y=frug_spp2,
                 by="especie", all.x =T)

# caps.frug.dna <- merge(x=caps.frug, y=spp1,
#                    by="especie", all.x =T)
# 
# write.csv(caps.frug.dna, here::here( "resultados",
#                                "caps.frug.dna.csv"))


all.sum <- merge(x=all.sum, y=frug_spp,
                   by="parcela", all.x =T)


plot.data <- merge(x=all.sum, y=plots_summary,
                   by="parcela", all.x =T)

write.csv(plot.data, here::here( "resultados",
                                     "plot.data.csv"))

#6 EXPLORATORY GRAPHS -----

#box plots showing activity (capture rate) on the X axis in each plot (Y) axis,
#ordered by the following variables:

        #connectivity/forest cover
          #1 mean, all captures
          #2 per species, all captures
          #3 mean, frugivores only
          #4 per species, frugivores only

        #age
          #1 mean, all captures
          #2 per species, all captures
          #3 mean, frugivores only
          #4 per species, frugivores only

        #div tree spp. zoo
          #1 mean, all captures
          #2 per species, all captures
          #3 mean, frugivores only
          #4 per species, frugivores only

        #abun tree spp. zoo
          #1 mean, all captures
          #2 per species, all captures
          #3 mean, frugivores only
          #4 per species, frugivores only

##6.1 CALCULATE CAPTURE RATE--------

#capture rate for all captures

cap.rate.all <- 
  master %>%                            
  group_by(parcela, especie) %>%
  summarise(spp.captura = n_distinct(amostra))


cap.rate.all <- merge(x=cap.rate.all, y=all.sum,
                      by="parcela", all.x =T)

cap.rate.all <- cap.rate.all %>% 
  select(1:3, 10)

cap.rate.all$spp.cap.rate <- (cap.rate.all$spp.captura/cap.rate.all$rede_hora)

cap.rate.all <- merge(x=cap.rate.all, y=plots_summary,
                      by="parcela", all.x =T)

#capture rate for frugivores only

cap.rate.fru <- 
  frug_only %>%                            
  group_by(parcela, especie) %>%
  summarise(spp.captura = n_distinct(amostra))


cap.rate.fru <- merge(x=cap.rate.fru, y=all.sum,
                      by="parcela", all.x =T)

cap.rate.fru <- cap.rate.fru %>% 
  select(1:3, 10)

cap.rate.fru$spp.cap.rate <- (cap.rate.fru$spp.captura/cap.rate.fru$rede_hora)

cap.rate.fru <- merge(x=cap.rate.fru, y=plots_summary,
                      by="parcela", all.x =T)

##6.2 CAPTURE RATE & FOREST TRAITS------
###6.2.1 CONNECTIVITY & CAPTURE RATE--------

#connectivity, average capture rate, all species  
    mean_conn <- ggplot(plot.data, aes(y = taxa_cap, 
                                       x = reorder(parcela, p.cover1))) +
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") +
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (all)",
           x = "plot (ordered by connectivity)")+
      theme( axis.text.x = element_text(face = "bold",colour = "black", size = 10),
             axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
             axis.title= element_text(face = "bold", size = 12, colour = "black"),
             panel.background = element_blank(),
             panel.border = element_rect(fill = NA, colour = "black"),
             strip.text = element_text(size = 9, face = "bold"))

    mean_conn
    
    # mean_conn2 <- ggscatter(conn_mean, x = "p_conn_ordered", y = "taxa_cap",
    #                       add = "none", conf.int = T,
    #                       cor.coef = T, cor.method = "pearson",
    #                       xlab = "plot (ordered by connectivity)",
    #                       ylab = "capture rate (all)")+
    #   geom_point(size =2, alpha = 1) +
    #   theme (panel.border = element_rect(fill = NA, colour = "black"))
    # 
    # mean_conn2

#connectivity, per species capture rate, all species 

    box_c <- ggplot(cap.rate.all, aes(x= reorder(parcela, p.cover1),
                    y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (all)", 
           x = "plot (ordered by connectivity)") + 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                         colour = "black"),
             axis.title.y = element_text(face = "bold", size = 9, 
                                         colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    box_c

#connectivity, average capture rate, frugivores only

    mean_conn_f <- ggplot(plot.data, aes(y = taxa_cap_frug,
                                           x = reorder(parcela, p.cover1))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (frugivores only)", 
           x = "plot (ordered by connectivity)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                      colour = "black"),
             axis.title.y = element_text(face = "bold", size = 9, 
                                       colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_conn_f
    

#connectivity, per species capture rate, frugivores only

    box_c_f <- ggplot(cap.rate.fru, aes(x=reorder(parcela, p.cover1), 
                                      y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (frugivores only)", 
           x = "plot (ordered by connectivity)") + 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                         colour = "black"),
             axis.title.y = element_text(face = "bold", size = 9, 
                                         colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    box_c_f

###6.2.2 AGE & CAPTURE RATE------
#age, average capture rate, all species
    
    mean_age <- ggplot(plot.data, aes(y = taxa_cap,
                                      x = reorder(parcela, p.new_age))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (all)", 
           x = "plot (ordered by age)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title= element_text(face = "bold", size = 12,
                                      colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_age

#age, per species capture rate, all species

    box_age <- ggplot(cap.rate.all, aes(x=reorder(parcela, p.new_age), 
                                   y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (all)", 
           x = "plot (ordered by age)") + 
      theme(axis.text.x = element_text(face = "bold",
                                       colour = "black", size = 10), 
            axis.text.y = element_text(face = "bold", size = 10, 
                                       colour = "black"), 
            axis.title.x = element_text(face = "bold", size = 12, 
                                        colour = "black"),
            axis.title.y = element_text(face = "bold", size = 9, 
                                        colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 12, face = "bold"))
    
    box_age

#age, average capture rate, frugivores only

    mean_age_f <- ggplot(plot.data, aes(y = taxa_cap_frug, 
                                        x = reorder(parcela, p.new_age))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (frugivores only)", 
           x = "plot (ordered by age)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                      colour = "black"), 
             axis.title.y = element_text(face = "bold", size = 9, 
                                      colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_age_f

#age, per species capture rate, frugivores only
    
    box_age_f <- ggplot(cap.rate.fru, aes(x=reorder(parcela, p.new_age), 
                                   y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (frugivores only)", 
           x = "plot (ordered by age)") + 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                         colour = "black"),
             axis.title.y = element_text(face = "bold", size = 9, 
                                         colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    box_age_f

###6.2.3 DIVERSITY & CAPTURE RATE------

#div, average capture rate, all species
    
    mean_div <- ggplot(plot.data, aes(y = taxa_cap,
                                      x = reorder(parcela, p.div))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (all)", 
           x = "plot (ordered by zoo tree spp richness)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title= element_text(face = "bold", size = 12,
                                      colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_div

#div, per species capture rate, all species
    
    box_div <- ggplot(cap.rate.all, aes(x=reorder(parcela, p.div), 
                                        y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (all)", 
           x = "plot (ordered by zoo tree spp richness)") + 
      theme(axis.text.x = element_text(face = "bold",
                                       colour = "black", size = 10), 
            axis.text.y = element_text(face = "bold", size = 10, 
                                       colour = "black"), 
            axis.title.x = element_text(face = "bold", size = 12, 
                                        colour = "black"),
            axis.title.y = element_text(face = "bold", size = 9, 
                                        colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 12, face = "bold"))
    
    box_div
  
#div, average capture rate, frugivores only
    
    mean_div_f <- ggplot(plot.data, aes(y = taxa_cap_frug, 
                                        x = reorder(parcela, p.div))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (frugivores only)", 
           x = "plot (ordered by zoo tree spp richness)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                      colour = "black"), 
             axis.title.y = element_text(face = "bold", size = 9, 
                                      colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_div_f
    
#div, per species capture rate, frugivores only
    
    box_div_f <- ggplot(cap.rate.fru, aes(x=reorder(parcela, p.div), 
                                          y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (frugivores only)", 
           x = "plot (ordered by zoo tree spp richness)") + 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                         colour = "black"),
             axis.title.y = element_text(face = "bold", size = 9, 
                                         colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    box_div_f
    


###6.2.4 ABUNDANCE & CAPTURE RATE------
    
#abun, average capture rate, all species
    
    mean_abun <- ggplot(plot.data, aes(y = taxa_cap,
                                      x = reorder(parcela, p.abun))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (all)", 
           x = "plot (ordered by zoo tree abundance)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title= element_text(face = "bold", size = 12,
                                      colour = "black"), 
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_abun
    
#div, per species capture rate, all species
    
    box_abun <- ggplot(cap.rate.all, aes(x=reorder(parcela, p.abun), 
                                        y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (all)", 
           x = "plot (ordered by zoo tree abundance)") + 
      theme(axis.text.x = element_text(face = "bold",
                                       colour = "black", size = 10), 
            axis.text.y = element_text(face = "bold", size = 10, 
                                       colour = "black"), 
            axis.title.x = element_text(face = "bold", size = 12, 
                                        colour = "black"),
            axis.title.y = element_text(face = "bold", size = 9, 
                                        colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 12, face = "bold"))
    
    box_abun
    
#div, average capture rate, frugivores only
    
    mean_abun_f <- ggplot(plot.data, aes(y = taxa_cap_frug, 
                                        x = reorder(parcela, p.abun))) + 
      geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
      geom_point(size =2, alpha = 1) +
      labs(y = "capture rate (frugivores only)", 
           x = "plot (ordered by zoo tree abundance)")+ 
      theme( axis.text.x = element_text(face = "bold",
                                        colour = "black", size = 10), 
             axis.text.y = element_text(face = "bold", size = 10, 
                                        colour = "black"), 
             axis.title.x = element_text(face = "bold", size = 12, 
                                      colour = "black"), 
             axis.title.y = element_text(face = "bold", size = 10, 
                                      colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    
    mean_abun_f
    
    
#div, per species capture rate, frugivores only
    
    box_abun_f <- ggplot(cap.rate.fru, aes(x=reorder(parcela, p.abun), 
                                          y=spp.cap.rate)) + 
      geom_boxplot() + 
      geom_jitter(alpha = 0.8, width = 0.2) +
      #scale_y_log10() +
      labs(y = "capture rate per species (frugivores only)", 
           x = "plot (ordered by zoo tree abundance)") + 
      theme(axis.text.x = element_text(face = "bold",
                                       colour = "black", size = 10), 
            axis.text.y = element_text(face = "bold", size = 10, 
                                       colour = "black"), 
            axis.title.x = element_text(face = "bold", size = 12, 
                                        colour = "black"),
            axis.title.y = element_text(face = "bold", size = 10, 
                                        colour = "black"),
             panel.background = element_blank(), 
             panel.border = element_rect(fill = NA, colour = "black"), 
             strip.text = element_text(size = 9, face = "bold"))
    box_abun_f
    


###6.2.5 COMBINE THE PLOTS-------------
graphics.off()
    
cap.all.graph <- ggarrange(mean_conn, mean_age, mean_div, mean_abun, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

cap.all.graph

graphics.off()

cap.m.graph <- ggarrange(box_c, box_age, box_div, box_abun, 
                           labels = c("A", "B", "C", "D"),
                           ncol = 2, nrow = 2)
cap.m.graph

graphics.off()

cap.f.graph <- ggarrange(mean_conn_f, mean_age_f, mean_div_f, mean_abun_f,
                         labels = c("A", "B", "C", "D"), label.y = 1.02,
                         ncol = 2, nrow = 2)
cap.f.graph

graphics.off()

cap.f.a.graph <- ggarrange(box_c_f, box_age_f, box_div_f, box_abun_f, 
                         labels = c("A", "B", "C", "D"), label.y = 1.02,
                         ncol = 2, nrow = 2)
cap.f.a.graph

#7 EXPLORATORY MODELS ------


 model_SDE_cover <- lm(SDE_sample_f ~ p.cover1, 
                          data = plot.data)

summary(model_SDE_cover)


model_SDE_age <- lm(SDE_sample_f ~ p.new_age, data = plot.data)

summary(model_SDE_age)


model_SDE_rich <- lm(SDE_sample_f ~ p.div, data = plot.data)

summary(model_SDE_rich)


model_SDE_abun <- lm(SDE_sample_f ~ p.abun, data = plot.data)

summary(model_SDE_abun)

##frug



#8 SEEDSCAPE---------
##8.1 BY SPECIES------

#seedscape summing all plots by species

#calculate standard error

SDE_error <- SDE_master %>%                             
  group_by(especie) %>%
  summarise(SDE.a.max = max(SDE_amos), SDE.a.min = min(SDE_amos),
            SDE.s.max = max(SDE_seeds), SDE.s.min = min(SDE_seeds),
            taxa.max = max(taxcap), taxa.min = min(taxcap))

##

###8.1.1 FREQUENCY OF SEEDS FOUND IN FECES----

#frequency of seeds found in feces 

seed_disp  <- 
  seed.sum %>%  
  group_by(especie)  %>%  
  summarise(amostras.seeds = n())

seed_frug <- merge(x=seed_disp, y=caps.amostras,
                    by="especie", all.x =T)

seed_frug$freq.seed <- (seed_frug$amostras.seeds/seed_frug$amostras)

#calculate number of captures
list.caps <- 
  master  %>%  
  group_by(especie)  %>%  
  summarise(capturas = n())


cap.seed <- merge(x=seed_frug, y=list.caps,
                   by="especie", all.x =T)

cap.seed$taxa_cap <- (cap.seed$capturas/2758)

cap.seed2 <- merge(x=cap.seed, y=SDE_error,
                    by="especie", all.x =T)


effectiveness_plot(cap.seed2$taxa_cap, cap.seed$freq.seed, 
                   label = cap.seed$especie, 
                   myxlab = "Capture Rate", 
                   myylab = "Frequency of Seed Dispersal",
                   lines.color = "#607729",
                   pts.size = 4,
                   label.size = 4, italic = T)
  # geom_errorbar(ymax = cap.seed2$taxa.max, ymin = cap.seed2$taxa.min,
  #               xmax = cap.seed2$SDE.a.max, xmin = cap.seed2$SDE.a.min)
dev.off()
graphics.off()

seedscape <- ggplot(cap.seed2, aes(x = taxa_cap, y = freq.seed, 
                                   label = especie)) +
  geom_point(size = 2, alpha = 1) +
  #geom_pointrange(aes(xmax = taxa.max, xmin = taxa.min)) +
  labs(y = "frequency of seeds found in feces",
       x = "capture rate")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12),
         axis.text.y = element_text(face = "bold", size = 12, colour = "black"),
         axis.title= element_text(face = "bold", size = 14, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold")) +
  geom_isobands +
  geom_text(hjust=0, vjust=0) +
  xlim(0.0001, 0.011)

seedscape

###8.1.2 AVERAGE NUMBER OF SEEDS FOUND IN FECES------

seed.sum.spp.amostra <- seed.sum %>%
  group_by(especie) %>%
  summarise(amostra_seeds = sum(seeds))

seed.sum.spp <- subset.seeds %>%                             
  group_by(especie) %>%
  summarise(t_seeds_spp = sum(no_seeds))

seed.sum.spp.all <- merge(x=seed.sum.spp, y=seed.sum.spp.amostra,
                          by="especie", all.x =T)

frug.amostras <- filter(caps.amostras, frug == 1)

frug.amostras.seeds <- merge(x=frug.amostras, y=seed.sum.spp.all,
      by="especie", all.x =T) 

frug.amostras.seeds[is.na(frug.amostras.seeds)] <- 0

frug.amostras.seeds <- filter(frug.amostras.seeds, amostra_seeds >= 1)

frug.amostras.seeds$avg_seeds <- frug.amostras.seeds$t_seeds_spp/frug.amostras.seeds$amostras

frug.amostras.seeds$taxa <- cap.seed$taxa



effectiveness_plot(frug.amostras.seeds$taxa,
                   frug.amostras.seeds$avg_seeds, 
                   label = frug.amostras.seeds$especie,  
                   myxlab = "capture rate (no. ind captured/total net hours)", 
                   myylab = "average number of seeds/feces")



##8.2 BY PLOT ------ 

###8.2.1 FREQUENCY OF SEEDS FOUND IN FECES, FRUGIVORES ONLY------
#frequency of seeds in feces, frugivores only 
plot.data$s_parcela_f <- ((plot.data$a_c_seeds)/(plot.data$frug_c_amostra))

plot.data$plot <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")

ep_p_amos_fru <- effectiveness_plot(plot.data$taxa_cap_frug,
                                     plot.data$s_parcela_f, 
                   label = plot.data$plot,  
                   myxlab = "Capture Rate", 
                   myylab = "Seed Dispersal Frequency",
                   pts.size = 4,
                   label.size = 6)
ep_p_amos_fru

###8.2.2 FREQUENCY OF SEEDS FOUND IN FECES, ALL SPP------
#frequency of seeds in feces, all
plot.data$s_parcela <- ((plot.data$a_c_seeds)/(plot.data$n_amostras))

ep_p_amos_all <- effectiveness_plot(plot.data$taxa_cap, plot.data$s_parcela, 
                   label = plot.data$parcela,  
                   myxlab = "capture rate (no. ind captured/total net hours)", 
                   myylab = "frequency of seeds found in feces",
                   pts.size = 4,
                   label.size = 10)

###8.2.3 AVERAGE NUMBER OF SEEDS FOUND IN FECES, FRUGIVORES ONLY------
#average number of seeds in feces

  #frugivores
  plot.data$avg_seeds <- (plot.data$t_seeds/plot.data$frug_c_amostra)
  #frugivores only
  ep_p_seeds_fru <- effectiveness_plot(plot.data$taxa_cap_frug, 
                                       plot.data$avg_seeds, 
                                       label = plot.data$parcela,  
                                       myxlab = "capture rate frug (no. ind captured/total net hours)", 
                                       myylab = "average number of seeds found in feces of frug")

###8.2.3 AVERAGE NUMBER OF SEEDS FOUND IN FECES, ALL SPP---------
#all
  plot.data$avg_seeds_all <- (plot.data$t_seeds/plot.data$n_amostras)

#all
ep_p_seeds_all <- effectiveness_plot(plot.data$taxa_cap, plot.data$avg_seeds_all, 
                   label = plot.data$parcela,  
                   myxlab = "capture rate (no. ind captured/total net hours)", 
                   myylab = "average number of seeds found in feces")

###8.2.4 COMBINE THE PLOTS SEEDSCAPE DIFFERENT LEVELS------
graphics.off()

seedscapes <- ggarrange(ep_p_amos_fru, ep_p_amos_all, 
                            ep_p_seeds_fru, ep_p_seeds_all,
                            labels = c("A", "B", "C", "D"), label.y = 1.02,
                            ncol = 2, nrow = 2)
seedscapes

#9 AVERAGE SDE PER PLOT-------

#just frugivores

plot.data$SDE_sample_f <- ((plot.data$s_parcela_f)*plot.data$taxa_cap_frug)

plot.data$SDE_seeds_f <- ((plot.data$avg_seeds)*plot.data$taxa_cap_frug)

#all spp., average seeds and capture rate

plot.data$SDE_sample <- ((plot.data$s_parcela)*plot.data$taxa_cap)

plot.data$SDE_seeds <- ((plot.data$avg_seeds_all)*plot.data$taxa_cap)

aov_plots <- aov(SDE_sample_f ~ parcela, plot.data)

summary(aov_plots)

#test normality 

shapiro.test(plot.data$SDE_sample_f)

##9.1 CONNECTIVITY SDE--------

#connectivity, SDE freq of seeds in sample, frugivores only  
mean_conn_SDE <- ggplot(plot.data, aes(y = SDE_sample_f, 
                                   x = p.cover1)) +
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") +
  geom_point(size =4, alpha = 1) +
  labs(y = "Plot-level SDE",
       x = "Forest Cover (1 km)")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12),
         axis.text.y = element_text(face = "bold", size = 12, colour = "black"),
         axis.title.x= element_text(face = "bold", size = 18, colour = "black"),
         axis.title.y= element_text(face = "bold", size = 18, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

mean_conn_SDE

#connectivity, SDE average number of seeds in sample, frugivores only

mean_conn_SDE_seeds <- ggplot(plot.data, aes(y = SDE_seeds_f,
                                     x = reorder(parcela, p.cover1))) + 
  geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =2, alpha = 1) +
  labs(y = "SDE (seeds/sample)", 
       x = "plot (ordered by connectivity)")+ 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

mean_conn_SDE_seeds

##9.2 AGE SDE------

#age, SDE freq of seeds in sample, frugivores only  

mean_age_SDE <- ggplot(plot.data, aes(y = SDE_sample_f,
                                  x = p.new_age)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Plot-level SDE", 
       x = "Age")+ 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12),
         axis.text.y = element_text(face = "bold", size = 12, colour = "black"),
         axis.title.x= element_text(face = "bold", size = 18, colour = "black"),
         axis.title.y= element_text(face = "bold", size = 18, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

mean_age_SDE

#age, SDE average number of seeds in sample, frugivores only

mean_age_SDE_seeds <- ggplot(plot.data, aes(y = SDE_seeds_f, 
                                    x = reorder(parcela, p.new_age))) + 
  geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =2, alpha = 1) +
  labs(y = "SDE (seeds/sample)", 
       x = "plot (ordered by age)")+ 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"), 
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

mean_age_SDE_seeds


##9.3 DIVERSITY SDE------

#div, SDE freq of seeds in sample, frugivores only  

mean_div_SDE <- ggplot(plot.data, aes(y = SDE_sample_f,
                                  x = p.div)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Plot-level SDE", 
       x = "Zoochoric Tree Species Richness")+ 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12),
         axis.text.y = element_text(face = "bold", size = 12, colour = "black"),
         axis.title.x= element_text(face = "bold", size = 18, colour = "black"),
         axis.title.y= element_text(face = "bold", size = 18, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

mean_div_SDE


#div, SDE average number of seeds in sample, frugivores only

mean_div_SDE_seeds <- ggplot(plot.data, aes(y = SDE_seeds_f, 
                                    x = reorder(parcela, p.div))) + 
  geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =2, alpha = 1) +
  labs(y = "SDE (seeds/sample)", 
       x = "plot (ordered by zoo tree spp richness)")+ 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"), 
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

mean_div_SDE_seeds



##9.4 ABUNDANCE SDE-------

#abun, average capture rate, frugivores only

mean_abun_SDE <- ggplot(plot.data, aes(y = SDE_sample_f,
                                   x = p.abun)) + 
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =4, alpha = 1) +
  labs(y = "Plot-level SDE", 
       x = "Zoochoric Tree Species Abundance")+ 
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12),
         axis.text.y = element_text(face = "bold", size = 12, colour = "black"),
         axis.title.x= element_text(face = "bold", size = 18, colour = "black"),
         axis.title.y= element_text(face = "bold", size = 18, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

mean_abun_SDE

#abun, average capture rate, frugivores only

mean_abun_SDE_seeds <- ggplot(plot.data, aes(y = SDE_seeds_f, 
                                     x = reorder(parcela, p.abun))) + 
  geom_smooth(method = "lm", alpha =0.51, colour = "blue") + 
  geom_point(size =2, alpha = 1) +
  labs(y = "SDE (seeds/sample)", 
       x = "plot (ordered by zoo tree abundance)")+ 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"), 
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

mean_abun_SDE_seeds

##9.5 COMBINE THE PLOTS SDE AVERAGE-------------
graphics.off()

SDE.sample.graph <- ggarrange(mean_conn_SDE, mean_age_SDE, mean_div_SDE, 
                              mean_abun_SDE, 
                           labels = c("A", "B", "C", "D"),
                           ncol = 2, nrow = 2)

SDE.sample.graph

ggsave(
  filename = here::here("SDE_habitat-feat.png"),
  plot = SDE.sample.graph,
  width = 30, 
  height = 20, 
  units = "cm", 
  dpi = 720)

graphics.off()

SDE.seed.graph <- ggarrange(mean_conn_SDE_seeds, mean_age_SDE_seeds, 
                            mean_div_SDE_seeds, mean_abun_SDE_seeds,
                         labels = c("A", "B", "C", "D"),
                         ncol = 2, nrow = 2)
SDE.seed.graph



#10 SDE PER SPECIES PER PLOT------

subset.master <- master %>% 
  select(6, 8, 9, 10, 12, 13, 14)

#parcela 20

    parcela_20 <- filter(subset.master, parcela == 20)
    
    p20_caps <- parcela_20 %>%                             
      group_by(especie) %>%
      summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())
    
    p20_amos <- filter(parcela_20, fc == "y", frug == "1")
    
    p20_amos <- p20_amos %>%                             
      group_by(especie) %>%
      summarise(amos = n())
    
    p20_caps$taxcap <- (p20_caps$ind/275)
    
    
    p20_seeds <- filter(p20_caps, seeds >= 1)
    
    p20_SDE <- merge(x=p20_seeds, y=p20_amos,
               by="especie", all.x =T) 
    
    
    p20_SDE$id <- c("A_gal20", "E_pen20", "M_man20", "M_fer20")
    
    
    p20_SDE$s_amos <- (p20_SDE$seeds)/(p20_SDE$amos)
    
    p20_SDE$s_total <- (p20_SDE$t_seeds)/(p20_SDE$amos)
    
    p20_SDE$parcela <- "20"

#parcela 22

parcela_22 <- filter(subset.master, parcela == 22)

p22_caps <- parcela_22 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p22_amos <- filter(parcela_22, fc == "y", frug == "1")

p22_amos <- p22_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p22_caps$taxcap <- (p22_caps$ind/280)


p22_seeds <- filter(p22_caps, seeds >= 1)

p22_SDE <- merge(x=p22_seeds, y=p22_amos,
                 by="especie", all.x =T) 


p22_SDE$id <- c("A_gal22", "E_pen22")


p22_SDE$s_amos <- (p22_SDE$seeds)/(p22_SDE$amos)

p22_SDE$s_total <- (p22_SDE$t_seeds)/(p22_SDE$amos)

p22_SDE$parcela <- "22"


#parcela 56

parcela_56 <- filter(subset.master, parcela == 56)

p56_caps <- parcela_56 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p56_amos <- filter(parcela_56, fc == "y", frug == "1")

p56_amos <- p56_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p56_caps$taxcap <- (p56_caps$ind/301)


p56_seeds <- filter(p56_caps, seeds >= 1)

p56_SDE <- merge(x=p56_seeds, y=p56_amos,
                 by="especie", all.x =T) 


p56_SDE$id <- c("A_gal56", "E_pen56", "L_eul56", "T_cor56", "T_leu56")


p56_SDE$s_amos <- (p56_SDE$seeds)/(p56_SDE$amos)

p56_SDE$s_total <- (p56_SDE$t_seeds)/(p56_SDE$amos)

p56_SDE$parcela <- "56"

#parcela 64

parcela_64 <- filter(subset.master, parcela == 64)

p64_caps <- parcela_64 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p64_amos <- filter(parcela_64, fc == "y", frug == "1")

p64_amos <- p64_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p64_caps$taxcap <- (p64_caps$ind/301)


p64_seeds <- filter(p64_caps, seeds >= 1)

p64_SDE <- merge(x=p64_seeds, y=p64_amos,
                 by="especie", all.x =T) 


p64_SDE$id <- c("A_gal64", "C_fus64", "E_pen64","T_cor64")


p64_SDE$s_amos <- (p64_SDE$seeds)/(p64_SDE$amos)

p64_SDE$s_total <- (p64_SDE$t_seeds)/(p64_SDE$amos)

p64_SDE$parcela <- "64"


#parcela 84

parcela_84 <- filter(subset.master, parcela == 84)

p84_caps <- parcela_84 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p84_amos <- filter(parcela_84, fc == "y", frug == "1")

p84_amos <- p84_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p84_caps$taxcap <- (p84_caps$ind/269)


p84_seeds <- filter(p84_caps, seeds >= 1)

p84_SDE <- merge(x=p84_seeds, y=p84_amos,
                 by="especie", all.x =T) 


p84_SDE$id <- c("A_gal84", "E_pen84","N_pil84", "T_ama84", "T_leu84")


p84_SDE$s_amos <- (p84_SDE$seeds)/(p84_SDE$amos)

p84_SDE$s_total <- (p84_SDE$t_seeds)/(p84_SDE$amos)

p84_SDE$parcela <- "84"

#parcela 88

parcela_87 <- filter(subset.master, parcela == 87)

p87_caps <- parcela_87 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p87_amos <- filter(parcela_87, fc == "y", frug == "1")

p87_amos <- p87_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p87_caps$taxcap <- (p87_caps$ind/271)


p87_seeds <- filter(p87_caps, seeds >= 1)

p87_SDE <- merge(x=p87_seeds, y=p87_amos,
                 by="especie", all.x =T) 


p87_SDE$id <- c("A_gal87", "A_fla87","M_mac87", "R_car87", "T_say87")


p87_SDE$s_amos <- (p87_SDE$seeds)/(p87_SDE$amos)

p87_SDE$s_total <- (p87_SDE$t_seeds)/(p87_SDE$amos)

p87_SDE$parcela <- "87"

#parcela 88

parcela_88 <- filter(subset.master, parcela == 88)

p88_caps <- parcela_88 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p88_amos <- filter(parcela_88, fc == "y", frug == "1")

p88_amos <- p88_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p88_caps$taxcap <- (p88_caps$ind/274)


p88_seeds <- filter(p88_caps, seeds >= 1)

p88_SDE <- merge(x=p88_seeds, y=p88_amos,
                 by="especie", all.x =T) 


p88_SDE$id <- c("A_gal88", "T_cor88","T_dsor88", "T_ama88", "T_leu88")


p88_SDE$s_amos <- (p88_SDE$seeds)/(p88_SDE$amos)

p88_SDE$s_total <- (p88_SDE$t_seeds)/(p88_SDE$amos)

p88_SDE$parcela <- "88"


#parcela 90

parcela_90 <- filter(subset.master, parcela == 90)

p90_caps <- parcela_90 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p90_amos <- filter(parcela_90, fc == "y", frug == "1")

p90_amos <- p90_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p90_caps$taxcap <- (p90_caps$ind/281)


p90_seeds <- filter(p90_caps, seeds >= 1)

p90_SDE <- merge(x=p90_seeds, y=p90_amos,
                 by="especie", all.x =T) 


p90_SDE$id <- c("A_gal90", "L_ama90","M_fer90", "P_sul90", "T_ama90")


p90_SDE$s_amos <- (p90_SDE$seeds)/(p90_SDE$amos)

p90_SDE$s_total <- (p90_SDE$t_seeds)/(p90_SDE$amos)

p90_SDE$parcela <- "90"

#parcela 206

parcela_206 <- filter(subset.master, parcela == 206)

p206_caps <- parcela_206 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p206_amos <- filter(parcela_206, fc == "y", frug == "1")

p206_amos <- p206_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p206_caps$taxcap <- (p206_caps$ind/281)


p206_seeds <- filter(p206_caps, seeds >= 1)

p206_SDE <- merge(x=p206_seeds, y=p206_amos,
                 by="especie", all.x =T) 


p206_SDE$id <- c("A_gal206", "T_leu206")


p206_SDE$s_amos <- (p206_SDE$seeds)/(p206_SDE$amos)

p206_SDE$s_total <- (p206_SDE$t_seeds)/(p206_SDE$amos)

p206_SDE$parcela <- "206"


#parcela 207

parcela_207 <- filter(subset.master, parcela == 207)

p207_caps <- parcela_207 %>%                             
  group_by(especie) %>%
  summarise(seeds = sum(seeds), t_seeds = sum(t_seeds), ind = n())

p207_amos <- filter(parcela_207, fc == "y", frug == "1")

p207_amos <- p207_amos %>%                             
  group_by(especie) %>%
  summarise(amos = n())

p207_caps$taxcap <- (p207_caps$ind/281)


p207_seeds <- filter(p207_caps, seeds >= 1)

p207_SDE <- merge(x=p207_seeds, y=p207_amos,
                  by="especie", all.x =T) 


p207_SDE$id <- c("A_gal207", "M_man207", "R_car207", "T_cor207", "T_leu207")


p207_SDE$s_amos <- (p207_SDE$seeds)/(p207_SDE$amos)

p207_SDE$s_total <- (p207_SDE$t_seeds)/(p207_SDE$amos)

p207_SDE$parcela <- "207"


SDE_sp_plots <- p20_SDE %>%
  bind_rows(p22_SDE) %>%
  bind_rows(p56_SDE) %>%
              bind_rows(p64_SDE) %>%
                          bind_rows(p84_SDE) %>%
                                      bind_rows(p87_SDE) %>%
                                                  bind_rows(p88_SDE) %>%
                                                  bind_rows(p90_SDE) %>%
                                                  bind_rows(p206_SDE) %>%
                                                  bind_rows(p207_SDE)

SDE_sp_plots$SDE_amos <- SDE_sp_plots$s_amos*SDE_sp_plots$taxcap

SDE_sp_plots$SDE_seeds <- SDE_sp_plots$s_total*SDE_sp_plots$taxcap


SDE_master <- merge(x=SDE_sp_plots, y=plots_summary,
                  by="parcela", all.x =T) 


##10.1 SDE PER SPECIES PER PLOT CONNECTIVITY ------

#connectivity, per species SDE, freq of seeds in sample 

box_c_SDE <- ggplot(SDE_master, aes(x= reorder(parcela, p.cover1),
                                  y=SDE_amos)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per species (freq seeds in sample)", 
       x = "plot (ordered by connectivity)") + 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

box_c_SDE

#connectivity, per species SDE, average no. seeds in sample

box_c_SDE_seeds <- ggplot(SDE_master, aes(x=reorder(parcela, p.cover1), 
                                    y=SDE_seeds)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per spp (seeds/sample)", 
       x = "plot (ordered by connectivity)") + 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

box_c_SDE_seeds

##10.2 SDE PER SPECIES PER PLOT AGE------------

#age, per species SDE, freq of seeds in sample 

box_age_SDE <- ggplot(SDE_master, aes(x=reorder(parcela, p.new_age), 
                                    y=SDE_amos)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per species (freq seeds in sample)", 
       x = "plot (ordered by age)") + 
  theme(axis.text.x = element_text(face = "bold",
                                   colour = "black", size = 10), 
        axis.text.y = element_text(face = "bold", size = 10, 
                                   colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        axis.title.y = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        strip.text = element_text(size = 12, face = "bold"))

box_age_SDE


#age, per species SDE, average no. seeds in sample

box_age_SDE_seeds <- ggplot(SDE_master, aes(x=reorder(parcela, p.new_age), 
                                      y=SDE_seeds)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per spp (seeds/sample)", 
       x = "plot (ordered by age)") + 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

box_age_SDE_seeds

##10.3 SDE PER SPECIES PER PLOT DIVERSITY-------

#div, per species SDE, freq of seeds in sample 

box_div_SDE <- ggplot(SDE_master, aes(x=reorder(parcela, p.div), 
                                    y=SDE_amos)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per species (freq seeds in sample)", 
       x = "plot (ordered by zoo tree spp richness)") + 
  theme(axis.text.x = element_text(face = "bold",
                                   colour = "black", size = 10), 
        axis.text.y = element_text(face = "bold", size = 10, 
                                   colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        axis.title.y = element_text(face = "bold", size = 12, 
                                    colour = "black"), 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        strip.text = element_text(size = 12, face = "bold"))

box_div_SDE

#div, per species SDE, average no. seeds in sample

box_div_SDE_seeds <- ggplot(SDE_master, aes(x=reorder(parcela, p.div), 
                                      y=SDE_seeds)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per spp (seeds/sample)", 
       x = "plot (ordered by zoo tree spp richness)") + 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 10), 
         axis.text.y = element_text(face = "bold", size = 10, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 12, 
                                     colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))

box_div_SDE_seeds

##10.4 SDE PER SPECIES PER PLOT ABUNDANCE-------

#abun, per species SDE, freq of seeds in sample 

box_abun_SDE <- ggplot(SDE_master, aes(x=reorder(parcela, p.abun), 
                                     y=SDE_amos)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per species (freq seeds in sample)", 
       x = "plot (ordered by zoo tree abundance)") + 
  theme(axis.text.x = element_text(face = "bold",
                                   colour = "black", size = 10), 
        axis.text.y = element_text(face = "bold", size = 10, 
                                   colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        axis.title.y = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        strip.text = element_text(size = 12, face = "bold"))

box_abun_SDE

#div, per species SDE, average no. seeds in sample

box_abun_SDE_seeds <- ggplot(SDE_master, aes(x=reorder(parcela, p.abun), 
                                       y=SDE_seeds)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.8, width = 0.2) +
  #scale_y_log10() +
  labs(y = "SDE per spp (seeds/sample)", 
       x = "plot (ordered by zoo tree abundance)") + 
  theme(axis.text.x = element_text(face = "bold",
                                   colour = "black", size = 10), 
        axis.text.y = element_text(face = "bold", size = 10, 
                                   colour = "black"), 
        axis.title.x = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        axis.title.y = element_text(face = "bold", size = 12, 
                                    colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        strip.text = element_text(size = 9, face = "bold"))

box_abun_SDE_seeds


##10.5 COMBINE THE PLOTS SDE PER SPECIES PER PLOT--------
graphics.off()

SDE.spp.sample <- ggarrange(box_c_SDE, box_age_SDE, box_div_SDE, box_abun_SDE,
                         labels = c("A", "B", "C", "D"), label.y = 1.02,
                         ncol = 2, nrow = 2)
SDE.spp.sample

graphics.off()

SDE.spp.seeds <- ggarrange(box_c_SDE_seeds, box_age_SDE_seeds, 
                           box_div_SDE_seeds, box_abun_SDE_seeds,
                            labels = c("A", "B", "C", "D"), label.y = 1.02,
                            ncol = 2, nrow = 2)
SDE.spp.seeds

#11 BIRD SPECIES TRAITS------
aov_spp <- aov(SDE_sample ~ especie, cap.seed)

summary(aov_spp)

##11.1 BODY SIZE -------

#calculate SDE using frequency that seeds are found in sample
cap.seed$SDE_sample <- cap.seed$freq.seed*cap.seed$taxa_cap

#separate body mass and spp names from elton traits database
bodymass <- elton.traits %>%
  select(8, 36)

colnames(bodymass)[1] ="especie"
colnames(bodymass)[2] ="bm"


SDE_bodymass_sample <- merge(x=cap.seed, y=bodymass,
                    by="especie", all.x =T) 

SDE_bm_sample <- ggplot(SDE_bodymass_sample,
                        aes(y = log.SDE,
                        x = bm)) +
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

SDE_bm_sample

model_body_mass <- lm(log.SDE ~ bm, 
                      data = SDE_bodymass_sample)

summary(model_body_mass)

shapiro.test(SDE_bodymass_sample$SDE_sample)
#not normal

#transform

SDE_bodymass_sample$log.SDE <- log10(SDE_bodymass_sample$SDE_sample)

shapiro.test(SDE_bodymass_sample$log.SDE)

# SDE_bm_sample2 <- ggscatter(SDE_bodymass_sample, 
#                             x = "bm", y = "log.SDE",
#                         add = "none", conf.int = T,
#                         cor.coef = T, cor.method = "pearson",
#                         xlab = "body mass (g)",
#                         ylab = "log(SDE) (freq of seeds in sample)")+
#   geom_point(size =2, alpha = 1) +
#   theme (panel.border = element_rect(fill = NA, colour = "black"))
# 
# SDE_bm_sample2


#calculate SDE using frequency that seeds are found in sample
frug.amostras.seeds$SDE_seeds <- 
  frug.amostras.seeds$avg_seeds*frug.amostras.seeds$taxa

SDE_bodymass_seeds <- merge(x=frug.amostras.seeds, y=bodymass,
                             by="especie", all.x =T) 

shapiro.test(SDE_bodymass_seeds$SDE_seeds)

#not normal at alllll

SDE_bodymass_seeds$log.SDE <- log10(SDE_bodymass_seeds$SDE_seeds)

shapiro.test(SDE_bodymass_seeds$log.SDE)

SDE_bm_seeds <- ggscatter(SDE_bodymass_seeds, 
                            x = "bm", y = "log.SDE",
                            add = "none", conf.int = T,
                            cor.coef = T, cor.method = "pearson",
                            xlab = "body mass (g)",
                            ylab = "SDE (seeds/sample)")+
  geom_point(size =2, alpha = 1) +
  theme (panel.border = element_rect(fill = NA, colour = "black"))

SDE_bm_seeds

##11.2 % DIET FRUG

dietfruit <- elton.traits %>%
  select(8, 16)

colnames(dietfruit)[1] ="especie"
colnames(dietfruit)[2] ="diet_f"


SDE_diet_sample <- merge(x=cap.seed, y=dietfruit,
                             by="especie", all.x =T) 

shapiro.test(SDE_diet_sample$SDE_sample)

#not normal?

SDE_diet_sample$log.SDE <- log10(SDE_diet_sample$SDE_sample)

shapiro.test(SDE_diet_sample$log.SDE)

#now it's normal?
SDE_diet_samp <- ggplot(SDE_diet_sample,
                        aes(y = log.SDE,
                            x = diet_f)) +
  geom_smooth(method = "lm", alpha =0.51, colour = "blue") +
  geom_point(size =2, alpha = 1) +
  labs(y = "log(Species-level Seed Dispersal Effectiveness)",
       x = "Percent Diet Frugivory")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 10),
         axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
         axis.title.x = element_text(face = "bold", size = 12, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 10, 
                                     colour = "black"), 
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

SDE_diet_samp


SDE.spp.2 <- ggarrange(SDE_bm_sample, SDE_diet_samp,
                       labels = c("A", "B"), label.y = 1.02,
                       ncol = 2, nrow = 1)

SDE.spp.2


ggsave(
  filename = here::here("SDE_spp.png"),
  plot = SDE.spp.2,
  width = 30, 
  height = 10, 
  units = "cm", 
  dpi = 720)

model_diet <- lm(log.SDE ~ diet_f, 
                      data = SDE_diet_sample)

summary(model_diet)

# % diet frug and frequency of seeds in droppings

shapiro.test(SDE_diet_sample$freq.seed)

SDE_diet_sample$log.freq <- log10(SDE_diet_sample$freq.seed)

shapiro.test(SDE_diet_sample$log.freq)

SDE_diet_freq <- ggplot(SDE_diet_sample,
                        aes(y = log.freq,
                            x = diet_f)) +
  geom_smooth(method = "lm", alpha =0.51, colour = "blue") +
  geom_point(size =2, alpha = 1) +
  labs(y = "Log(Seed Dispersal Frequency)",
       x = "Percent Diet Frugivory")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 10),
         axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
         axis.title= element_text(face = "bold", size = 12, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

SDE_diet_freq

model_diet_freq <- lm(log.freq ~ diet_f, 
                 data = SDE_diet_sample)

summary(model_diet_freq)



# % diet frug and capture rate

shapiro.test(SDE_diet_sample$taxa_cap)

SDE_diet_sample$log.cap <- log10(SDE_diet_sample$taxa_cap)

shapiro.test(SDE_diet_sample$log.cap)

SDE_diet_cap <- ggplot(SDE_diet_sample,
                        aes(y = log.cap,
                            x = diet_f)) +
  #geom_smooth(method = "lm", alpha =0.51, colour = "blue") +
  geom_point(size =2, alpha = 1) +
  labs(y = "Log(Capture Rate)",
       x = "Percent Diet Frugivory")+
  theme( axis.text.x = element_text(face = "bold",colour = "black", size = 10),
         axis.text.y = element_text(face = "bold", size = 10, colour = "black"),
         axis.title= element_text(face = "bold", size = 12, colour = "black"),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA, colour = "black"),
         strip.text = element_text(size = 9, face = "bold"))

SDE_diet_cap

SDE.spp.seeds <- ggarrange(box_c_SDE_seeds, box_age_SDE_seeds, 
                           box_div_SDE_seeds, box_abun_SDE_seeds,
                           labels = c("A", "B", "C", "D"), label.y = 1.02,
                           ncol = 2, nrow = 2)
SDE.spp.seeds

model_diet_cap <- lm(log.cap ~ diet_f, 
                      data = SDE_diet_sample)

summary(model_diet_cap)


#seeds

SDE_diet_seed <- merge(x=frug.amostras.seeds, y=dietfruit,
                         by="especie", all.x =T) 


shapiro.test(SDE_diet_seed$SDE_seeds)

#not normal?

SDE_diet_seed$log.SDE <- log10(SDE_diet_seed$SDE_seeds)

shapiro.test(SDE_diet_seed$log.SDE)

SDE_df_seed <- ggscatter(SDE_diet_seed, 
                             x = "df", y = "log.SDE",
                             add = "none", conf.int = T,
                             cor.coef = T, cor.method = "pearson",
                             xlab = "% diet frugivory",
                             ylab = "log(SDE) (seeds/sample)")+
  geom_point(size =2, alpha = 1) +
  theme (panel.border = element_rect(fill = NA, colour = "black"))

SDE_df_seed


# Number of seeds found in feces vs. frequency of seeds in feces -------

seeds.v.freq <- ggscatter(plot.data, 
                            x = "s_parcela_f", y = "avg_seeds",
                            add = "none", conf.int = T,
                            cor.coef = T, cor.method = "pearson",
                            xlab = "freq of seeds in feces",
                            ylab = "average number of seeds in feces")+
  geom_point(size =2, alpha = 1) +
  theme (panel.border = element_rect(fill = NA, colour = "black"))

seeds.v.freq


seeds.v.freq.spp <- ggscatter(SDE_master, 
                          x = "s_amos", y = "s_total",
                          add = "none", conf.int = T,
                          cor.coef = T, cor.method = "pearson",
                          xlab = "freq of seeds in feces",
                          ylab = "average number of seeds in feces")+
  geom_point(size =2, alpha = 1) +
  theme (panel.border = element_rect(fill = NA, colour = "black"))

seeds.v.freq.spp




##SPP ACCUMULATION CURVE------

#-------


spp_acc <- master %>%
  group_by(dia, especie) %>% 
  summarise(amostras = n_distinct(amostra)) %>%
  pivot_wider(names_from = especie, 
              values_from = amostras) %>% 
  column_to_rownames(var = "dia") 

spp_acc[is.na(spp_acc)] <- 0

allm <- specaccum(spp_acc,method="rarefaction")

plot(allm)


mat_frug <- master %>%
  filter(frug == "1") #%>% #only frug
  # group_by(parcela) %>% 
  # arrange(parcela, .by_group=T)


spp_acc_frug <- mat_frug %>%
  group_by(dia, especie) %>% 
  summarise(amostras = n_distinct(amostra))# %>%
  # pivot_wider(names_from = especie, 
  #             values_from = amostras) %>% 
  # column_to_rownames(var = "dia") 

colnames(spp_acc_frug)[1] ="dia"

missing_numbers <- setdiff(1:121, unique(spp_acc_frug$dia))

no_frug_caps <- data.frame( dia = missing_numbers, especie = "no_f_cap", 
                            amostras = 0) 

spp_acc_frug_gaps <- spp_acc_frug %>%
  bind_rows(no_frug_caps)

spp_acc_frug_gaps <- spp_acc_frug_gaps %>%
  arrange(dia)

spp_acc_frug_gaps <- spp_acc_frug_gaps %>%
 pivot_wider(names_from = especie, 
             values_from = amostras) %>% 
 column_to_rownames(var = "dia")

spp_acc_frug_gaps[is.na(spp_acc_frug_gaps)] <- 0

graphics.off()


#sa_a <- specaccum(spp_acc, method="rarefaction")

sa_f <- specaccum(spp_acc_frug_gaps, method="rarefaction")

cummspp <- plot(allm, ci.type="poly", col="#5B7C91", lwd=3, ci.lty=0, 
                ci.col=alpha("#5B7C91", 0.5),
                xlab = "Sampling Day", ylab = "Cummulative No. Species",
                cex.lab = 1.4, cex.axis =1.4) +
        lines(sa_f, ci.type="poly", col="#F19E14", lwd=3, ci.lty=0,
        ci.col= alpha("#F19E14", 0.5))
legend(84, 16, legend=c("All species", "Fruit-eating species"),
       col=c("#5B7C91", "#F19E14"), lty = 1, lwd=3, bty = "n", cex=1.4,
       text.font=2)

### dropping collection and seeds by SPECIES 

caps.frug2 <- caps.frug %>%
  filter(frug == 1)

caps.frug3 <- caps.frug2 %>%
    filter(sem > 0)

caps.frug4 <- caps.frug3 %>%
  select(1, 6)

caps.frug5 <- caps.frug4 %>%
  pivot_wider(names_from = especie, 
              values_from = sem) #%>%
  #column_to_rownames(var = "dia")



caps.frug4$fraction = caps.frug4$sem / sum(caps.frug4$sem)

# Compute the cumulative percentages (top of each rectangle)
caps.frug4$ymax = cumsum(caps.frug4$fraction)

# Compute the bottom of each rectangle
caps.frug4$ymin = c(0, head(caps.frug4$ymax, n=-1))

#library(RColorBrewer)

#brewer.pal(n = 17, name = "RdYlGn")

#cols <-colorRampPalette(c("red", "green"))(17)

# Make the plot
p <- ggplot(caps.frug4, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,
                       fill = reorder (especie, -sem))) +
  geom_rect(color = "black") +
  #scale_fill_manual(values= cols) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()

p

p + theme(legend.position = "none")


plot.data2 <- plot.data %>%
  select(10, 11, 5)

plot.data2$plot <- c(1:10)

plot.data2$nf_sample <- (plot.data$n_amostras - plot.data$frug_c_amostra)

plot.data2$f_n_seeds <- (plot.data$frug_c_amostra - plot.data$a_c_seeds)

plot.data2 <- plot.data2 %>%
  select(4, 5, 6, 3)

plot.data3 <- t(plot.data2[,-1])

colnames(plot.data3) <- plot.data2$plot


barplot(plot.data3, xlab = "Plot",
        ylab = "No. Samples",
        col = c("#5B7C91", "#F19E14", "darkorange2"),
        border = NA, ylim = c(0, 70),
        cex.lab = 1.6, cex.axis =1.4, cex.names = 1.5)

legend(7.8, 70,
       xpd = TRUE,
       legend = c("non-fruit eating", "fruit-eating", "fruit-eating with seeds"),
       fill = c("#5B7C91", "#F19E14", "darkorange2"),
       box.lty=0,
       border = NA,
       cex=1.4,
       text.font=2
)



#seeds plots

plot.data4 <- data.frame(plot = c("01", "02", "03", "04", "05", "06", "07",
                                  "08", "09", "10"), seeds = plot.data$t_seeds) 

plot.data4$plot <- as.character(plot.data4$plot)

ggplot(plot.data4, aes(x=plot, y =seeds))+ 
  geom_bar(stat = "identity", fill ="#607729")+
  labs(y = "No. Seeds", 
       x = "Plot") + 
  theme( axis.text.x = element_text(face = "bold",
                                    colour = "black", size = 12), 
         axis.text.y = element_text(face = "bold", size = 12, 
                                    colour = "black"), 
         axis.title.x = element_text(face = "bold", size = 18, 
                                     colour = "black"),
         axis.title.y = element_text(face = "bold", size = 18, 
                                     colour = "black"), 
         panel.background = element_blank(), 
         panel.border = element_rect(fill = NA, colour = "black"), 
         strip.text = element_text(size = 9, face = "bold"))


# spp curve for plants in DNA
dna_spp <- readr::read_csv(here::here("dados","col_curve_dna.csv"))

dna_colcurve <- dna_spp %>%
  select(1, 3)


dna_colcurve2 <- unique(dna_colcurve)

dna_colcurve3 <- dna_colcurve2 %>%
  dplyr::group_by(genus, amostra) %>%
   dplyr::summarise(n = n_distinct(amostra)) 


dna_colcurve4 <- dna_colcurve3 %>%
  pivot_wider(names_from = genus, 
              values_from = n) %>%
column_to_rownames(var = "amostra")

dna_colcurve4[is.na(dna_colcurve4)] <- 0


sa_dna <- specaccum(dna_colcurve4, method="rarefaction")

cummspp <- plot(sa_dna, ci.type="poly", col="#6A0207", lwd=3, ci.lty=0, 
                ci.col=alpha("#6A0207", 0.5),
                xlab = "No. Individuals Sequenced", 
                ylab = "Cummulative No. Plant OTUs Detected",
                cex.lab = 1.8, cex.axis =1.8) #+
  # lines(sa_f, ci.type="poly", col="#F19E14", lwd=3, ci.lty=0,
  #       ci.col= alpha("#F19E14", 0.5))
# legend(84, 16, legend=c("All species", "Fruit-eating species"),
#        col=c("#5B7C91", "#F19E14"), lty = 1, lwd=3, bty = "n", cex=1.4,
#        text.font=2)


##dropping collection across all plots------
sum_drop <- plot.data %>%
  select(10, 11, 5)

sum_drop$frug <- sum_drop$frug_c_amostra - sum_drop$a_c_seeds

sum_drop$n_frug <- sum_drop$n_amostras - sum_drop$frug_c_amostra

sum_drop3 <- data.frame(sum(sum_drop$n_frug), sum(sum_drop$frug),
                        sum(sum_drop$a_c_seeds))

colnames(sum_drop3)[c(1, 2, 3)] = c("n_frug","frug", "seeds")

sum_drop4 <- t(sum_drop3)

colnames(sum_drop4)[1] = "drop"

sum_drop5 <- as.data.frame(sum_drop4)

sum_drop5$fraction = sum_drop5$drop / sum(sum_drop5$drop)

# Compute the cumulative percentages (top of each rectangle)
sum_drop5$ymax = cumsum(sum_drop5$fraction)

# Compute the bottom of each rectangle
sum_drop5$ymin = c(0, head(sum_drop5$ymax, n=-1))

#library(RColorBrewer)

#brewer.pal(n = 17, name = "RdYlGn")

#cols <-colorRampPalette(c("red", "green"))(17)

# Make the plot
p <- ggplot(sum_drop5, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,
                            fill =(drop))) +
  geom_rect(color = "black") +
  #scale_fill_manual(values= cols) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()

p

p + theme(legend.position = "none")


##SEEEDSSS-------
morph_plot <- seed.sum %>%
  group_by(parcela) %>%
  summarise(morf = n_distinct(morpho_spp))

morph_spp <- seed.sum %>%
  group_by(especie) %>%
  summarise(morf = n_distinct(morpho_spp))

morph_ind <- seed.sum %>%
  group_by(amostra) %>%
  summarise(morf = n_distinct(morpho_spp))

seed.sum <- distinct(seed.sum, amostra, 
                     .keep_all = T)

seed.sum_spp <- seed.sum %>%
  group_by(especie) %>%
  summarise(seed = sum(t_seeds))

mean(seed.sum_spp$seed)

median(seed.sum_spp$seed)

sd(seed.sum_spp$seed)

#morhpospp

morphoseed <- merge(x=seeds, y=morpho,
                    by="morpho_spp", all.x =T) 


morphoseed[is.na(morphoseed)] <- "unknown"

morph.sum <- morphoseed %>%
  group_by(family, genus) %>%
  summarise(sum = n_distinct(morpho_spp), seeds = sum(no_seeds))

write.csv(morph.sum, here::here( "resultados",
                                 "morph.sum.csv"))


seed.sum.fam <- seed.sum %>%                             
  group_by(s_family) %>%
  summarise(t_seeds = sum(no_seeds))

median(seed.sum.ind$t_seeds)

sd(seed.sum.ind$t_seeds)