#### import libraries ####
# data wrangling

library(feather)
library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
library(usethis)

#### Importing and cleaning the dataset #####

dgd <- read_csv("DGD.csv", locale = readr::locale(encoding = "latin1"))

# Only choose the column with relevant topics
dgd_fil <- dgd %>% filter(str_detect(str_to_lower(as.character(SECTOR)),'water|environment|hydro-electric|agricultur|forestry|fish') & `first-year-exp` > 2007)

# Remove the columns with too many missing values 

dgd_min <- dgd_fil %>% select(-c(BUDGET_CODE, SERVICE_CODE, OWNER, BUDGETHOLDER_CODE,CONTRACTOR_PROJ_CODE,
                                 CONTRACTOR_ALT, PLANNED_START_DT, PLANNED_END_DT, MINISTER_NOTIFIED_DT,MINISTER_APPROVED_DT,
                                 EFFECTIVE_START_DT, EFFECTIVE_END_DT, ADVICE_FI_DT, FORMAL_CLOSURE_DT, PHASE, REGION,
                                 TAGS, SDG_SDTS, REGI_ID
                                 # , BACKGROUND..IN., BACKGROUND..CO., GENERAL.PURPOSE..CO., SPECIFIC.PURPOSE..CO.,
                                 # SPECIFIC.RESULTS..CO., RESULTS_OUTPUT..CO.
                                 ))

# this is to check again the columns with high number of missing values

dgd_min <- dgd_min[,-c(31:58, 67:82)] 

#### Lat and long #####

lat_long <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE))
lat_long$Country <- rownames(lat_long)
rownames(lat_long) <- c(1:nrow(lat_long))

# add Palestine 

lat_long <- bind_rows(lat_long, data.frame(x = 31.9522, y = 35.2332, Country = 'Palestine'))

# Country
dgd_min$COUNTRY <- str_to_title(as.character(dgd_min$COUNTRY))

# Check to see if some countries on dgd list but not lat_long list
setdiff(levels(as.factor(dgd_min$COUNTRY)), lat_long$Country)
old <- c("Central African Rep","China \\(People's Republic\\)","Congo \\(Democratic Rep.\\)",
         "Congo \\(Rep.\\)","Guinea Republic","Honduras Rep","Kazakstan","Madagascar Dr",
         "Mali Rep","Myanmar \\(Burma\\)","Niger Rep","Somali Rep", 
         "Tanzania","Togo,Rep")


new <- c("Central African Republic","China","Democratic Republic of the Congo",
         "Republic of the Congo","Guinea","Honduras","Kazakhstan","Madagascar",
         "Mali", "Myanmar","Niger", "Somalia",
         "United Republic of Tanzania", "Togo")


for (i in 1:nrow(dgd_min)){
        for (j in 1:length(old)){
                dgd_min$COUNTRY[i] <- str_replace_all(dgd_min$COUNTRY[i], old[j], new[j])
        }
}

# match lat long 

dgd_min$COUNTRY[is.na(dgd_min$COUNTRY)] <- "Belgium"
dgd_min$lat <- NA
dgd_min$long <- NA

lat_long <- lat_long %>% filter(Country %in% intersect(levels(as.factor(dgd_min$COUNTRY)), lat_long$Country))

# Fill the other with 

for (i in 1:nrow(dgd_min)){
        for (j in 1:nrow(lat_long)){
                if (dgd_min$COUNTRY[i] == lat_long$Country[j]){
                        dgd_min$lat[i] <- lat_long$x[j]
                        dgd_min$long[i] <- lat_long$y[j]
                } 
        }
}

dgd_min$COUNTRY[dgd_min$COUNTRY == "Belgium"] <- "Other"

# Divide into different categories of budget

dgd_min$BUDGET <- NA
dgd_min$BUDGET[dgd_min$TOTAL_BUDGET > 10000000 & !is.na(dgd_min$TOTAL_BUDGET)] <-'Larger than ten million euros'
dgd_min$BUDGET[dgd_min$TOTAL_BUDGET <= 10000000 & !is.na(dgd_min$TOTAL_BUDGET)] <-'Smaller than ten million euros'
dgd_min$BUDGET[dgd_min$TOTAL_BUDGET <= 1000000 & !is.na(dgd_min$TOTAL_BUDGET)] <-'Smaller than one million euros'
dgd_min$BUDGET[dgd_min$TOTAL_BUDGET <= 100000 & !is.na(dgd_min$TOTAL_BUDGET)] <-'Smaller than 100,000 euros'
dgd_min$BUDGET[is.na(dgd_min$TOTAL_BUDGET)] <-'Not Available'
dgd_min$BUDGET <- as.factor(dgd_min$BUDGET)
    
# change a bit

dgd_min$TYPOLOGY <- str_replace_all(dgd_min$TYPOLOGY, "Contributions to  specific-purpose programmes and funds managed by international organisations \\(multilateral, INGO\\)",
                                    "Contributions to specific-purpose programs")
dgd_min$TYPOLOGY <- str_replace_all(dgd_min$TYPOLOGY, "Core support to NGOs, other private bodies, PPPs and research institutes", 
                                    "Core support to NGOs and other organizations")

# Make country and lat-long as individual columns ####

dgd_min <- dgd_min %>% select(c(1,2,5:8, 10:14, 20:23, 26:28, 30:38, 40:44))
dgd_min[,(ncol(dgd_min_v2)+1):(ncol(dgd_min_v2)+nrow(lat_long))] <- NA
colnames(dgd_min)[(ncol(dgd_min_v2)+1):(ncol(dgd_min_v2)+nrow(lat_long))] <- lat_long$Country
colnames(dgd_min)[str_which(colnames(dgd_min), "Belgium")] <- "Other"

for(i in seq_len(nrow(dgd_min))){
        for (j in (ncol(dgd_min_v2)+1):(ncol(dgd_min_v2)+nrow(lat_long))){
                if(dgd_min$COUNTRY[i] == colnames(dgd_min)[j]){
                        dgd_min[i,j] <- dgd_min$COUNTRY[i]
                }
        }
}

# making lat long columns

first_country <- which(colnames(dgd_min) == 'Afghanistan')
last_country <- which(colnames(dgd_min) == 'Palestine')

x_lat <- dgd_min[, first_country:last_country, drop = F]
colnames(x_lat) <- paste("lat",colnames(dgd_min[,first_country:last_country]), sep = "_")
x_long <- dgd_min[, first_country:last_country, drop = F]
colnames(x_long) <- paste("long",colnames(dgd_min[,first_country:last_country]), sep = "_")

for(i in 1:ncol(x_lat)){
        for(j in 1:nrow(x_lat)){
                if(!is.na(x_lat[j,i])){
                        x_lat[j,i] <- lat_long$x[i]
                        x_long[j,i] <- lat_long$y[i]
                }
        }
}

dgd_min <- bind_cols(dgd_min, x_lat, x_long)

# Add year column ####

list_year <- union(levels(as.factor(dgd_min$`first-year-exp`)), levels(as.factor(dgd_min$`last-year-exp`)))
dgd_min[,(ncol(dgd_min)+1):(ncol(dgd_min)+length(list_year))] <- NA
colnames(dgd_min)[(ncol(dgd_min)+1-length(list_year)):(ncol(dgd_min))] <- levels(as.factor(list_year))

for(i in seq_len(nrow(dgd_min))){
    for (j in (ncol(dgd_min)+1-length(list_year)):(ncol(dgd_min))){
        if(between(as.numeric(colnames(dgd_min[,j])),as.numeric(dgd_min$`first-year-exp`[i]), as.numeric(dgd_min$`last-year-exp`[i]))){
            dgd_min[i,j] <- as.numeric(colnames(dgd_min[,j]))
        }
    }
}


# Add Top sector ####

old1 <- c("Agriculture, forestry, fishing \\(31xxx\\)", "Energy \\(23xxx\\)", "Environmental protection \\(41xxx\\)", 
          "Multisector \\(43xxx\\)", "Transport and storage \\(21xxx\\)", "Water and sanitation \\(14xxx\\)")
new1 <- c("Agriculture, forestry, fishing", "Energy", "Environmental protection", 
          "Multisector", "Transport and storage", "Water and sanitation")

for (i in 1:nrow(dgd_min)){
    for (j in 1:length(old1)){
        dgd_min$`TOP SECTOR`[i] <- str_replace_all(dgd_min$`TOP SECTOR`[i], old1[j], new1[j])
    }
}

list_topsector <- levels(as.factor(dgd_min$`TOP SECTOR`))
dgd_min[,(ncol(dgd_min)+1):(ncol(dgd_min)+length(list_topsector))] <- NA
colnames(dgd_min)[(ncol(dgd_min)+1-length(list_topsector)):(ncol(dgd_min))] <- levels(as.factor(list_topsector))

for(i in seq_len(nrow(dgd_min))){
    for (j in (ncol(dgd_min)+1-length(list_topsector)):(ncol(dgd_min))){
        if(dgd_min$`TOP SECTOR`[i] == colnames(dgd_min)[j]){
            dgd_min[i,j] <- dgd_min$`TOP SECTOR`[i]
        }
    }
}

# Add Type of Aid #### 

list_typology <- levels(as.factor(dgd_min$TYPOLOGY))
dgd_min[,(ncol(dgd_min)+1):(ncol(dgd_min)+length(list_typology))] <- NA
colnames(dgd_min)[(ncol(dgd_min)+1-length(list_typology)):(ncol(dgd_min))] <- levels(as.factor(list_typology))

for(i in seq_len(nrow(dgd_min))){
    for (j in (ncol(dgd_min)+1-length(list_typology)):(ncol(dgd_min))){
        if(dgd_min$TYPOLOGY[i] == colnames(dgd_min)[j] & !is.na(dgd_min$TYPOLOGY[i])){
            dgd_min[i,j] <- dgd_min$TYPOLOGY[i]
        }
    }
}

# Add sector #### 

list_sector <- levels(as.factor(dgd_min$SECTOR))
dgd_min[,(ncol(dgd_min)+1):(ncol(dgd_min)+length(list_sector))] <- NA
colnames(dgd_min)[(ncol(dgd_min)+1-length(list_sector)):(ncol(dgd_min))] <- levels(as.factor(list_sector))

for(i in seq_len(nrow(dgd_min))){
    for (j in (ncol(dgd_min)+1-length(list_sector)):(ncol(dgd_min))){
        if(dgd_min$SECTOR[i] == colnames(dgd_min)[j]){
            dgd_min[i,j] <- dgd_min$SECTOR[i]
        }
    }
}

# save files ####

write.csv(dgd_min, "dgd_min.csv", row.names = FALSE)
write_feather(dgd_min, "dgd_min.feather")