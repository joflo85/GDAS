
# Installeer de benodigde R packages
# tidyverse voor importeren van data en data transformatie 
# lubridate voor datum functies
# ggplot2 voor data visualisatie
# tinytex voor het renderen van "Bike Share project - analysis" in PDF output

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
tinytex::install_tinytex()

library(tidyverse)  
library(lubridate)
library(ggplot2) 
library(tinytex)


#Creëer een werkmap op je computer. LET OP! Onderstaande directory is mijn eigen working directory! Voor het reproduceren dient deze te worden aangepast naar een eigen working directory op jouw eigen PC of laptop! 
setwd("/Users/joramhofm/Desktop/GDAPC/Archive/Cyclistic Bike Share Data Analysis")

# 1. Verzamelen van de data
# upload de CSV bestanden en converteer ieder bestand naar een R data.frame
Q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
Q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
Q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
Q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# Bepaal of de kolom namen van de vier data.frames overeenkomen met elkaar
colnames(Q2_2019)
colnames(Q3_2019)
colnames(Q4_2019)
colnames(Q1_2020)

# Hernoem de kolom namen van de 2019 data.frames, zodat deze overeenkomen met die van Q1_2020.
# Hernoem de naam van kolom "usertype" naar "type_fietser" voor alle 4 de data.frames.
(Q4_2019 <- rename(Q4_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,  
                   ended_at = end_time,  
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   type_fietser = usertype))

(Q3_2019 <- rename(Q3_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,  
                   ended_at = end_time,  
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   type_fietser = usertype))

(Q2_2019 <- rename(Q2_2019,
                   ride_id = "01 - Rental Details Rental ID",
                   rideable_type = "01 - Rental Details Bike ID",
                   started_at = "01 - Rental Details Local Start Time",  
                   ended_at = "01 - Rental Details Local End Time",  
                   start_station_name = "03 - Rental Start Station Name",
                   start_station_id = "03 - Rental Start Station ID",
                   end_station_name = "02 - Rental End Station Name",
                   end_station_id = "02 - Rental End Station ID",
                   type_fietser = "User Type"))

(Q1_2020 <- rename(Q1_2020, 
                   type_fietser = member_casual))

# Inspecteer de data van de vier data.frames op gelijkheid in datatype
str(Q1_2020)
str(Q4_2019)
str(Q3_2019)
str(Q2_2019)

# De kolommen 'ride_id' en rideable_type van de 2019 data.frames bevatten nummerieke waarden.
# Dit komt niet overeen met het data type in het data.frame Q1_2020, daar zijn de waarden in  # de twee kolommen namelijk van het data type 'character'. 
# Converteer de waarden in de 2019 data.frames naar data type character, zodat deze matchen   # met de waarden van het Q1_2020 data.frame.
Q4_2019 <-  mutate(Q4_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type))
Q3_2019 <-  mutate(Q3_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type))
Q2_2019 <-  mutate(Q2_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type))   

# Controleer of de data types van alle data.frames nu matchen
str(Q1_2020)
str(Q4_2019)
str(Q3_2019)
str(Q2_2019)

# De data types van alle data.frames matchen.

# Voeg alle vier data.frames samen in één data.frame
all_trips <- bind_rows(Q2_2019, Q3_2019, Q4_2019, Q1_2020)
str(all_trips)

# Verwijder alle data die niet gebruikt gaat worden voor dit project
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, 
            "01 - Rental Details Duration In Seconds Uncapped", 
            "05 - Member Details Member Birthday Year", 
            "Member Gender", 
            "tripduration"))

# Inspecteer de nieuwe data.frame om te bepalen of er nog meer data cleaning uitgevoerd dient # te worden
colnames(all_trips)  #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. 
tail(all_trips) #See the last 6 rows of data frame.  
str(all_trips) #See list of columns and data types (numeric, character, etc).
summary(all_trips)  #Statistical summary of data. Mainly for numerics.

#Hieronder de gevonden issues na inspectie van de data:
# 1. Kolom member_casual heeft vier labels. Twee namen voor member: "member" en "Subscriber"
#    en twee namen voor casual: "casual" en "Customer".
# 2. Het huidige data.frame geeft geen mogelijkheid om "members" en 
#    "casuals" data te aggregeren in de context van datum en tijd (dag, maand en jaar).
# 3. Data.frame Q1_2020 bevat geen "tripduration" kolom. Deze data is nodig voor het gehele 
#    all_trips data.frame om de ritduur te kunnen aggregeren..
# 4. Het all_trips dataframe bevat een paar honderd vermeldingen waarbij of fietsen uit de 
#    docks werden gehaald voor quality control of de ritlengte negatief was.

# Hieronder de oplossingen voor de gevonden issues:
# 1. Breng eerst het aantal observaties per label in kaart
table(all_trips$type_fietser)
# Breng het aantal labels terug van vier naar 2, waarbij alleen nog de twee labels
# "member"en "casual" overblijven. 
all_trips <-  all_trips %>%
  mutate(type_fietser = recode(type_fietser,
                               "Subscriber" = "member",
                               "Customer" = "casual"))

# Check of alle observaties die gewijzigd moesten worden nu op juist gewijzigd zijn. Dit is het geval.
table(all_trips$type_fietser)

# 2. Voeg kolommen toe met de datum, maand, dag en jaar van elke rit.
#    Hierdoor kunnen we ritgegevens voor elke maand, dag of jaar aggregeren.
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$hour_of_day <- format(as.POSIXct(all_trips$started_at), "%H")

# 3. Voeg een kolom genaamd "ride_length" toe met een calculatie van de ritduur aan het all_trips data.frame. 
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspecteer de structuur van de kolommen.
str(all_trips)

# Converteer "ride_length" van Factor naar numeriek, zodat er berekeningen op de ride_length  # observaties uitgevoerd kunnen worden. Controleer daarna of de conversie is gelukt.
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Creëer een nieuwe versie van het dataframe genaamd "v2", aangezien gegevens worden verwijderd.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Het cleanen en transformeren van de data voor dit project is nu afgerond. 
# Creëer een nieuw CSV bestand voor de data analyse en data visualisatie 
write_csv(all_trips_v2, "Cyclistic Data")