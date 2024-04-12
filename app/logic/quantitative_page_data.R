box::use (
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, filter, ],
  magrittr[`%>%`],
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, right_join,left_join, filter],
  stats[aggregate]
          )
box::use(
  app/logic/import_data,
)


############ Data to display age chart #################
data_age <- as.data.frame(table(import_data$data$age))%>%
  mutate(Var1 = factor(Var1, levels = c("Unter 18 Jahre", "18 - 27", "28 - 40", "41 - 65", "Über 65")))

data_age <- data_age %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))



############ Data to display gender chart #################
data_gen <- as.data.frame(table(import_data$data$gender))
data_gen <- data_gen %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
         pct1 = paste0(percentage, "%"))


########### Data to display temporal distance ###########
data_temp <- import_data$data %>%
  select(valueDate, endDate, createdAt) %>%
  mutate(
    valueDate = as.Date(valueDate, format = "%d.%m.%Y"),
    endDate = as.Date(endDate, format = "%d.%m.%Y"),
    createdAt = as.Date(createdAt, format = "%d.%m.%Y")
  ) %>%
  mutate(
    temporal_distance = if_else(is.na(endDate), as.numeric(difftime(createdAt, valueDate, units = "days")),
                                as.numeric(difftime(createdAt, endDate, units = "days")))
  )


data_months <- data_temp %>%
  mutate(
    temporal_class = cut(temporal_distance,breaks = c(-Inf,90,180,270,Inf),
                         labels = c("(0-3)Monate","(4-6)Monate","(7-9)Monate","(10+)Monate"))
  ) %>%
  select(temporal_class) %>%
  group_by(temporal_class) %>%
  summarise(total = n()) %>%
  mutate(percentage=round(100*(total/sum(total)),2),
         pct1=paste0(percentage,"%"))


############## Data to display location ################
data_onreal <- as.data.frame(table(import_data$data$locationOnline))
data_onreal <- data_onreal %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))


############# data to display person affected ##########
data_personaf <- as.data.frame(table(import_data$data$identity))
data_personaf <- data_personaf %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))



############## data to display previous measure ################
previous_measures <-as.data.frame(table(import_data$data$haveYouReported))
previous_measures <- previous_measures %>%
  dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                pct1 = paste0(percentage, "%"))




#################### data to display map of person affected #############
data_location <- as.data.frame(table(import_data$data$location)) %>%
  rename(location = Var1, Value = Freq)

# Filtrer et sélectionner les données uniquement pour l'État de Nordrhein-Westfalen
data1<- import_data$Allemagne %>%
  filter(NAME_1 == "Nordrhein-Westfalen") %>%
  select(NAME_1, Province, location) %>%
  left_join(data_location, by = "location") %>%
  mutate(Value = if_else(is.na(Value), 0, Value))

# Agréger les données par province et calculer la somme des valeurs
sum_by_province <- aggregate(data1["Value"], by = list(Province = data1$Province), FUN = sum)
interval1 <- c(0, 20, 40, Inf)
categories1 <- c(paste("unter",20), paste(20,"-",40, sep=""),paste("Über", 40))
sum_by_province$cat <- cut(sum_by_province$Value, breaks = interval1, labels = categories1, right = FALSE)



