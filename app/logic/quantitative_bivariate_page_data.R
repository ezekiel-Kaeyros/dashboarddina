box::use (
  dplyr[mutate, arrange, if_else, first, summarise, summarize,group_by, select, n, rename,inner_join, right_join,left_join, filter, ungroup],
  magrittr[`%>%`],
  stats[aggregate],
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  sf[st_sfc, st_centroid, st_coordinates, st_drop_geometry],
  topicmodels[LDA, posterior],
)
box::use(
  app/logic/import_data,
)


################## Data to display age_Otherdiscrimination ######################
data_age_disc <- data.frame(category_age = rep(import_data$data$age, sapply(import_data$data$otherFormOfDisc, length)),
                            discrimination = import_data$data$otherFormOfDisc)
table_age_disc <- with(data_age_disc, table(category_age, discrimination))

order <- c("Unter 18 Jahre", "18 - 27", "28 - 40", "41 - 65", "Über 65")
table_age_disc <- as.data.frame(table_age_disc) %>%
  mutate(category_age = factor(category_age, levels = order)) %>%
  arrange(category_age)


############## Data to display age_influence ###############
data_age_inf <- data.frame(category_age = rep(import_data$data$age, sapply(import_data$data$manifestationOfDiscrimination, length)),
                           influence = import_data$data$manifestationOfDiscrimination)
table_age_inf <- with(data_age_inf, table(category_age, influence))

order <- c("Unter 18 Jahre", "18 - 27", "28 - 40", "41 - 65", "Über 65")
table_age_inf <- as.data.frame(table_age_inf) %>%
  mutate(category_age = factor(category_age, levels = order)) %>%
  arrange(category_age)


# ########### Data to display gender Other discrimination ############
data_gender_disc <- data.frame(gender = rep(import_data$data$gender, sapply(import_data$data$otherFormOfDisc, length)),
                               discrimination = import_data$data$otherFormOfDisc)
table_gender_disc <- with(data_gender_disc, table(gender, discrimination))
table_gender_disc <- as.data.frame(table_gender_disc)
