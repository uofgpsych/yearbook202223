library(tidyverse)

abstracts <- read_csv("Abstract Submissions.csv") %>%
  mutate(ID = substr(GUID,nchar(GUID)-9,nchar(GUID)-1)) %>%
  select(-GUID)


classlist <- read_csv("class_list.csv")


supervisors <- abstracts %>%
  select(ID, Supervisor) %>%
  separate(Supervisor, into = c("LastName", "SFirstName"), sep = ", ") %>%
  mutate(newcol2 = tolower(substr(LastName, 2, nchar(LastName)))) %>%
  mutate(newcol1 = substr(LastName, 1, 1)) %>%
  mutate(SLastName = paste0(newcol1, newcol2)) %>%
  select(ID, SFirstName, SLastName) %>%
  mutate(SLastName = ifelse(SLastName == "Wilson smith", "Wilson-Smith", SLastName)) %>%
  mutate(SLastName = ifelse(SLastName == "Mcaleer", "McAleer", SLastName)) %>%
  mutate(SLastName = ifelse(SLastName == "Debruine", "DeBruine", SLastName)) %>%
  mutate(SLastName = ifelse(SLastName == "Woods", "Cleland Woods", SLastName))

studname <- classlist %>%
  select(ID, Name) %>%
  separate(Name, into = c("LastName", "FirstName"), sep = ",") %>%
  mutate(ID = as.character(ID)) %>%
  select(ID, FirstName, LastName) %>%
  mutate(initial = substr(LastName, 1, 1))

alldat <- full_join(abstracts, studname, "ID") %>%
  full_join(supervisors, "ID") %>%
  select(-Tags, -Preferences, -Name, -Supervisor) %>%
  separate(Theme, into = c("Theme1","Theme2","Theme3", "Theme4"), sep = "##") %>%
  mutate(Abstract = gsub("<.*?>", "", Abstract)) %>%
  mutate(Abstract = gsub("\r\n", " ", Abstract)) %>%
  mutate(Included = "Yes") %>%
  select(ID, FirstName, LastName, initial, Title, Abstract, 
         Theme1, Theme2, Theme3, Theme4, SFirstName, SLastName, Included) %>%
  arrange(LastName) %>%
  unique() %>%
  mutate(rows = row_number()) %>%
  filter(rows != 89) %>%
  select(-ID)

write.csv(alldat, "abstracts_amended.csv", row.names = FALSE)
