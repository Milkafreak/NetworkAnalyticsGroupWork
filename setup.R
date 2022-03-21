# Load data
dt.trump <- fread("TrumpWorld-Data.csv")



# Change column names 
colnames(dt.trump) <- c("Entity_A_Type", "Entity_A", "Entity_B_Type", "Entity_B", "Connection", "Sources")


dt.trump <- dt.trump %>% mutate(entity_type_connection = paste(Entity_A_Type,Entity_B_Type))

save(dt.trump, file='trump.RData')

