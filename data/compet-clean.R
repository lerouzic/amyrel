fx.compet <- here::here("compet.xlsx")
tx.compet <- here::here("compet.txt")

cols <- c(
    'Génération' = "Generation",
    'croisement' = "Cross", 
    'milieu'     = "Medium",
    'réplicat'   = "Replicate",
    'wt/wt'      = "w.w",
    'mut/mut'    = "m.m",
    'wt/mut'     = "w.m",
    'effectif total (env.)' = "N")

header.line <- 2
data.lines <- c(28:31, 34:37, 40:43, 47:50, 55:58, 64:67, 76:83, 85, 87:95, 97, 99:106, 108, 111:118, 120, 122:129, 131, 133:140, 142, 149:156, 158, 161:168, 170, 172:177, 180:181, 183, 185:192, 194, 197:204, 206, 209:216, 218)

data.raw   <- as.data.frame(readxl::read_excel(fx.compet, sheet=1, skip=header.line-1, col_names=TRUE, .name_repair="minimal"))
data.clean <- data.raw[data.lines - header.line, names(cols)]
colnames(data.clean) <- cols

# Cleaning column types
data.clean$Generation <- as.numeric(data.clean$Generation)
data.clean$Cross      <- factor    (data.clean$Cross)
data.clean$Medium[data.clean$Medium == "glu"] <- "G"
data.clean$Medium     <- factor    (data.clean$Medium)
data.clean$Replicate  <- factor    (paste0("R", substr(data.clean$Replicate, 1, 1)))
data.clean$w.w        <- as.numeric(data.clean$w.w)
data.clean$m.m        <- as.numeric(data.clean$m.m)
data.clean$w.m        <- as.numeric(data.clean$w.m)
data.clean$N          <- as.numeric(data.clean$N)

write.table(data.clean, file=tx.compet, row.names=FALSE, sep="\t", quote=FALSE)
