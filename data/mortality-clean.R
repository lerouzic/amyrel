fx.mortality <- here::here("mortality.xlsx")
tx.mort.weight <- here::here("mortality-weight.txt")
tx.mort.mort   <- here::here("mortality.txt")

############# Weight

weight.raw <- as.data.frame(readxl::read_excel(fx.mortality, sheet=1, range="B12:G62", col_names=FALSE))
weight.clean <- data.frame(
                    Date = weight.raw[,1],
                    Line = weight.raw[,2],
                    Female = weight.raw[,3],
                    Male = weight.raw[,5])
weight.clean <- weight.clean[!apply(weight.clean, 1, function(x) all(is.na(x))),]

for (i in seq_len(nrow(weight.clean))) if (is.na(weight.clean$Date[i])) weight.clean$Date[i] <- weight.clean$Date[i-1]
weight.clean$Line[weight.clean$Line == "3B1"] <- "3B"
weight.clean$Line[weight.clean$Line == "10b"] <- "10B"
weight.clean$Line[weight.clean$Line == "12b"] <- "12B"

write.table(weight.clean, file=tx.mort.weight, row.names=FALSE, sep="\t", quote=FALSE)

############# Mortality

mort.raw <- as.data.frame(readxl::read_excel(fx.mortality, sheet=3, range="A4:F26", col_names=FALSE))

mort.raw <- mort.raw[!apply(mort.raw, 1, function(x) all(is.na(x))),]
mort.raw[(!is.na(mort.raw[,1])) & mort.raw[,1] == 'qq jours après',1] <- NA

mort.clean <- as.data.frame(do.call(rbind, lapply(unique(mort.raw[!is.na(mort.raw[,1]),1]), function(dd) { i <- which(mort.raw[,1] == dd); cbind(rep(dd, 4), c("3B", "6A", "10B", "12B"), t(mort.raw[i:(i+2), 3:6])) })))

colnames(mort.clean) <- c("Date", "Line", "Hatched", "nonHateched", "Adults")
mort.clean$Date <- as.Date(as.numeric(mort.clean$Date), origin="1899-12-30")

write.table(mort.clean, file=tx.mort.mort, row.names=FALSE, sep="\t", quote=FALSE)
