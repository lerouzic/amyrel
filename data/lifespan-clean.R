fx.lifespan <- here::here("data", "lifespan.xlsx")
tx.lifespan   <- here::here("data", "lifespan.txt")

lifespan.raw <- as.data.frame(readxl::read_excel(fx.lifespan, sheet=1, range="A33:N55", col_names=TRUE))

lifespan.clean <- do.call(rbind, lapply(3:ncol(lifespan.raw), function(cc) {
					dd <- lifespan.raw[,cc]
					dd <- dd[!is.na(dd)]
					sp <- strsplit(colnames(lifespan.raw)[cc], split="[ ♂]")[[1]]
					data.frame(Date=lifespan.raw[1:length(dd),"date"], Line=sp[1], Rep=sp[4], alive=dd)
				}))
					
write.table(lifespan.clean, tx.lifespan, row.names=FALSE, sep="\t", quote=FALSE)
