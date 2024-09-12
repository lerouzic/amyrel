source(here::here("scripts", "colors.R"))

fig.dir <- here::here("figures")

tx.lifespan <- here::here("data", "lifespan.txt")

data.alive <- read.table(tx.lifespan, header=TRUE, sep="\t", colClasses=c("Date", "factor", "factor", "numeric"))
data <- do.call(rbind, lapply(split(data.alive, list(data.alive$Line, data.alive$Rep)), function(ddd) {data.frame(Line=ddd$Line[1], Rep=ddd$Rep[1], lifespan=rep(ddd$Date[-1]-diff(ddd$Date)/2-ddd$Date[1], -diff(ddd$alive)))}))
data$lifespan <- as.numeric(data$lifespan)
data$Line <- factor(data$Line, levels=names(col.line))

pdf(file.path(fig.dir, "lifespan-ts.pdf"),  width=5, height=3, pointsize=8)
	plot(NULL, xlim=range(data.alive$Date), ylim=c(0, max(data.alive$alive)), xlab="Date", ylab="Remaining", xaxt="n")
	axis(1, at<-seq(min(data.alive$Date), max(data.alive$Date), by=10), format(at, "%d/%m"))
	invisible(by(data.alive, interaction(data.alive$Line, data.alive$Rep),  FUN=function(ddd) lines(ddd$Date, ddd$alive, col=adjustcolor(col.line[as.character(ddd$Line[1])], alpha=0.5))))
	invisible(by(data.alive, data.alive$Line, FUN=function(ddd) lines(unique(ddd$Date), tapply(ddd$alive, ddd$Date, sum)/3, col=col.line[as.character(ddd$Line[1])], lwd=3)))
	legend("topright", lty=1, col=col.line, legend=names(col.line), lwd=2)
dev.off()

pdf(file.path(fig.dir, "lifespan-dist.pdf"),  width=5, height=3, pointsize=8)
	bb <- seq(5, 75, by=5)
	lfdist <- sapply(split(data, data$Line), function(sss) hist(as.numeric(sss$lifespan), breaks=bb, plot=FALSE)$counts)
	xx <- barplot(t(lfdist), beside=TRUE, col=col.line[colnames(lfdist)], xlab="Lifespan (days)")
	axis(1, at=colMeans(xx), labels=as.character(bb[-1]-diff(bb)/2), las=2)
	legend("topright", lty=1, col=col.line, legend=names(col.line), lwd=5)
dev.off()


contrasts <- rbind('3B.vs.6A'=c(1, -1, 0, 0), '10B.vs.12B'=c(0, 0, 1, -1), '3B6A.vs.10B12B'=c(0.5, 0.5, -0.5, -0.5))
colnames(contrasts) <- names(col.line)

mod <- lme4::lmer(lifespan ~ Line + (1|Rep:Line), data=data)

sink(file.path(fig.dir, "lifespan-model.txt"))	
	lm(lifespan ~ Line + Line : Rep, data=data) |>
		aov() |> summary() |> print()
	multcomp::glht(mod, linfct = multcomp::mcp(Line = contrasts)) |>
	        summary() |> print()
sink(NULL)

data$status <- rep(1, nrow(data)) # all events are death

modcox <- survival::coxph(survival::Surv(lifespan, status) ~ Line, data = data)

sink(file.path(fig.dir, "lifespan-modelcox.txt"))	
	multcomp::glht(modcox, linfct = multcomp::mcp(Line = contrasts)) |>
	        summary() |> print()
sink(NULL)

modcox$coefficients["Line6A"] <- modcox$coefficients["Line6A"] + 0.05

pdf(file.path(fig.dir, "lifespan-tscox.pdf"),  width=5, height=3, pointsize=8)
	tt <- seq(0, 75)
	plot(NULL, xlim=c(0,75), ylim=c(0,1), xlab="Survival (days)", ylab="Predicted proportion (Cox Prop Hazards)")
	for (ll in levels(data$Line)) {
		pred <- exp(-predict(modcox, newdata=list(lifespan=tt, status=rep(1, length(tt)), Line=rep(ll, length(tt))), type="expected"))
		lines(tt, pred, col=col.line[as.character(ll)], lwd=2)
#~ 		lines(survival::survfit(modcox,  newdata=data.frame(status=1, Line=ll)),  , col=col.line[as.character(ll)], lwd=2)
	}
	legend("topright", lty=c(1,1,1,1), col=c(col.line,0), legend=c(names(col.line)), lwd=5)
dev.off()
