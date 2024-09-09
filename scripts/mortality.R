source(here::here("scripts", "colors.R"))

fig.dir <- here::here("figures")

tx.mort <- here::here("data", "mortality.txt")

data <- read.table(tx.mort, header=TRUE, colClasses=c("Date", "factor", "numeric", "numeric", "numeric"))
data$Line <- factor(data$Line, levels=names(col.line))

contrasts <- rbind('3B.vs.6A'=c(1, -1, 0, 0), '10B.vs.12B'=c(0, 0, 1, -1), '3B6A.vs.10B12B'=c(0.5, 0.5, -0.5, -0.5))
colnames(contrasts) <- names(col.line)

pdf(file.path(fig.dir, "mortality-hatched.pdf"), width=4, height=3, pointsize=8)
    boxplot(split(data$Hatched/(data$Hatched+data$nonHatched), f=data$Line)[names(col.line)], ylim=c(0,1), col=col.line, ylab="Hatching rate")
dev.off()

model.hatched <- glm(cbind(Hatched, nonHatched) ~ Line, data=data, family="quasibinomial")

sink(file.path(fig.dir, "mortality-hatched.txt"))
    multcomp::glht(model.hatched, linfct = multcomp::mcp(Line = contrasts)) |>
        summary() |> print()
sink()

pdf(file.path(fig.dir, "mortality-surv.pdf"), width=4, height=3, pointsize=8)
    boxplot(split(data$Adults/data$Hatched, f=data$Line)[names(col.line)], ylim=c(0,1), col=col.line, ylab="Survival rate")
dev.off()

model.surv <- glm(cbind(Adults, Hatched-Adults) ~ Line, data=data, family="quasibinomial")

sink(file.path(fig.dir, "mortality-surv.txt"))
    multcomp::glht(model.surv, linfct = multcomp::mcp(Line = contrasts)) |>
        summary() |> print()
sink()
