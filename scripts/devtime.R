source(here::here("scripts", "colors.R"))


fig.dir <- here::here("figures")

tx.devtime <- here::here("data", "devtime.txt")


data <- read.table(tx.devtime, header=TRUE, sep="\t", colClasses=c("factor", "factor", "numeric"))
data$Line <- factor(data$Line, levels=names(col.line))

data.size <- sapply(split(data, f=interaction(data$Rep, data$Line)), FUN=nrow)
barplot(data.size, col=col.line[sapply(strsplit(names(data.size), split="\\."), "[", 2)], xaxt="n", ylab="Number of observations")

data.spl <- sapply(split(data, f=interaction(data$Rep, data$Line)), FUN=function(x) x$devtime)
ll <- sapply(strsplit(names(data.spl), split="\\."), "[", 2)
at <- seq_along(ll)+cumsum(c(0,4*(diff(as.numeric(factor(ll)))!=0)))
boxplot(data.spl, col=col.line[ll], outline=FALSE, xaxt="n", at=at, ylab="Development time (days)")
axis(1, at=tapply(at, ll, FUN=mean), labels=names(tapply(at, ll, FUN=mean)))

boxplot(data$devtime ~ data$Line, col=col.line[levels(data$Line)], xlab="Line", ylab="Development time (days)")

hist(resid(lm(devtime ~ Line * Rep, data=data)), breaks=50)

contrasts <- rbind('3B.vs.6A'=c(1, -1, 0, 0), '10B.vs.12B'=c(0, 0, 1, -1), '3B6A.vs.10B12B'=c(0.5, 0.5, -0.5, -0.5))
colnames(contrasts) <- names(col.line)

mod <- lme4::lmer(devtime ~ Line + (1|Rep) + (1|Rep:Line), data=data)

multcomp::glht(mod, linfct = multcomp::mcp(Line = contrasts)) |>
        summary() |> print()
