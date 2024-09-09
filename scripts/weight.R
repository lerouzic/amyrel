source(here::here("scripts", "colors.R"))

fig.dir <- here::here("figures")

tx.mort.weight <- here::here("data", "mortality-weight.txt")

weight <- read.table(tx.mort.weight, header=TRUE)

weight.byline <- do.call(cbind, split(weight[,c("Female","Male")], f=weight$Line)[names(col.line)])
sex <- sapply(strsplit(colnames(weight.byline), ".", fixed=TRUE), "[", 2)

pdf(file.path(fig.dir, "weight-bxp.pdf"),  width=4, height=3, pointsize=8)
    par(mar=c(4,4,1,1))
    boxplot(weight.byline, xaxt="n", at=c(1:4, 6:9), ylim=c(0, max(weight.byline)), ylab="Weight (g/100 ind)", col=col.sex[sex])
    axis(1, at=c(1.5, 3.5, 6.5, 8.5), labels=names(col.line))
    legend("bottomright", fill=col.sex, legend=names(col.sex))
dev.off()

weight2 <- data.frame(Line=factor(rep(weight$Line, 2)), Sex=factor(rep(c("Female","Male"), each=nrow(weight))), Weight=c(weight$Female, weight$Male))

sink(file.path(fig.dir, "weight-aov.txt"))
    print(summary(aov(lm(Weight ~ Sex * Line, data=weight2))))
sink(NULL)
