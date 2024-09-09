source(here::here("scripts", "colors.R"))
source(here::here("scripts", "model-evol.R"))


fig.dir <- here::here("figures")

tx.compet <- here::here("data", "compet.txt")

data <- read.table(tx.compet, header=TRUE, sep="\t", colClasses=c("numeric", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric"))
data$p.w <- (data$w.w + 0.5*data$w.m) / (data$w.w + data$w.m + data$m.m)

# Computes Hardy-Weinberg chisq from vectors of counts
chi2HW <- function(nAA, nAB, nBB) 
    mapply(nAA, nAB, nBB, FUN=function(nnAA, nnAB, nnBB) { 
        n <- nnAA + nnAB + nnBB
        p <- (nnAA + 0.5*nnAB)/n
        (nnAA - n*p^2)^2 / (n*p^2) + (nnAB - n*2*p*(1-p))^2/ ( n*2*p*(1-p)) + (nnBB - n*(1-p)^2)^2 / ( n*(1-p)^2) 
    } )
    
# Turns model objects into data frames
as.df.coef <- function(mod) as.data.frame(as.list(mod$mle))
as.df.var  <- function(mod) as.data.frame(as.list(setNames(diag(mod$vcov), nm=paste0("V.", names(mod$mle)))))
as.df.conf <- function(mod) as.data.frame(as.list(setNames(c(mod$confint), nm=outer(rownames(mod$confint), colnames(mod$confint), FUN=paste0))), check.names=FALSE)


pdf(file.path(fig.dir, "compet-chi2.pdf"),  width=3, height=3, pointsize=8)
    hh <- hist(1-pchisq(chi2HW(data$w.w, data$w.m, data$m.m), df=1), breaks=20, xlab=expression(chi^2), 
        ylab="Frequency", main=expression(chi^2*" distribution"))
    abline(h=mean(hh$counts), lty=2, col="red")
dev.off()

# Not very clean: the for loops for plotting also perform model fitting
df.models <- NULL


pdf(file.path(fig.dir, "compet-timeseries.pdf"),  width=8, height=6, pointsize=8)
    layout(rbind(1:2, c(3,0)))
    par(cex=1, mar=c(4, 4, 3, 1))
    
    for (mm in levels(data$Medium)) {
        plot(NULL, xlim=range(data$Generation), ylim=c(0,1), xlab="Generation", ylab="freq(w)", main=paste0("Medium: ", mm))
        for (cc in levels(data$Cross))
            for (rr in levels(data$Replicate)) {
                sel <- data$Medium == mm & data$Cross == cc & data$Replicate == rr
                if (sum(sel) < 2) next
                mydat <- data[sel,]
                mydat <- mydat[order(mydat$Generation),]
                lines(mydat$Generation, mydat$p.w, col=col.cross[cc], type="b", lty=2)
                mod <- try(model1(mydat, dominance=FALSE, distrib="binomial"))
                if (class(mod) != "try-error") {
                    lines(0:(length(mod$pred)-1), mod$pred, col=col.cross[cc], lwd=3)
                    df.models <- rbind(df.models, cbind(data.frame(Medium=mm, Cross=cc, Replicate=rr), as.df.coef(mod), as.df.var(mod), as.df.conf(mod)))
                }
            }
        if (mm == levels(data$Medium)[1])
            legend("bottomright", fill=col.cross, legend=names(col.cross))
    }
dev.off()


num.params <- 2
pdf(file.path(fig.dir, "compet-param.pdf"),  width=4*num.params, height=3, pointsize=8)

    layout(t(1:num.params))
    par(cex=1, mar=c(1, 4, 1, 1))
    
    for (variable in colnames(df.models)[4:(3+num.params)]) {
        var.est <- df.models[,variable]
        var.se  <- sqrt(df.models[,paste0("V.", variable)])
        plot(NULL, xlim=c(0, nrow(df.models)+1), ylim=c(min(var.est - 2*var.se), max(var.est + 2*var.se)), xaxt="n", xlab="", ylab=variable)
        #arrows(x0=seq_along(var.est), y0=var.est-1.96*var.se, y1=var.est+1.96*var.se, col=col.medium[df.models$Medium], length=0)
        arrows(x0=seq_len(nrow(df.models)), 
            y0=df.models[,paste0(variable, "2.5 %")], 
            y1=df.models[,paste0(variable, "97.5 %")], 
            col=col.medium[df.models$Medium], length=0)
        points(var.est, col=col.medium[df.models$Medium], pch=pch.cross[df.models$Cross])
        if (variable == "s") abline(h=0, lty=3, col="gray")
        if (variable == "h") abline(h=0.5, tly=3, col="gray")
        legend("top", fill=col.medium, legend=names(col.medium), horiz=TRUE, bty="n")
    }
dev.off()

mod.all <- model1(data)
mod.HWo <- model1(data[1-pchisq(chi2HW(data$w.w, data$w.m, data$m.m), df=1) > 0.05,])

sink(file.path(fig.dir, "compet-model.txt"))
    cat("All data: \n")
    cat("p0: ", round(mod.all$mle["p0"], digits=3), " CI 95%: ", paste0(round(mod.all$confint["p0",], digits=3), collapse = " -- "), ".\n")
    cat("s : ", round(mod.all$mle["s" ], digits=3), " CI 95%: ", paste0(round(mod.all$confint["s" ,], digits=3), collapse = " -- "), ".\n")
    cat("Only HW samples: \n")
    cat("p0: ", round(mod.HWo$mle["p0"], digits=3), " CI 95%: ", paste0(round(mod.HWo$confint["p0",], digits=3), collapse = " -- "), ".\n")
    cat("s : ", round(mod.HWo$mle["s" ], digits=3), " CI 95%: ", paste0(round(mod.HWo$confint["s" ,], digits=3), collapse = " -- "), ".\n")
sink(NULL)
