

# model 1 is deterministic pop gen

model1.ts <- function(p0, s, h=0.5, Gmax=40) {
    ans <- c(p0, rep(NA, Gmax-1))
    for (i in 2:(Gmax+1)) {
        p <- ans[i-1]
        genot.freq <- c(p^2, 2*p*(1-p), (1-p)^2)
        aftersel <- genot.freq * c(1, 1-h*s, 1-s)
        aftersel <- aftersel / sum(aftersel)
        ans[i] <- aftersel[1]+0.5*aftersel[2]
    }
    ans
}


dhyperx<-function(x,m,n,k,log=FALSE){
       lp<-lchoose(m, x) + lchoose(n, k-x) - lchoose( m+n, k)
       if (log) {lp} else {exp(lp)}
}

model1.minusloglik <- function(p0, h, s, Generation, w.w, w.m, m.m, N, distrib="binomial") {

    if (p0 < 0 || p0 > 1) return(Inf)
    if (h < -1 || h > 3) return(Inf)
    if (s < -1 || s > 1) return(Inf)
    if (h*s > 1) return(Inf)
    
    Nobs <- 2*(w.w + w.m + m.m)
    wt.all <- 2*w.w + w.m
    Generation <- Generation - if(min(Generation) < 0) min(Generation) else 0
    
    ts <- model1.ts(p0=p0, s=s, h=h, Gmax=max(Generation)+1)
    
    theor <- ts[Generation+1]
    if (distrib == "hyper") {
        ans <- -sum(dhyper(wt.all, round(2*theor*N), round(2*(1-theor)*N), Nobs, log=TRUE), na.rm=TRUE)
    } else if (distrib == "binomial") {
        ans <- -sum(dbinom(wt.all, Nobs, theor, log=TRUE), na.rm=TRUE)
    } else { stop("Unknown distribution") }
#~     print(c(p0, h, s, ans))
    ans
}


model1 <- function(df, dominance=FALSE, confint=TRUE, ...) {
	library(bbmle)
    mll <- function(p0, h, s) model1.minusloglik(p0, h, s, Generation, w.w=w.w, w.m=w.m, m.m=m.m, N=N, ...)
    p0.guess <- mean(((df$w.w + 0.5*df$w.m)/(df$w.w + df$w.m + df$m.m))[df$Generation == min(df$Generation)])
    
    mod <- bbmle::mle2(mll, start=list(p0=p0.guess, s=0), fixed=list(h=0.5), data=df)
    if (dominance) {
        mod <- bbmle::mle2(mll, start=list(p0=mod@coef["p0"], s=mod@coef["s"], h=0.5), data=df)
    } 
    list(mle=mod@coef, vcov=vcov(mod), pred=do.call(model1.ts, c(as.list(mod@coef), Gmax=max(df$Generation))), confint=if(confint) suppressMessages(confint(mod)) else NULL)
}
