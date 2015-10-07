# file: armax.R
# action probability maximization
# 2015-07-02 -- 2015-09-22  ky

# decision making as inverse problems

# the main var to deal with is z rather than x
# z_caus: zs -> xs --caus--> xs -> zs
# naming convention: a function to z may have z omitted in name

if (!("install.load" %in% rownames(installed.packages()))) 
    install.packages(install.load)
library(install.load)

install_load("pryr", "plyr", "dplyr", "ggplot2", "directlabels")

# in ggplot2 use multiplot instead of par(mfrow = c(3,2))
source("multiplot.R")

afply <- function(.data, .flist = NULL) 
    Vectorize(f(i, .flist[[i]](.data)))(1:length(.flist))

rare <- 1e-3 # events below this prob do not take place in practice
init.item.l <- function(i) {
    i$tab$l <- -log(i$tab$p)
    i}

a0.f <- f(y0, z0, y1, z1, (y0*z1 - y1*z0)/(z1 - z0))
a1.f <- f(y0, z0, y1, z1, (y1 - y0)/(z1 - z0))
as.f <- function(a01.f, y, z) {
    a01.v <- Vectorize(f(k, a01.f(y[[k]], z[[k]], y[[k + 1]], z[[k + 1]])))
    c(a01.v(1:(length(y) - 1)), NA)}

init.item.a <- function(i) {
    y <- i$tab$y
    l <- i$tab$l
    k0 <- Position(f(v, identical(v, 0)), l)
    kz <- Vectorize(f(k, ifelse(k < k0, -sqrt(l[[k]]), sqrt(l[[k]]))))
    i$tab$z <- z <- kz(1:length(y))
    i$tab$a0 <- as.f(a0.f, y, z)
    i$tab$a1 <- as.f(a1.f, y, z)
    i}

q.f <- f(y, y^2)
p.f <- f(z, exp(-q.f(z)))
P.f <- f(z, prod(p.f(z)))
yl.item <- f(y, i, q.f(yz.item(y, i))) # y to subloss

zk.item <- function(z, i) { # z to k
    zs <- i$tab$z
    from_to <- which(z < zs, arr.ind = TRUE)
    if (length(from_to) == length(zs)) return(1)
    if (length(from_to) == 0)          return(length(zs) - 1)
    else                               return(min(from_to) - 1)}

yk.item <- function(y, i) { # y to k
    ys <- i$tab$y
    from_to <- which(y < ys, arr.ind = TRUE)
    if (length(from_to) == length(ys)) return(1)
    if (length(from_to) == 0)          return(length(ys) - 1)
    else                               return(min(from_to) - 1)}

y.item <- function(z, i) { # y = a0 + a1*z
    k <- zk.item(z, i)
    a0 <- i$tab$a0[[k]]
    a1 <- i$tab$a1[[k]]
    a0 + a1*z}

yz.item <- function(y, i) { # z = (y - a0)/a1
    k <- yk.item(y, i)
    a0 <- i$tab$a0[[k]]
    a1 <- i$tab$a1[[k]]
    (y - a0)/a1}

shortnum.digits <- round(-log10(rare)) - 1 # digits
shortnum <- f(x, round(x, shortnum.digits)) # shorten number

# tried to fetch name string from name but failed. hint:
# lapply( list(a=4,b=5), function(x) {
#     nm <- deparse(substitute(x)); strsplit(nm, '\\[')} )
template.item <- function(chr) { # item template
    i <- as.name(chr)
    i <- NULL
    i$chr <- chr
    i$unit <- "[]"
    i$tab <- data.frame(y = c(0, 1, 0), p = c(rare, 1, rare))
    i$caus <- function(y) y[chr]
    i}
# error if
#   class(item) <- "item"
# is included in the template

y.basket <- function(zs, basket) { # zs to ys
    ys <- Vectorize(f(i, y.item(zs[[i]], basket[[i]])))(1:length(zs))
    names(ys) <- names(zs)
    ys}

yz.basket <- function(ys, basket) { # ys to zs
    zs <- Vectorize(f(i, yz.item(ys[[i]], basket[[i]])))(1:length(zs))
    names(zs) <- names(ys)
    zs}

print.item <- function(i) {
    cat(i$chr, i$unit, sep = " ", "\n")
    tab_sorted <- data.frame(y = i$tab$y, z = i$tab$z, 
                             p = i$tab$p, l = i$tab$l,
                             a0 = i$tab$a0, a1 = i$tab$a1)
    print(shortnum(tab_sorted))
    cat("caus = ")
    print(i$caus)}
comment(print.item) <- c("y = a0 + a1*z", "p = prob", "l = loss", "z = std var")

init.item <- f(i, i %>% init.item.l %>% init.item.a)

print.basket <- f(basket, cat("items: \n", names(basket)))

z_caus <- function(zs, basket) { # update z of the item
    zs %>% y.basket(basket) -> ys
    basket %>% Map(f(i, i$caus), .) -> fl
    ys %<>% afply(fl) %>% yz.basket(basket)
    names(zs) -> names(ys)
    ys}

resfun.basket <- function(z_ind, basket) {
    z_ind ->> zs[ind]
    zs %>% z_caus(basket) ->> zs
    zs}

loss.basket <- function(z_ind, basket) {
    z_ind %>% resfun.basket(basket)
    zs %>% q.f %>% sum}

# resfun.mk <- f(z_ind, beerpeanuts, f(z_ind, resfun.basket(z_ind, beerpeanuts)))
# resfun2 <- f(x, y, resfun(c(x, y))) # resfun in 2 vars to plot contour

source("plot.R")
source("cd.R")
#source("beerpeanuts.R")
