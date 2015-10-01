# file: armax-cd.R
# coordinate descent for action rate maximization
# 2015-08-02 ky

armcd <- function(ord, basket, trace = FALSE, itlim = 100) { 
    # ord is ind permuted
    l1 <- function(z1, j) { # loss function with coordinate var
        zs[[j]] <<- z1 # j must be in ind
        loss.basket(zs[ind], basket)}
    iters <- 0
    dlt <- 1
    while (1e-4 < dlt && iters < itlim) {
        for (j in ord) {
            zs0 <- zs
            rslt <- optimize(f(z1, l1(z1, j)), c(-5, 5))
            zs[[j]] <<- rslt$minimum
            dlt <- max(abs(zs - zs0))
            if (trace == TRUE) {
                par <- zs[ind]
                value <- P.f(zs)
                now <- c(par, value)
                names(now) <- c(names(zs[ind]), "P")
                print(now)}
        }
        iters <- iters + 1
    }
    list(z = zs, P = P.f(zs), delta = dlt, iters = iters)}

