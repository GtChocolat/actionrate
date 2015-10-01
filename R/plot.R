# file: armax-plot.R
# 2015-07-27 -- 2015-08-24 ky
# separated 2015-07-27 from armax.R
# plotters

base_size <- 12
label_size <- 5

p.df <- function(to = 3, length.out = 101) {
    zs <- seq(-to, to, length.out = length.out)
    ps <- p.f(zs)
    data.frame(z = zs, p = ps)}

p.plot <- function(zs) { # [y, p(y)] graph
    y_to <- max(abs(zs))
    p_fr <- min(p.f(y_to))
    zp <- p.df(to = y_to)
    plt <- ggplot(zp, mapping = aes(x = z, y = p)) +
        theme_bw(base_size = base_size) + geom_line() + ylim(p_fr, 1)
    sgn <- ifelse(p.f(min(zs)) < p.f(max(zs)), 1, -1)
    add_pt <- function(i) {
        plt + geom_point(x = zs[[i]], y = p.f(zs[[i]])) +
            annotate("text", x = zs[[i]] + sgn*0.25*y_to, y = p.f(zs[[i]]),
                     label = names(zs)[[i]], size = label_size)}
    Vectorize(f(i, plt <<- add_pt(i)))(1:length(zs))
    zs %>% P.f %>% shortnum -> P
    plt + labs(title = paste("P =", P))}

# wanted to make this more generic,
# but there are lots of things i don't understand, like aes
contour <- function(xl = 15, yl = 15) {
    x <- seq(-3, 3, length.out = xl)
    y <- seq(-3, 3, length.out = yl)
    z.f <- f(x, y, prod(p.f(resfun2(x, y))))
    z <- as.vector(outer(x, y, Vectorize(z.f)))
    df <- as.data.frame(cbind(expand.grid(x, y), z))
    colnames(df) <- c("beer", "peanuts", "P")
    plt <- ggplot(df, #as.data.frame(df),
                  aes(x = beer, y = peanuts, z = P)) +
        theme_bw(base_size = base_size) + coord_fixed() + ggtitle("P") +
        stat_contour(aes(color = ..level..))
    direct.label(plt)}

# z to x bijection plots in ggplot2

crosshair <- function(plt, x, y) {
    plt + geom_vline(xintercept = x, color = "red", linetype = "solid") +
        geom_hline(yintercept = y, color = "red", linetype = "solid")}

item.plot <- function(i, xy = "all", v = NA, length.out = 101) {
    # xy = "zp", "zl", "zy", "yp", "yl", "yy", "all"
    # v = crosshair x-axis given in z
    y <- i$tab$y
    p <- i$tab$p
    l <- i$tab$l
    z <- i$tab$z
    K <- length(y)
    zs <- seq(i$tab$z[[1]], i$tab$z[[K]], length.out = length.out)
    ys <- seq(i$tab$y[[1]], i$tab$y[[K]], length.out = length.out)
    zs.y <- Vectorize(f(z, y.item(z, i)))(zs)
    yp.f <- f(y, p.f(yz.item(y, i)))
    yps <- Vectorize(yp.f)(ys)
    ls <- Vectorize(f(y, q.f(yz.item(y, i))))(ys)
    if (xy == "all" || xy == "zp") { # zp
        zp.g <- ggplot(data.frame(z, p), aes(x = z, y = p)) + 
            geom_point() + theme_bw() + 
            stat_function(fun = p.f, linetype = "dashed")
        if (!is.na(v)) zp.g <- crosshair(zp.g, v, p.f(v))
        if (xy != "all") return(zp.g)}
    if (xy == "all" || xy == "zl") { # zl
        zl.g <- ggplot(data.frame(z, l), aes(x = z, y = l)) + 
            geom_point() + theme_bw() + stat_function(fun = q.f)
        if (!is.na(v)) zl.g <- crosshair(zl.g, v, q.f(v))
        if (xy != "all") return(zl.g)}
    if (xy == "all" || xy == "zy") { # zy
        zy.g <- ggplot(data.frame(z, y), aes(x = z, y = y)) +
            geom_point() + theme_bw() +
            geom_line(aes(x = zs, y = zs.y), data.frame(zs, zs.y))
        if (!is.na(v)) zy.g <- crosshair(zy.g, v, y.item(v, i))
        if (xy != "all") return(zy.g)}
    if (xy == "all" || xy == "yp") { # yp
        yp.g <- ggplot(data.frame(y, p), aes(x = y, y = p)) + 
            geom_point() + theme_bw() +
            geom_line(aes(x = ys, y = yps), data.frame(ys, yps), 
                      linetype = "dotted")
        if (!is.na(v)) yp.g <- crosshair(yp.g, y.item(v, i), p.f(v))
        if (xy != "all") {
            yp.g <- yp.g + labs(title = i$chr)
            return(yp.g)}}
    if (xy == "all" || xy == "yl") { # yl
        yl.g <- ggplot() + theme_bw() +
            geom_point(aes(x = y, y = l), data.frame(y, l)) + 
            geom_line(aes(x = ys, y = ls), data.frame(ys, ls), 
                      linetype = "dotted")
        if (!is.na(v)) yl.g <- crosshair(yl.g, y.item(v, i), q.f(v))
        if (xy != "all") {
            yl.g <- yl.g + labs(title = i$chr)
            return(yl.g)}}
    if (xy == "all" || xy == "yy") { # identity
        yy.g <- ggplot(data.frame(y, y), aes(x = y, y = y)) +
            geom_point() + theme_bw() + 
            stat_function(fun = f(y, y), linetype = "dotted") +
            annotate("text", # x y labels ma b elaboratd
                     x = (y[[1]] + y[[K]])/1.7, 
                     y = (y[[1]] + y[[which(l == 0)]])/2, 
                     label = paste(i$chr, i$unit), size = label_size)
        if (!is.na(v)) {
            vy <- y.item(v, i)
            yy.g <- crosshair(yy.g, vy, vy)}
        if (xy != "all") return(yy.g)}
    if (xy == "all")
        multiplot(zp.g, zl.g, zy.g, yp.g, yl.g, yy.g, cols = 2)}
