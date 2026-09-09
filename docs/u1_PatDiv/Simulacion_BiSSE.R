install.packages("diversitree")
library(diversitree)
library(viridis)

#################################################
# Simulación de un proceso bajo el modelo BiSSE
#################################################

# estado 0
lambda_0 <- 0.25
mu_0     <- 0.05

# estado 1
lambda_1 <- 0.30
mu_1     <- 0.01

# transiciones 
q01 <- 0.3
q10 <- 0.2

# vector de parametros
pars <- c(lambda_0, lambda_1,
          mu_0, mu_1,
          q01, q10)

# simular arbol
tree <- diversitree::tree.bisse(pars, include.extinct = T, x0=0, max.t = 15)
hist <- diversitree::history.from.sim.discrete(tree, 0:1)

# assign a color to each state
cols <- c("#440154FF", "#35B779FF")

# ver arbol completo
p<-plot(hist, 
     phy = tree,
     cols = cols,
     show.tip.label = FALSE,
     show.tip.state = FALSE,
     show.node.state = TRUE)

points(p$xy$xx[seq_along(tree$tip.label)],
       p$xy$yy[seq_along(tree$tip.label)],
       pch = 19,
       cex = 1,
       col = cols[match(hist$tip.state, hist$states)])

legend( "topleft", 
        legend = c("Estado 0", "Estado 1"),
        col = cols,
        lwd = 3,
        bty = "n")


# ver arbol de solo actuales
tree.extant <- prune(tree)
hist.extant <- diversitree::history.from.sim.discrete(tree.extant , 0:1)

p.ext <- plot(hist.extant, 
     phy = tree.extant,
     cols = cols,
     show.tip.label = FALSE,
     show.tip.state = FALSE,
     show.node.state = TRUE)

points(p.ext$xy$xx[seq_along(tree.extant$tip.label)],
       p.ext$xy$yy[seq_along(tree.extant$tip.label)],
       pch = 19,
       cex = 1,
       col = cols[match(hist.extant$tip.state, hist.extant$states)])

legend( "topleft", 
        legend = c("Estado 0", "Estado 1"),
        col = cols,
        lwd = 3,
        bty = "n")

# lado a lado
par(mfrow = c(1, 2))

p<-plot(hist, 
        phy = tree,
        cols = cols,
        show.tip.label = FALSE,
        show.tip.state = FALSE,
        show.node.state = TRUE,
        main = "Árbol completo")

points(p$xy$xx[seq_along(tree$tip.label)],
       p$xy$yy[seq_along(tree$tip.label)],
       pch = 19,
       cex = 1,
       col = cols[match(hist$tip.state, hist$states)])

legend( "topleft", 
        legend = c("Estado 0", "Estado 1"),
        col = cols,
        lwd = 3,
        bty = "n")

p.ext <- plot(hist.extant, 
              phy = tree.extant,
              cols = cols,
              show.tip.label = FALSE,
              show.tip.state = FALSE,
              show.node.state = TRUE,
              main= "Sólo actuales")

points(p.ext$xy$xx[seq_along(tree.extant$tip.label)],
       p.ext$xy$yy[seq_along(tree.extant$tip.label)],
       pch = 19,
       cex = 1,
       col = cols[match(hist.extant$tip.state, hist.extant$states)])


# reset plotting 
par(mfrow = c(1, 1))

