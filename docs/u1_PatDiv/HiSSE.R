
library(diversitree)

#################################################
# Simulación de un proceso bajo el modelo HiSSE
#################################################

# Definimos especiacion y extincion

#---------------------------------------------------------------------------------
# Especiación para los dos estados observados con sus respectivos estados ocultos
# estado observado 0
lambda_0A <- 0.3
lambda_0B <- 0.6

# estado observado 1
lambda_1A <- 0.3
lambda_1B <- 0.6

#---------------------------------------------------------------------------------
# Extinción para los dos estados observados con sus respectivos estados ocultos
# estado observado 0
mu_0A <- 0.1
mu_0B <- 0.1

# estado observado 1
mu_1A <- 0.1
mu_1B <- 0.1

#---------------------------------------------------------------------------------
# tasas de cambio entre estados
# entre estados observados
q01 <- 1
q10 <- 1

# entre estados ocultos
qAB <- 1
qBA <- 1

#---------------------------------------------------------------------------------
# hacer vector de parametros
params <- c(
    lambda_0A,lambda_0B, lambda_1A, lambda_1B, # lambdas
    mu_0A, mu_0B, mu_1A, mu_1B, # mu
    qAB, q01, 0, # 0A -> 0B, 0A -> 1A, 0A -> 1B
    qBA, 0, q01, # 0B -> 0A, 0B -> 1A, 0B -> 1B
    q10, 0, qAB, # 1A -> 0A, 1A -> 0B, 1A -> 1B
    0, q10, qBA  # 1B -> 0A, 1B -> 0B, 1B -> 1A
    )

#---------------------------------------------------------------------------------
# simular proceso HiSSE
# estado inicial x0 = 0A
tree <- tree.musse(params, max.t = 10, x0=1, include.extinct=TRUE)
hist <- diversitree::history.from.sim.discrete(tree, 1:4)


#---------------------------------------------------------------------------------
# plotear!
par(mfrow = c(1, 2))
# Primero coloreando por estados observados

cols <- c("#440154FF", # 0A
          "#440154FF", # 0B
          "#35B779FF", # 1A
          "#35B779FF"  # 1B
)

# ver arbol completo
p <- plot(hist, 
        phy = tree,
        cols = cols,
        show.tip.label = FALSE,
        show.tip.state = FALSE,
        show.node.state = TRUE, 
        lwd = 3)

points(p$xy$xx[seq_along(tree$tip.label)],
       p$xy$yy[seq_along(tree$tip.label)],
       pch = 19,
       cex = 1,
       col = cols[match(hist$tip.state, hist$states)])

# Observed
legend("topleft",
       legend = c("0", "1"),
       title = "Estados observados",
       col = c("#440154FF", "#35B779FF"),
       lwd = 3,
       bty = "n", 
       cex=0.8)

# legend( "topleft", 
#         legend = c("0A", "0B", "1A", "1B"),
#         title = "ESTADOS OBSERVADOS",
#         col = cols,
#         lwd = 3,
#         bty = "n")


# Ahora coloreando por estados ocultos
cols <- c("grey80", # 0A
          "grey30", # 0B
          "grey80", # 1A
          "grey30"  # 1B
)

# ver arbol completo
p_oculto <- plot(hist, 
          phy = tree,
          cols = cols,
          show.tip.label = FALSE,
          show.tip.state = FALSE,
          show.node.state = TRUE,
          lwd =3)

points(p_oculto$xy$xx[seq_along(tree$tip.label)],
       p_oculto$xy$yy[seq_along(tree$tip.label)],
       pch = 19,
       cex = 1,
       col = cols[match(hist$tip.state, hist$states)])

# Hidden
legend("topleft",
       legend = c("A", "B"),
       title = "Estados ocultos",
       col = c("grey80", "grey30"),
       lwd = 3,
       bty = "n",
       cex = 0.8)

# legend( "topleft", 
#         legend = c("0A", "0B", "1A", "1B"),
#         title = "ESTADOS OCULTOS",
#         col = cols,
#         lwd = 3,
#         bty = "n")

