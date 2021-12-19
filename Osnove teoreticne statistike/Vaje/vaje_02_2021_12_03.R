# nepristranskost


# 1. Za genotipe AA, Aa in aa v genih velja t.i. ravnoteže Hardy-Weinberg (Rice, str. 273),
# in sicer so frekvence pojavljanj genotipov kot v tabeli pod nalogo
#   a.) Iz frekvence za genotip aa (n_aa) želimo oceniti parameter \theta. zapišite najenostavnejšo
#   (intuitivno) cenilko.
#   b.) Izračunajte pričakovano vrednost cenilke za \theta iz prvega vprašanja. Ali je cenilka
#   nepristranska?
#   c.) Izračunajte varianco za spremenljivko X. (Pomoč: izberite možne vrednosti x tako, da boste
#   lahko izarčunali E(X). Ali ima E(X) smiselno interpretacijo?)
#
#   X       AA              Aa                  aa
#   p(X)    (1-\theta)^2    2\theta(1-\☺theta)  \theta^2





n <- 200
reps <- 100000
vzorec_init <- replicate(reps, rexp(n, rate=2))

lambde_hat <- n/colSums(vzorec_init)

hist(lambde_hat, nclass = 30)
mean(lambde_hat)
