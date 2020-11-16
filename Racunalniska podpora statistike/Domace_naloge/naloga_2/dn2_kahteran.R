##########################################################################################
##########################################################################################
##
## Avtor:           Alen Kahteran
##
## Mail:            Alen.kahteran@gmail.com
##
## Predmet:         Racunalniska Podpora Statistike
##
## Domaca naloga:   2
##
## Datum:           09. 10. 2020
##
##########################################################################################
##########################################################################################

# navadna round() funkcija cudno deluje.
#
#
# > round(15, digits=-1)
#
# [1] 20
#
# > round(25, digits=-1)
#
# [1] 20
#

# funkcija za "korektno" zaokrozevanje
round2 = function(x, digits) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^digits
    z*posneg
}


# funkcija za izracun nakljucnega stevila med 0 in 100
nakljucenRezultatIzpita <- function() {
    # generira eno nakljucno stevilo med 0 in 100
    rand_numb <- runif(1, 0, 100)
    # vrne "korektno" zaokrozeno nakljucno stevilo
    return(round2(rand_numb, 0))
}

# funkcija za izpis procentov
izpisOdstotkov <- function(x) {
    # "zlepimo" x in znak % v tekstovno obliko
    return(paste0(x, "%"))
}

# funkcija za pretvorbo rezultata izpita + dodatnih tock v oceno
ocenaIzpita <- function(x, d) {
    # sestejemo rezultat izpita z dodatkom ki je ocena
    ocena <- x + d
    # preverimo kam sodi nasa ocena in vrnemo ustrezno oceno
    if (ocena < 50) {
        return(5)
    } else if (ocena >= 50 & ocena < 60) {
        return(6)
    } else if (ocena >= 60 & ocena < 70) {
        return(7)
    } else if (ocena >= 70 & ocena < 80) {
        return(8)
    } else if (ocena >= 80 & ocena < 90) {
        return(9)
    } else if (ocena >= 90) {
        return(10)
    } else {
        # tu le v primeru ce bi se hoteli igrati s kaksnim error handlingom
        return("neveljavna ocena")
    }
}

# funkcija za izpis teksta za izpit
izpisIzpita <- function(x, d) {
    # skupaj zlozimo vse podatke za ustrezen izpis ki ga vrnemo
    string_return <- paste("Rezultat izpita:",
                           izpisOdstotkov(x),
                           "dodatno",
                           izpisOdstotkov(d),
                           "ocena:",
                           ocenaIzpita(x, d)
                           )
    return(string_return)
}

# funkcija za izracun nakljucnega dodatka med 0 in 20
nakljucenDodatekIzpitu <- function() {
    # generira eno nakljucno stevilo med 0 in 20
    rand_numb <- runif(1, 0, 20)
    # vrne "korektno" zaokrozeno nakljucno stevilo
    return(round2(rand_numb, 0))
}

# izpise n izpisov z nakljucnimi podatki ne returna nic.
# oz ce napisemo v obliki
# x <- izpisIzpitov(100)
# bo nato x = NULL
izpisIzpitov <-  function(n) {
    for (i in 1:n) {
        print(izpisIzpita(nakljucenRezultatIzpita(), nakljucenDodatekIzpitu()))
    }
}

# izpise 100 izpisov
izpisIzpitov(100)
