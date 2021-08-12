anti_log_aprox <- function(taper_pct=50, pot=100, load_res=10) {
  taper <- taper_pct/100
  (pot*load_res*taper)/(pot*taper+load_res)
}



log_aprox <- function(taper_pct=50, pot=100, load_res=10) {
  taper <- taper_pct/100
  parallel_res <- taper*pot*load_res/(taper*pot + load_res)
  pot*parallel_res/(parallel_res + (1-taper)*pot)
  #parallel_res + (1-taper)*pot
}

log_pot <- function(taper_pct = 50, pot=25, mid=0.15) {
  taper <- taper_pct/100
  b = (1/mid - 1)^2
  a = 1/(b-1)
  (a*b^taper - a)*pot
}

anti_log_pot <- function(taper_pct = 50, pot=25, mid=0.85) {
  taper <- taper_pct/100
  b = (1/mid - 1)^2
  a = 1/(b-1)
  (a*b^taper - a)*pot
}


