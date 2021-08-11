anti_log_aprox <- function(taper_pct=50, pot=100, load_res=10) {
  taper <- taper_pct/100
  (pot*load_res*taper)/(pot*taper+load_res)
}



log_res_divider <- function(taper_pct=50, pot=100, load_res=10) {
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

# real_pot = 150
# pot=500
# load_resistor=220
# 
# df_plot_data <- tibble(taper_pct=seq(0, 100, 0.1))
# df_plot_data <- df_plot_data %>%
#   mutate(tru_anti_log_pot = anti_log_pot(taper_pct, pot=real_pot),
#          aprox_anti_log_pot = anti_log_aprox(taper_pct, 
#                                                  pot=pot, load_res=load_resistor))
# 
# p1 <- df_plot_data %>%
#   rename(Approximation = aprox_anti_log_pot, 
#          Real = tru_anti_log_pot, rotation = taper_pct ) %>%
#   pivot_longer(c(Real, Approximation), names_to = "potentiometre",
#                values_to = "resistance") %>%
#   ggplot(aes(rotation, resistance, colour=potentiometre)) + 
#   geom_line() + 
#   theme_minimal() +
#   labs(title= ' Real vs Approx. Anti-log', x="Rotation %", y="Resistance (kohms)") +
#   theme(legend.title = element_blank())
#   # geom_function(fun=res_divider, args=list(pot=200, resistor =20)) +
#   # geom_function(fun=log_pot, args=list(pot=150), color="red") 
# p1
# ggplotly(p1) 

