avg_temp_per_displ_marker <- function(df_muni, curr_sel_data) {
  df_muni %>%
    semi_join(curr_sel_data, by = c("Cidade" = "NAME_1", "Estado" = "NAME_2")) %>%
    group_by(Heatwaves) %>%
    summarise(tavg = mean(Heatwaves, na.rm = TRUE))
}

avg_temp_per_state <- function(df_muni, curr_data) {
  curr_data %>%
    left_join(df_muni, by = c("NAME_1" = "Estado")) %>%
    group_by(NAME_1) %>%
    summarise(tavg = mean(Heatwaves, na.rm = TRUE)) %>%
    pull(tavg)
}

avg_temp_per_county <- function(df_muni, curr_data) {
  curr_data %>%
    left_join(df_muni, by = c("NAME_1" = "Estado", "NAME_2" = "Cidade")) %>%
    group_by(NAME_1, NAME_2) %>%
    summarise(tavg = mean(Heatwaves, na.rm = TRUE)) %>%
    pull(tavg)
}

