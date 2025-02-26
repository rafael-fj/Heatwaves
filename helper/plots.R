add_bar <- function(plot, col_name) {
  plot %>% e_bar_(col_name, barGap = "-50%")
}

add_line <- function(plot, col_name) {
  plot %>% e_line_(col_name)
}

add_scatter <- function(plot, intensidade, duracao) {
  #plot %>% e_scatter_(col_name)
  plot %>% e_scatter(intensidade, duracao)
}

add_cumulative_line <- function(plot, col_name) {
  cum_values <- cumsum(df[[col_name]])
  plot %>% e_line_(cum_values, name = col_name)
}

########################## Plot Heat - Consulta ##########################

plot_heat <- function (data, x = t, y = temp, min_duration = 5, spread = 0,
            metric = "intensity_cumulative", start_date = NULL, end_date = NULL,
            category = FALSE, x_axis_title = NULL, x_axis_text_angle = NULL,
            y_axis_title = NULL, y_axis_range = NULL)
  {
    date_end <- date_start <- duration <- temp <- NULL
    if (!(exists("event", data)) | !(exists("climatology", data)))
      stop("Please ensure you are running this function on the output of 'heatwaveR::detect_event()'")
    ts_x <- eval(substitute(x), data$climatology)
    data$climatology$ts_x <- ts_x
    ts_y <- eval(substitute(y), data$climatology)
    data$climatology$ts_y <- ts_y
    if (is.null(start_date))
      start_date <- min(data$climatology$ts_x)
    if (is.null(end_date))
      end_date <- max(data$climatology$ts_x)
    event <- data$event %>% dplyr::filter(date_end >= start_date &
                                            date_start <= end_date) %>% data.frame()
    if (nrow(event) == 0){
      #print(paste0("this is start_date:", start_date))
      #print(paste0("this is end_date:", end_date))
      data_no <- data$climatology %>%
        filter(t >= start_date & t <= end_date)
      #print(head(data_no))

      plot_no <- ggplot() +
        geom_line(data = data_no, aes(x = ts_x, y = ts_y, color = "Temperatura"), size = 0.6) +
        geom_line(data = data_no, aes(x = ts_x, y = thresh, color = "Limite"), size = 0.6) +
        geom_line(data = data_no, aes(x = ts_x, y = seas, color = "Climatologia"), size = 0.7, alpha = 1) +
        scale_color_manual(name = NULL,
                           values = c("Temperatura" = "black", "Limite" = "darkgreen", "Climatologia" = "blue")) +
        labs(x = x_axis_title, y = y_axis_title) +
        theme(
          plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), angle = x_axis_text_angle),
          axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.ticks.length = unit(-0.25, "cm"),
          legend.background = element_rect(colour = "black"),
          legend.direction = "horizontal",
          legend.justification = c(0, 0),
          legend.position = c(0.005, 0.015),
          legend.key = element_blank()
        )
      return(plot_no)
      stop()
      #stop("Não houveram ondas de calor no ano selecionado.")
    }
    if (!(metric %in% c("intensity_mean", "intensity_max", "intensity_var",
                        "intensity_cumulative", "intensity_mean_relThresh",
                        "intensity_max_relThresh", "intensity_var_relThresh",
                        "intensity_cumulative_relThresh", "intensity_mean_abs",
                        "intensity_max_abs", "intensity_var_abs", "intensity_cumulative_abs",
                        "rate_onset", "rate_decline"))) {
      stop("Please ensure you have spelled the desired metric correctly.")
    }

    index_start <- index_end <- event_idx <- NULL
    event_idx <- as.vector(-abs(event[colnames(event) == metric])[,
                                                                  1])
    event <- event[base::order(event_idx), ]
    event <- event %>% dplyr::filter(duration >= min_duration) %>%
      dplyr::mutate(index_start_fix = index_start - 1, index_end_fix = index_end +
                      1)
    event_top <- event[1, ]
    date_spread <- seq((event_top$date_start - spread), (event_top$date_end +
                                                           spread), by = "day")
    event_sub <- event %>% dplyr::filter(date_start >= min(date_spread),
                                         date_end <= max(date_spread))
    thresh_2x <- thresh_3x <- thresh_4x <- NULL
    clim_diff <- data$climatology %>% dplyr::mutate(diff = thresh -
                                                      seas, thresh_2x = thresh + diff, thresh_3x = thresh_2x +
                                                      diff, thresh_4x = thresh_3x + diff)
    clim_events <- data.frame()
    for (i in seq_len(nrow(event_sub))) {
      clim_sub <- clim_diff[(event_sub$index_start_fix[i]):(event_sub$index_end_fix[i]),
      ]
      clim_events <- rbind(clim_events, clim_sub)
    }
    clim_top <- clim_diff[event_top$index_start_fix:event_top$index_end_fix,
    ]
    clim_spread <- clim_diff %>% dplyr::filter(ts_x %in% date_spread)
    thresh <- seas <- y1 <- y2 <- NULL
    if (event_top$intensity_mean > 0) {
      fillCol <- c(events = "salmon", `peak event` = "red")
      clim_events$y1 <- clim_events$ts_y
      clim_events$y2 <- clim_events$thresh
      clim_top$y1 <- clim_top$ts_y
      clim_top$y2 <- clim_top$thresh
    }
    else {
      fillCol <- c(events = "steelblue3", `peak event` = "navy")
      clim_events$y1 <- clim_events$thresh
      clim_events$y2 <- clim_events$ts_y
      clim_top$y1 <- clim_top$thresh
      clim_top$y2 <- clim_top$ts_y
    }
    if (!is.null(y_axis_title)) {
      if (!is.character(y_axis_title))
        stop("Please ensure that the argument provided to 'y_axis_title' is a character string.")
      ylabel <- y_axis_title
    }
    else {
      ylabel <- expression(paste("Temperatura [", degree,
                                 "C]"))
    }
    if (!is.null(x_axis_title)) {
      if (!is.character(x_axis_title))
        stop("Please ensure that the argument provided to 'x_axis_title' is a character string.")
      xlabel <- x_axis_title
    }
    else {
      xlabel <- NULL
    }
    if (!is.null(x_axis_text_angle)) {
      if (!is.numeric(x_axis_text_angle))
        stop("Please ensure that the argument provided to 'x_axis_text_angle' is a number.")
      xtangle <- x_axis_text_angle
    }
    else {
      xtangle <- 0
    }
    ep <- ggplot(data = clim_spread, aes(x = ts_x, y = ts_y)) +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
      labs(x = xlabel, y = ylabel) + theme(plot.background = element_blank(),
                                           panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black",
                                                                                                                        fill = NA, size = 0.75), panel.grid.minor = element_line(colour = NA),
                                           panel.grid.major = element_line(colour = "black", size = 0.2,
                                                                           linetype = "dotted"), axis.text = element_text(colour = "black"),
                                           axis.text.x = element_text(margin = unit(c(0.5, 0.5,
                                                                                      0.5, 0.5), "cm"), angle = xtangle), axis.text.y = element_text(margin = unit(c(0.5,
                                                                                                                                                                     0.5, 0.5, 0.5), "cm")), axis.ticks.length = unit(-0.25,
                                                                                                                                                                                                                      "cm"), legend.background = element_rect(colour = "black"),
                                           legend.direction = "horizontal", legend.justification = c(0,
                                                                                                     0), legend.position = c(0.005, 0.015), legend.key = element_blank())
    if (category) {
      lineColCat <- c(Temperatura = "black", Climatologia = "gray20",
                      Limite = "darkgreen", `2x Limite` = "darkgreen",
                      `3x Limite` = "darkgreen", `4x Limite` = "darkgreen")
      if (event_top$intensity_mean < 0) {
        fillColCat <- c(Moderate = "#C7ECF2", Strong = "#85B7CC",
                        Severe = "#4A6A94", Extreme = "#111433")
        ep <- ep + geom_flame(data = clim_events, size = 0.5,
                              aes(x = ts_x, y = thresh, y2 = ts_y, fill = "Moderate")) +
          geom_flame(data = clim_events, size = 0.5, aes(x = ts_x,
                                                         y = thresh_2x, y2 = ts_y, fill = "Strong")) +
          geom_flame(data = clim_events, size = 0.5, aes(x = ts_x,
                                                         y = thresh_3x, y2 = ts_y, fill = "Severe")) +
          geom_flame(data = clim_events, size = 0.5, aes(x = ts_x,
                                                         y = thresh_4x, y2 = ts_y, fill = "Extreme"))
      }
      else {
        fillColCat <- c(Moderate = "#ffc866", Strong = "#ff6900",
                        Severe = "#9e0000", Extreme = "#2d0000")
        ep <- ep + geom_flame(data = clim_events, size = 0.5,
                              aes(x = ts_x, y = y1, y2 = y2, fill = "Moderate")) +
          geom_flame(data = clim_events, size = 0.5, aes(x = ts_x,
                                                         y = y1, y2 = thresh_2x, fill = "Strong")) +
          geom_flame(data = clim_events, size = 0.5, aes(x = ts_x,
                                                         y = y1, y2 = thresh_3x, fill = "Severe")) +
          geom_flame(data = clim_events, size = 0.5, aes(x = ts_x,
                                                         y = y1, y2 = thresh_4x, fill = "Extreme"))
      }
      ep <- ep + geom_line(aes(y = thresh_2x, col = "2x Limite"),
                           size = 0.7, linetype = "dashed") + geom_line(aes(y = thresh_3x,
                                                                            col = "3x Limite"), size = 0.7, linetype = "dotdash") +
        geom_line(aes(y = thresh_4x, col = "4x Limite"),
                  size = 0.7, linetype = "dotted") + geom_line(aes(y = seas,
                                                                   col = "Climatologia"), size = 0.7, alpha = 1) + geom_line(aes(y = thresh,
                                                                                                                                col = "Limite"), size = 0.7, alpha = 1) + geom_line(aes(y = ts_y,
                                                                                                                                                                                           col = "Temperatura"), size = 0.6) + scale_colour_manual(name = NULL,
                                                                                                                                                                                                                                                   values = lineColCat, breaks = c("Temperatura", "Climatologia",
                                                                                                                                                                                                                                                                                   "Limite", "2x Limite", "3x Limite",
                                                                                                                                                                                                                                                                                   "4x Limite")) + scale_fill_manual(name = NULL,
                                                                                                                                                                                                                                                                                                                        values = fillColCat, guide = FALSE) + guides(colour = guide_legend(override.aes = list(linetype = c("solid",
                                                                                                                                                                                                                                                                                                                                                                                                                            "solid", "solid", "dashed", "dotdash", "dotted"),
                                                                                                                                                                                                                                                                                                                                                                                                               size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)))) + theme(legend.direction = "vertical")
      ep
    }
    else {
      lineCol <- c(Temperatura = "black", Climatologia = "blue",
                   Limite = "darkgreen")
      ep <- ep + geom_flame(data = clim_events, size = 0.5,
                            aes(x = ts_x, y = y1, y2 = y2, fill = "events")) +
        geom_flame(data = clim_top, size = 0.5, aes(x = ts_x,
                                                    y = y1, y2 = y2, fill = "peak event")) + geom_line(aes(y = seas,
                                                                                                           col = "Climatologia"), size = 0.7, alpha = 1) + geom_line(aes(y = thresh,
                                                                                                                                                                        col = "Limite"), size = 0.7, alpha = 1) + geom_line(aes(y = ts_y,
                                                                                                                                                                                                                                   col = "Temperatura"), size = 0.6) + scale_colour_manual(name = NULL,
                                                                                                                                                                                                                                                                                           values = lineCol, breaks = c("Temperatura", "Climatologia",
                                                                                                                                                                                                                                                                                                                        "Limite")) + scale_fill_manual(name = NULL,
                                                                                                                                                                                                                                                                                                                                                          values = fillCol, guide = FALSE)
      if (!is.null(y_axis_range)) {
        if (length(y_axis_range) != 2)
          stop("Please ensure that exactly two numbers are provided to 'y_axis_range' (e.g. c(10, 20)).")
        if (!is.numeric(y_axis_range[1]) | !is.numeric(y_axis_range[2]))
          stop("Please ensure that only numeric values are provided to 'y_axis_range'.")
        ep <- ep + coord_cartesian(ylim = c(y_axis_range[1],
                                            y_axis_range[2]))
      }
      ep
    }
  }

########################## Lolli Plot - Consulta #######################################

plot_lolli <- function(id){

  #event <- mhw$event %>% dplyr::select("intensity_max", "date_peak")
  event <- lolli_list[[id]]
  y_top <- as.numeric(event[which(abs(event[, 1]) == max(abs(event[,
                                                                   1])))[1], 1]) * 1.05

  if (y_top >= 0)
    y_limits <- c(0, y_top)
  if (y_top < 0)
    y_limits <- c(y_top, 0)

  lolli <- ggplot(data = event, aes_string(x = "date_peak", y = "intensity_max")) +
    geom_lolli(colour = "salmon", colour_n = "red",
               fill = "grey70", n = 3) + labs(x = "Pico da onda de calor",
                                              y = expression(paste("Intensidade Máxima [", degree,
                                                                   "C]"))) + scale_y_continuous(expand = c(0, 0), limits = y_limits) +
    theme(plot.background = element_blank(), panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA,
                                      size = 0.75), panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black",
                                          size = 0.2, linetype = "dotted"), axis.text = element_text(colour = "black"),
          axis.text.x = element_text(margin = unit(c(0.5,
                                                     0.5, 0.5, 0.5), "cm")), axis.text.y = element_text(margin = unit(c(0.5,
                                                                                                                        0.5, 0.5, 0.5), "cm")), axis.ticks.length = unit(-0.25,
                                                                                                                                                                         "cm"))

  lolli
}

########################### plot heat echarts
plot_heat_echarts <- function(data, x = "t", y = "temp", min_duration = 5, spread = 0,
                              metric = "intensity_cumulative", start_date = NULL, end_date = NULL,
                              category = FALSE, x_axis_title = NULL, x_axis_text_angle = NULL,
                              y_axis_title = NULL, y_axis_range = NULL) {

  if (!("event" %in% names(data)) | !("climatology" %in% names(data))) {
    stop("Please ensure you are running this function on the output of 'heatwaveR::detect_event()'")
  }

  if (is.null(start_date)) start_date <- min(data$climatology[[x]])
  if (is.null(end_date)) end_date <- max(data$climatology[[x]])

  event <- data$event %>%
    filter(date_end >= start_date & date_start <= end_date)

  if (nrow(event) == 0) {
    data_no <- data$climatology %>%
      filter(!!sym(x) >= start_date & !!sym(x) <= end_date)

    e_chart(data_no, !!sym(x)) %>%
      e_line(!!sym(y), name = "Temperatura", lineStyle = list(width = 1.5)) %>%
      e_line(thresh, name = "Limite", lineStyle = list(width = 1.5)) %>%
      e_line(seas, name = "Climatologia", lineStyle = list(width = 1.5)) %>%
      e_legend(right = "0%", top = "5%") %>%
      e_x_axis(name = x_axis_title, axisLabel = list(rotate = x_axis_text_angle)) %>%
      e_y_axis(name = y_axis_title, min = min(y_axis_range), max = max(y_axis_range))
  } else {
    event_idx <- as.vector(-abs(event[[metric]]))
    event <- event[order(event_idx), ]
    event <- event %>% filter(duration >= min_duration)

    date_spread <- seq((event$date_start - spread), (event$date_end + spread), by = "day")
    clim_diff <- data$climatology %>%
      mutate(diff = thresh - seas, thresh_2x = thresh + diff, thresh_3x = thresh_2x + diff, thresh_4x = thresh_3x + diff)

    if (category) {
      e_chart(clim_diff, !!sym(x)) %>%
        e_flame(!!sym(y), thresh_2x, name = "Strong") %>%
        e_flame(!!sym(y), thresh_3x, name = "Severe") %>%
        e_flame(!!sym(y), thresh_4x, name = "Extreme") %>%
        e_line(seas, name = "Climatologia", lineStyle = list(width = 1.5)) %>%
        e_line(thresh, name = "Limite", lineStyle = list(width = 1.5)) %>%
        e_line(!!sym(y), name = "Temperatura", lineStyle = list(width = 1.5)) %>%
        e_legend(right = "0%", top = "5%") %>%
        e_x_axis(name = x_axis_title, axisLabel = list(rotate = x_axis_text_angle)) %>%
        e_y_axis(name = y_axis_title, min = min(y_axis_range), max = max(y_axis_range))
    } else {
      e_chart(clim_diff, !!sym(x)) %>%
        e_flame(!!sym(y), thresh, name = "events") %>%
        e_line(seas, name = "Climatologia", lineStyle = list(width = 1.5)) %>%
        e_line(thresh, name = "Limite", lineStyle = list(width = 1.5)) %>%
        e_line(!!sym(y), name = "Temperatura", lineStyle = list(width = 1.5)) %>%
        e_legend(right = "0%", top = "5%") %>%
        e_x_axis(name = x_axis_title, axisLabel = list(rotate = x_axis_text_angle)) %>%
        e_y_axis(name = y_axis_title, min = min(y_axis_range), max = max(y_axis_range))
    }
  }
}
