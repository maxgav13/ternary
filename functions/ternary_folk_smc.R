ternary_folk_smc = function(output = c('ggplot','plotly'), 
                            language = c('en','es'),
                            opacity = .5) {
  
  library(magrittr)
  
  tb.Folk = tibble::tribble(
    ~S,    ~M,    ~C,                    ~Label,                 ~Label.es,
    100,     0,     0,           "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    90,     10,     0,           "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    90,      0,    10,           "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    100,     0,     0,           "Sandstone \n(Sand)",      "Arenisca \n(Arena)",
    90,     10,     0,     "Silty sandstone \n(Silty sand)",    "Arenisca limosa \n(Arena limosa)",
    90,     6.5,    3.5,     "Silty sandstone \n(Silty sand)",    "Arenisca limosa \n(Arena limosa)",
    50,     33.4,  16.6,     "Silty sandstone \n(Silty sand)",    "Arenisca limosa \n(Arena limosa)",
    50,     50,     0,     "Silty sandstone \n(Silty sand)",    "Arenisca limosa \n(Arena limosa)",
    90,     10,     0,     "Silty sandstone \n(Silty sand)",    "Arenisca limosa \n(Arena limosa)",
    90,     6.5,    3.5,   "Muddy sandstone \n(Muddy sand)",           "Arenisca lodosa \n(Arena lodosa)",
    90,     3.5,    6.5,          "Muddy sandstone \n(Muddy sand)",           "Arenisca lodosa \n(Arena lodosa)",
    50,     16.6,    33.4,          "Muddy sandstone \n(Muddy sand)",           "Arenisca lodosa \n(Arena lodosa)",
    50,     33.4,  16.6,          "Muddy sandstone \n(Muddy sand)",           "Arenisca lodosa \n(Arena lodosa)",
    90,     6.5,    3.5,          "Muddy sandstone \n(Muddy sand)",           "Arenisca lodosa \n(Arena lodosa)",
    90,     3.5,    6.5,       "Clayey sandstone \n(Clayey sand)",                  "Arenisca arcillosa \n(Arena arcillosa)",
    90,     0,     10,                  "Clayey sandstone \n(Clayey sand)",                  "Arenisca arcillosa \n(Arena arcillosa)",
    50,    0,    50,                  "Clayey sandstone \n(Clayey sand)",                  "Arenisca arcillosa \n(Arena arcillosa)",
    50,     16.6,    33.4,                  "Clayey sandstone \n(Clayey sand)",                  "Arenisca arcillosa \n(Arena arcillosa)",
    90,     3.5,    6.5,                  "Clayey sandstone \n(Clayey sand)",                  "Arenisca arcillosa \n(Arena arcillosa)",
    50,     50,     0,           "Sandy siltstone \n(Sandy silt)",           "Limolita arenosa \n(Limo arenoso)",
    50,     33.4,  16.6,           "Sandy siltstone \n(Sandy silt)",           "Limolita arenosa \n(Limo arenoso)",
    10,    60,    30,           "Sandy siltstone \n(Sandy silt)",           "Limolita arenosa \n(Limo arenoso)",
    10,    90,    0,           "Sandy siltstone \n(Sandy silt)",           "Limolita arenosa \n(Limo arenoso)",
    50,     50,     0,           "Sandy siltstone \n(Sandy silt)",           "Limolita arenosa \n(Limo arenoso)",
    50,     33.4,  16.6, "Sandy mudstone \n(Sandy mud)", "Lodolita arenosa \n(Lodo arenoso)",
    50,     16.6,    33.4, "Sandy mudstone \n(Sandy mud)", "Lodolita arenosa \n(Lodo arenoso)",
    10,    30,    60, "Sandy mudstone \n(Sandy mud)", "Lodolita arenosa \n(Lodo arenoso)",
    10,    60,    30, "Sandy mudstone \n(Sandy mud)", "Lodolita arenosa \n(Lodo arenoso)",
    50,     33.4,  16.6, "Sandy mudstone \n(Sandy mud)", "Lodolita arenosa \n(Lodo arenoso)",
    50,     16.6,    33.4,             "Sandy claystone \n(Sandy clay)",              "Arcillolita arenosa \n(Arcilla arenosa)",
    50,    0,    50,             "Sandy claystone \n(Sandy clay)",              "Arcillolita arenosa \n(Arcilla arenosa)",
    10,    0,    90,             "Sandy claystone \n(Sandy clay)",              "Arcillolita arenosa \n(Arcilla arenosa)",
    10,    30,    60,             "Sandy claystone \n(Sandy clay)",              "Arcillolita arenosa \n(Arcilla arenosa)",
    50,     16.6,    33.4,             "Sandy claystone \n(Sandy clay)",              "Arcillolita arenosa \n(Arcilla arenosa)",
    10,    90,    0,    "Siltstone \n(Silt)",      "Limolita \n(Limo)",
    10,    60,    30,    "Siltstone \n(Silt)",      "Limolita \n(Limo)",
    0,    66.7,    33.3,    "Siltstone \n(Silt)",      "Limolita \n(Limo)",
    0,    100,    0,    "Siltstone \n(Silt)",      "Limolita \n(Limo)",
    10,    90,    0,    "Siltstone \n(Silt)",      "Limolita \n(Limo)",
    10,    60,    30,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    10,    30,    60,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    0,    33.3,   66.7,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    0,    66.7,    33.3,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    10,    60,    30,    "Mudstone \n(Mud)",      "Lodolita \n(Lodo)",
    10,    30,    60,    "Claystone \n(Clay)",      "Arcillolita \n(Arcilla)",
    10,    0,    90,    "Claystone \n(Clay)",      "Arcillolita \n(Arcilla)",
    0,    0,    100,    "Claystone \n(Clay)",      "Arcillolita \n(Arcilla)",
    0,    33.3,   66.7,    "Claystone \n(Clay)",      "Arcillolita \n(Arcilla)",
    10,    30,    60,    "Claystone \n(Clay)",      "Arcillolita \n(Arcilla)"
  ) %>% 
    dplyr::mutate(dplyr::across(Label:Label.es,forcats::as_factor))
  
  
  # reusable function for creating annotation object
  label <- function(txt) {
    list(
      text = txt, 
      x = 0.1, y = 1,
      ax = 0, ay = 0,
      xref = "paper", yref = "paper", 
      align = "center",
      font = list(size = 15, color = "white"),
      bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
    )
  }
  
  # reusable function for axis formatting
  axis <- function(txt) {
    list(
      title = txt, tickformat = ".0%", tickfont = list(size = 10)
    )
  }
  
  Folk.ternaryAxes.en <- list(
    aaxis = axis("Sand"), 
    baxis = axis("Silt"), 
    caxis = axis("Clay")
  )
  
  Folk.ternaryAxes.es <- list(
    aaxis = axis("Arena"), 
    baxis = axis("Limo"), 
    caxis = axis("Arcilla")
  )
  
  Folk.pal = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,'OrRd'))(10)
  
  if (any(output == 'ggplot' & language == 'en')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(M,S,C)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label,color=Label,
                                         group=Label),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(fill = "Rock (sediment)",
                    color = "Rock (sediment)",
                    T="Sand",
                    L="Silt",
                    R="Clay")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Folk <- ggtern::ggtern(data=tb.Folk,ggtern::aes(M,S,C)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,color=Label.es,
                                         group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = Folk.pal) +
      ggplot2::scale_color_manual(values = Folk.pal) +
      ggplot2::labs(fill = "Roca (sedimento)",
                    color = "Roca (sedimento)",
                    T="Arena",
                    L="Limo",
                    R="Arcilla")
  } else if (any(output == 'plotly' & language == 'en')) {
    Folk = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Folk,
        a = ~S, b = ~M, c = ~C, 
        color = ~Label, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Folk.ternaryAxes.en,
        legend = list(title=list(text='<b> Rock (sediment) </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Folk = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Folk,
        a = ~S, b = ~M, c = ~C, 
        color = ~Label.es, 
        colors = Folk.pal %>% purrr::set_names(levels(tb.Folk$Label.es)),
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Folk.ternaryAxes.es,
        legend = list(title=list(text='<b> Roca (sedimento) </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Folk',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(Folk)
  
}
