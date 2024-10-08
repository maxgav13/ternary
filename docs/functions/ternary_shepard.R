ternary_shepard = function(output = c('ggplot','plotly'), 
                           language = c('en','es'),
                           opacity = .5) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.Shepard = tibble::tribble(
    ~Clay, ~Sand, ~Silt,               ~Label,                ~Label.es,
    1,     0,     0,               "Clay",                "Arcilla",
    0.75,  0.25,     0,               "Clay",                "Arcilla",
    0.75,     0,  0.25,               "Clay",                "Arcilla",
    1,     0,     0,               "Clay",                "Arcilla",
    0.5,   0.5,     0,         "Sandy Clay",        "Arcilla arenosa",
    0.75,  0.25,     0,         "Sandy Clay",        "Arcilla arenosa",
    0.75, 0.125, 0.125,         "Sandy Clay",        "Arcilla arenosa",
    0.6,   0.2,   0.2,         "Sandy Clay",        "Arcilla arenosa",
    0.4,   0.4,   0.2,         "Sandy Clay",        "Arcilla arenosa",
    0.5,   0.5,     0,         "Sandy Clay",        "Arcilla arenosa",
    0.75, 0.125, 0.125,         "Silty Clay",         "Arcilla limosa",
    0.75,     0,  0.25,         "Silty Clay",         "Arcilla limosa",
    0.5,     0,   0.5,         "Silty Clay",         "Arcilla limosa",
    0.4,   0.2,   0.4,         "Silty Clay",         "Arcilla limosa",
    0.6,   0.2,   0.2,         "Silty Clay",         "Arcilla limosa",
    0.75, 0.125, 0.125,         "Silty Clay",         "Arcilla limosa",
    0.6,   0.2,   0.2, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.2,   0.2,   0.6, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.2,   0.6,   0.2, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.6,   0.2,   0.2, "Sand + Silt + Clay", "Arena + Limo + Arcilla",
    0.5,   0.5,     0,        "Clayey Sand",        "Arena arcillosa",
    0.4,   0.4,   0.2,        "Clayey Sand",        "Arena arcillosa",
    0.2,   0.6,   0.2,        "Clayey Sand",        "Arena arcillosa",
    0.125,  0.75, 0.125,        "Clayey Sand",        "Arena arcillosa",
    0.25,  0.75,     0,        "Clayey Sand",        "Arena arcillosa",
    0.5,   0.5,     0,        "Clayey Sand",        "Arena arcillosa",
    0.5,     0,   0.5,        "Clayey Silt",         "Limo arcilloso",
    0.25,     0,  0.75,        "Clayey Silt",         "Limo arcilloso",
    0.125, 0.125,  0.75,        "Clayey Silt",         "Limo arcilloso",
    0.2,   0.2,   0.6,        "Clayey Silt",         "Limo arcilloso",
    0.4,   0.2,   0.4,        "Clayey Silt",         "Limo arcilloso",
    0.5,     0,   0.5,        "Clayey Silt",         "Limo arcilloso",
    0,     1,     0,               "Sand",                  "Arena",
    0.25,  0.75,     0,               "Sand",                  "Arena",
    0,  0.75,  0.25,               "Sand",                  "Arena",
    0,     1,     0,               "Sand",                  "Arena",
    0.125,  0.75, 0.125,         "Silty Sand",           "Arena limosa",
    0.2,   0.6,   0.2,         "Silty Sand",           "Arena limosa",
    0.2,   0.4,   0.4,         "Silty Sand",           "Arena limosa",
    0,   0.5,   0.5,         "Silty Sand",           "Arena limosa",
    0,  0.75,  0.25,         "Silty Sand",           "Arena limosa",
    0.125,  0.75, 0.125,         "Silty Sand",           "Arena limosa",
    0.2,   0.4,   0.4,         "Sandy Silt",           "Limo arenoso",
    0.2,   0.2,   0.6,         "Sandy Silt",           "Limo arenoso",
    0.125, 0.125,  0.75,         "Sandy Silt",           "Limo arenoso",
    0,  0.25,  0.75,         "Sandy Silt",           "Limo arenoso",
    0,   0.5,   0.5,         "Sandy Silt",           "Limo arenoso",
    0.2,   0.4,   0.4,         "Sandy Silt",           "Limo arenoso",
    0.25,     0,  0.75,               "Silt",                   "Limo",
    0,     0,     1,               "Silt",                   "Limo",
    0,  0.25,  0.75,               "Silt",                   "Limo",
    0.25,     0,  0.75,               "Silt",                   "Limo"
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
  
  Shepard.ternaryAxes.en <- list(
    aaxis = axis("Clay"), 
    baxis = axis("Sand"), 
    caxis = axis("Silt")
  )
  
  Shepard.ternaryAxes.es <- list(
    aaxis = axis("Arcilla"), 
    baxis = axis("Arena"), 
    caxis = axis("Limo")
  )

  
  if (any(output == 'ggplot' & language == 'en')) {
    Shepard <- ggtern::ggtern(data=tb.Shepard,ggtern::aes(Sand,Clay,Silt)) +
      ggplot2::geom_polygon(aes(fill=Label,color=Label,group=Label),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') + 
      ggplot2::scale_color_brewer(palette = 'Set3') + 
      ggplot2::labs(fill = "Soil",
                    color = "Soil",
                    T="Clay",
                    L="Sand",
                    R="Silt")
  } else if (any(output == 'ggplot' & language == 'es')) {
    Shepard <- ggtern::ggtern(data=tb.Shepard,ggtern::aes(Sand,Clay,Silt)) +
      ggplot2::geom_polygon(aes(fill=Label.es,color=Label.es,group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') + 
      ggplot2::scale_color_brewer(palette = 'Set3') + 
      ggplot2::labs(fill = "Suelo",
                    color = 'Suelo',
                    T="Arcilla",
                    L="Arena",
                    R="Limo")
  } else if (any(output == 'plotly' & language == 'en')) {
    Shepard = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Shepard,
        a = ~Clay, b = ~Sand, c = ~Silt, 
        color = ~Label, 
        colors = 'Set3',
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Shepard.ternaryAxes.en,
        legend = list(title=list(text='<b> Soil type </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Shepard',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    Shepard = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.Shepard,
        a = ~Clay, b = ~Sand, c = ~Silt, 
        color = ~Label.es, 
        colors = 'Set3',
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = Shepard.ternaryAxes.es,
        legend = list(title=list(text='<b> Tipo de suelo </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Shepard',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(Shepard)
  
}
