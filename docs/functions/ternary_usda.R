ternary_usda = function(output = c('ggplot','plotly'), 
                        language = c('en','es'),
                        opacity = .5) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.usda = tibble::tribble(
    ~Clay, ~Sand, ~Silt,            ~Label,
    1,     0,     0,            "Clay",
    0.55,  0.45,     0,            "Clay",
    0.4,  0.45,  0.15,            "Clay",
    0.4,   0.2,   0.4,            "Clay",
    0.6,     0,   0.4,            "Clay",
    1,     0,     0,            "Clay",
    0.55,  0.45,     0,      "Sandy Clay",
    0.35,  0.65,     0,      "Sandy Clay",
    0.35,  0.45,   0.2,      "Sandy Clay",
    0.55,  0.45,     0,      "Sandy Clay",
    0.35,  0.65,     0, "Sandy Clay Loam",
    0.2,   0.8,     0, "Sandy Clay Loam",
    0.2, 0.525, 0.275, "Sandy Clay Loam",
    0.275,  0.45, 0.275, "Sandy Clay Loam",
    0.35,  0.45,   0.2, "Sandy Clay Loam",
    0.35,  0.65,     0, "Sandy Clay Loam",
    0.2,   0.8,     0,      "Sandy Loam",
    0.15,  0.85,     0,      "Sandy Loam",
    0,   0.7,   0.3,      "Sandy Loam",
    0,   0.5,   0.5,      "Sandy Loam",
    0.05,  0.45,   0.5,      "Sandy Loam",
    0.05, 0.525, 0.425,      "Sandy Loam",
    0.2, 0.525, 0.275,      "Sandy Loam",
    0.2,   0.8,     0,      "Sandy Loam",
    0.15,  0.85,     0,      "Loamy Sand",
    0.1,   0.9,     0,      "Loamy Sand",
    0,  0.85,  0.15,      "Loamy Sand",
    0,   0.7,   0.3,      "Loamy Sand",
    0.15,  0.85,     0,      "Loamy Sand",
    0.1,   0.9,     0,            "Sand",
    0,     1,     0,            "Sand",
    0,  0.85,  0.15,            "Sand",
    0.1,   0.9,     0,            "Sand",
    0.4,  0.45,  0.15,       "Clay Loam",
    0.275,  0.45, 0.275,       "Clay Loam",
    0.275,   0.2, 0.525,       "Clay Loam",
    0.4,   0.2,   0.4,       "Clay Loam",
    0.4,  0.45,  0.15,       "Clay Loam",
    0.275,  0.45, 0.275,            "Loam",
    0.2, 0.525, 0.275,            "Loam",
    0.05, 0.525, 0.425,            "Loam",
    0.05,  0.45,   0.5,            "Loam",
    0.275, 0.225,   0.5,            "Loam",
    0.275,  0.45, 0.275,            "Loam",
    0.275, 0.225,   0.5,       "Silt Loam",
    0,   0.5,   0.5,       "Silt Loam",
    0,   0.2,   0.8,       "Silt Loam",
    0.125, 0.075,   0.8,       "Silt Loam",
    0.125,     0, 0.875,       "Silt Loam",
    0.275,     0, 0.725,       "Silt Loam",
    0.275, 0.225,   0.5,       "Silt Loam",
    0.6,     0,   0.4,      "Silty Clay",
    0.4,   0.2,   0.4,      "Silty Clay",
    0.4,     0,   0.6,      "Silty Clay",
    0.6,     0,   0.4,      "Silty Clay",
    0.4,   0.2,   0.4, "Silty Clay Loam",
    0.275,   0.2, 0.525, "Silty Clay Loam",
    0.275,     0, 0.725, "Silty Clay Loam",
    0.4,     0,   0.6, "Silty Clay Loam",
    0.4,   0.2,   0.4, "Silty Clay Loam",
    0.125, 0.075,   0.8,            "Silt",
    0,   0.2,   0.8,            "Silt",
    0,     0,     1,            "Silt",
    0.125,     0, 0.875,            "Silt",
    0.125, 0.075,   0.8,            "Silt"
  ) %>% 
    dplyr::mutate(Label.es = 
                    dplyr::case_when(
                      Label == 'Clay' ~ "Arcilla",
                      Label == 'Sandy Clay' ~ "Arcilla arenosa",
                      Label == 'Sandy Clay Loam' ~ "Franco arcillo arenoso",
                      Label == 'Sandy Loam' ~ "Franco arenoso",
                      Label == 'Loamy Sand' ~ "Arena franca",
                      Label == 'Sand' ~ "Arena",
                      Label == 'Clay Loam' ~ "Franco arcilloso",
                      Label == 'Loam' ~ "Franco",
                      Label == 'Silt Loam' ~ "Franco limoso",
                      Label == 'Silty Clay' ~ "Arcilla limosa",
                      Label == 'Silty Clay Loam' ~ "Franco arcillo limoso",
                      Label == 'Silt' ~ "Limo"),
                  dplyr::across(Label:Label.es,forcats::as_factor))
  
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
  
  usda.ternaryAxes.en <- list(
    aaxis = axis("Clay"), 
    baxis = axis("Sand"), 
    caxis = axis("Silt")
  )
  
  usda.ternaryAxes.es <- list(
    aaxis = axis("Arcilla"), 
    baxis = axis("Arena"), 
    caxis = axis("Limo")
  )

  
  if (any(output == 'ggplot' & language == 'en')) {
    usda <- ggtern::ggtern(data=tb.usda,ggtern::aes(Sand,Clay,Silt)) +
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
    usda <- ggtern::ggtern(data=tb.usda,ggtern::aes(Sand,Clay,Silt)) +
      ggplot2::geom_polygon(aes(fill=Label.es,color=Label.es,group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set3') + 
      ggplot2::scale_color_brewer(palette = 'Set3') + 
      ggplot2::labs(fill = "Suelo",
                    color = "Suelo",
                    T="Arcilla",
                    L="Arena",
                    R="Limo")
  } else if (any(output == 'plotly' & language == 'en')) {
    usda = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.usda,
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
        ternary = usda.ternaryAxes.en,
        legend = list(title=list(text='<b> Soil type </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'usda',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    usda = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.usda,
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
        ternary = usda.ternaryAxes.es,
        legend = list(title=list(text='<b> Tipo de suelo </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'usda',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(usda)
  
}
