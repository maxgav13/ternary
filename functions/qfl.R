qfl = function(output = c('ggplot','plotly'), 
               language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QFL = tibble::tribble(
    ~Qt,     ~F,     ~L,                     ~Label,                ~Label.es,
    100,      0,      0,          "Craton interior",        "Cratón interior",
    82,     18,      0,          "Craton interior",        "Cratón interior",
    80,     15,      5,          "Craton interior",        "Cratón interior",
    97,      0,      3,          "Craton interior",        "Cratón interior",
    82,     18,      0, "Transitional Continental", "Transición continental",
    55,     45,      0, "Transitional Continental", "Transición continental",
    52,     40,      8, "Transitional Continental", "Transición continental",
    80,     15,      5, "Transitional Continental", "Transición continental",
    55,     45,      0,          "Basement Uplift",      "Basamento elevado",
    0,    100,      0,          "Basement Uplift",      "Basamento elevado",
    0,     85,     15,          "Basement Uplift",      "Basamento elevado",
    20,   67.7,   12.3,          "Basement Uplift",      "Basamento elevado",
    52,     40,      8,          "Basement Uplift",      "Basamento elevado",
    97,      0,      3,        "Recycled Orogenic",      "Orógeno reciclado",
    80,     15,      5,        "Recycled Orogenic",      "Orógeno reciclado",
    52,     40,      8,        "Recycled Orogenic",      "Orógeno reciclado",
    33.781, 12.897, 53.322,        "Recycled Orogenic",      "Orógeno reciclado",
    25,      0,     75,        "Recycled Orogenic",      "Orógeno reciclado",
    52,     40,      8,            "Dissected Arc",         "Arco disectado",
    20,   67.7,   12.3,            "Dissected Arc",         "Arco disectado",
    33.781, 12.897, 53.322,            "Dissected Arc",         "Arco disectado",
    20,   67.7,   12.3,         "Transitional Arc",      "Arco transicional",
    0,     85,     15,         "Transitional Arc",      "Arco transicional",
    0,     50,     50,         "Transitional Arc",      "Arco transicional",
    25,      0,     75,         "Transitional Arc",      "Arco transicional",
    25,      0,     75,         "Transitional Arc",      "Arco transicional",
    33.781, 12.897, 53.322,         "Transitional Arc",      "Arco transicional",
    25,      0,     75,          "Undissected Arc",      "Arco no disectado",
    0,     50,     50,          "Undissected Arc",      "Arco no disectado",
    0,      0,    100,          "Undissected Arc",      "Arco no disectado"
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
      font = list(family = "serif", size = 15, color = "white"),
      bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
    )
  }
  
  # reusable function for axis formatting
  axis <- function(txt) {
    list(
      title = txt, tickformat = ".0%", tickfont = list(size = 10)
    )
  }
  
  QFL.ternaryAxes <- list(
    aaxis = axis("Qt"), 
    baxis = axis("F"), 
    caxis = axis("L")
  )
  
  QFL.pal = c("#989FA7", "#5A9AE1", "#2F4996", 
              "#6B6943", 
              "#BFAED2", "#DAB7A3", "#B21C3F")
  
  if (any(output == 'ggplot' & language == 'en')) {
    QFL <- ggtern::ggtern(data=tb.QFL,ggtern::aes(F,Qt,L)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label,group=Label),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QFL.pal) +
      ggplot2::labs(title="QtFL",
                    fill = "Provenance",
                    T="Qt",
                    L="F",
                    R="L")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QFL <- ggtern::ggtern(data=tb.QFL,ggtern::aes(F,Qt,L)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QFL.pal) +
      ggplot2::labs(title="QtFL",
                    fill = "Proveniencia",
                    T="Qt",
                    L="F",
                    R="L")
  } else if (any(output == 'plotly' & language == 'en')) {
    QFL = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QFL,
        a = ~Qt, b = ~F, c = ~L, 
        color = ~Label, 
        colors = QFL.pal %>% purrr::set_names(levels(tb.QFL$Label)),
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("QtFL"), ternary = QFL.ternaryAxes,
        legend = list(title=list(text='<b> Provenance </b>'))
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QtFL',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QFL = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QFL,
        a = ~Qt, b = ~F, c = ~L, 
        color = ~Label.es, 
        colors = QFL.pal %>% purrr::set_names(levels(tb.QFL$Label.es)),
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("QtFL"), ternary = QFL.ternaryAxes,
        legend = list(title=list(text='<b> Proveniencia </b>'))
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QtFL',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(QFL)
  
}
