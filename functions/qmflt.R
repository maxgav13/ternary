qmflt = function(output = c('ggplot','plotly'), 
                 language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QmFLt = tibble::tribble(
    ~Qm,     ~F,    ~Lt,                     ~Label,                ~Label.es,
    100,      0,      0,          "Craton interior",        "Cratón interior",
    80,     20,      0,          "Craton interior",        "Cratón interior",
    73.3,   13.8,   12.9,          "Craton interior",        "Cratón interior",
    89,      0,     11,          "Craton interior",        "Cratón interior",
    80,     20,      0, "Transitional Continental", "Transición continental",
    57,     43,      0, "Transitional Continental", "Transición continental",
    49.341, 34.377, 16.283, "Transitional Continental", "Transición continental",
    68.151, 18.133, 13.717, "Transitional Continental", "Transición continental",
    73.3,   13.8,   12.9, "Transitional Continental", "Transición continental",
    57,     43,      0,          "Basement Uplift",      "Basemento elevado",
    0,    100,      0,          "Basement Uplift",      "Basemento elevado",
    0,     77,     23,          "Basement Uplift",      "Basemento elevado",
    22.199,  57.56, 20.242,          "Basement Uplift",      "Basemento elevado",
    49.341, 34.377, 16.283,          "Basement Uplift",      "Basemento elevado",
    89,      0,     11,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    73.3,   13.8,   12.9,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    68.151, 18.133, 13.717,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    50.273, 16.786, 32.941,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    58,      0,     42,       "Quartzose Recycled",     "Reciclaje cuarzoso",
    58,      0,     42,    "Transitional Recycled",  "Reciclaje transiconal",
    50.273, 16.786, 32.941,    "Transitional Recycled",  "Reciclaje transiconal",
    29.66, 15.191, 55.148,    "Transitional Recycled",  "Reciclaje transiconal",
    21.639, 14.544, 63.817,    "Transitional Recycled",  "Reciclaje transiconal",
    29,      0,     71,    "Transitional Recycled",  "Reciclaje transiconal",
    29,      0,     71,          "Lithic Recycled",       "Reciclaje lítico",
    21.639, 14.544, 63.817,          "Lithic Recycled",       "Reciclaje lítico",
    12.685, 13.932, 73.383,          "Lithic Recycled",       "Reciclaje lítico",
    0,     13,     87,          "Lithic Recycled",       "Reciclaje lítico",
    0,      0,    100,          "Lithic Recycled",       "Reciclaje lítico",
    68.151, 18.133, 13.717,                    "Mixed",                 "Mezcla",
    49.341, 34.377, 16.283,                    "Mixed",                 "Mezcla",
    29.66, 15.191, 55.148,                    "Mixed",                 "Mezcla",
    50.273, 16.786, 32.941,                    "Mixed",                 "Mezcla",
    49.341, 34.377, 16.283,            "Dissected Arc",         "Arco disectado",
    22.199,  57.56, 20.242,            "Dissected Arc",         "Arco disectado",
    29.66, 15.191, 55.148,            "Dissected Arc",         "Arco disectado",
    22.199,  57.56, 20.242,         "Transitional Arc",      "Arco transicional",
    0,     77,     23,         "Transitional Arc",      "Arco transicional",
    0,     47,     53,         "Transitional Arc",      "Arco transicional",
    12.685, 13.932, 73.383,         "Transitional Arc",      "Arco transicional",
    21.639, 14.544, 63.817,         "Transitional Arc",      "Arco transicional",
    29.66, 15.191, 55.148,         "Transitional Arc",      "Arco transicional",
    0,     47,     53,          "Undissected Arc",      "Arco no disectado",
    0,     13,     87,          "Undissected Arc",      "Arco no disectado",
    12.685, 13.932, 73.383,          "Undissected Arc",      "Arco no disectado"
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
  
  QmFLt.ternaryAxes <- list(
    aaxis = axis("Qm"), 
    baxis = axis("F"), 
    caxis = axis("Lt")
  )
  
  QmFLt.pal = c("#989FA7", "#5A9AE1", "#2F4996", 
                "#9EA76E", "#6B6943", "#564E37", 
                "#D4E3D0",  
                "#BFAED2", "#DAB7A3", "#B21C3F")
  
  if (any(output == 'ggplot' & language == 'en')) {
    QmFLt <- ggtern::ggtern(data=tb.QmFLt,ggtern::aes(F,Qm,Lt)) +
      ggplot2::geom_polygon(aes(fill=Label,group=Label),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QmFLt.pal) +
      ggplot2::labs(title="QmFLt",
                    fill = "Provenance",
                    T="Qm",
                    L="F",
                    R="Lt")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QmFLt <- ggtern::ggtern(data=tb.QmFLt,ggtern::aes(F,Qm,Lt)) +
      ggplot2::geom_polygon(aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QmFLt.pal) +
      ggplot2::labs(title="QmFLt",
                    fill = "Proveniencia",
                    T="Qm",
                    L="F",
                    R="Lt")
  } else if (any(output == 'plotly' & language == 'en')) {
    QmFLt = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QmFLt,
        a = ~Qm, b = ~F, c = ~Lt, 
        color = ~Label, 
        colors = QmFLt.pal %>% purrr::set_names(levels(tb.QmFLt$Label)),
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("QmFLt"), ternary = QmFLt.ternaryAxes,
        legend = list(title=list(text='<b> Provenance </b>'))
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QmFLt',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QmFLt = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QmFLt,
        a = ~Qm, b = ~F, c = ~Lt, 
        color = ~Label.es, 
        colors = QmFLt.pal %>% purrr::set_names(levels(tb.QmFLt$Label.es)),
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("QmFLt"), ternary = QmFLt.ternaryAxes,
        legend = list(title=list(text='<b> Proveniencia </b>'))
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'QmFLt',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(QmFLt)
  
}
