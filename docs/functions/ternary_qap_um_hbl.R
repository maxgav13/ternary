ternary_qap_um_hbl = function(output = c('ggplot','plotly'), 
                              language = c('en','es'),
                              opacity = .5) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP_UM = tibble::tribble(
    ~Ol, ~Px, ~Hbl,                 ~Label.en,                   ~Label.es,
    100,    0,    0,                  "Dunite",                    "Dunita",
    90,   10,    0,                  "Dunite",                    "Dunita",
    90,    0,   10,                  "Dunite",                    "Dunita",
    100,    0,    0,                  "Dunite",                    "Dunita",
    90,   10,    0,             "Pyroxene peridotite",               "Peridotita piroxénica",
    40,   60,    0,             "Pyroxene peridotite",               "Peridotita piroxénica",
    40,   55,    5,             "Pyroxene peridotite",               "Peridotita piroxénica",
    90,    5,    5,             "Pyroxene peridotite",               "Peridotita piroxénica",
    90,   10,    0,             "Pyroxene peridotite",               "Peridotita piroxénica",
    90,    5,    5,              "Pyroxene hornblende peridotite",                "Peridotita piroxénica hornbléndica",
    40,   55,    5,              "Pyroxene hornblende peridotite",                "Peridotita piroxénica hornbléndica",
    40,    5,   55,              "Pyroxene hornblende peridotite",                "Peridotita piroxénica hornbléndica",
    90,    5,    5,              "Pyroxene hornblende peridotite",                "Peridotita piroxénica hornbléndica",
    90,    5,    5,                "Hornblende peridotite",                  "Peridotita hornbléndica",
    40,    5,   55,                "Hornblende peridotite",                  "Peridotita hornbléndica",
    40,    0,   60,                "Hornblende peridotite",                  "Peridotita hornbléndica",
    90,    0,   10,                "Hornblende peridotite",                  "Peridotita hornbléndica",
    90,    5,    5,                "Hornblende peridotite",                  "Peridotita hornbléndica",
    40,   60,    0, "Olivine pyroxenite",  "Piroxenita olivínica",
    10,   90,    0, "Olivine pyroxenite",  "Piroxenita olivínica",
    5,   90,    5, "Olivine pyroxenite",  "Piroxenita olivínica",
    40,   55,    5, "Olivine pyroxenite",  "Piroxenita olivínica",
    40,   60,    0, "Olivine pyroxenite",  "Piroxenita olivínica",
    40,   55,    5,      "Olivine hornblende pyroxenite",      "Piroxenita olivínica hornbléndica",
    5,   90,    5,      "Olivine hornblende pyroxenite",      "Piroxenita olivínica hornbléndica",
    5,    47.5,   47.5,      "Olivine hornblende pyroxenite",      "Piroxenita olivínica hornbléndica",
    40,    30,   30,      "Olivine hornblende pyroxenite",      "Piroxenita olivínica hornbléndica",
    40,   55,    5,      "Olivine hornblende pyroxenite",      "Piroxenita olivínica hornbléndica",
    40,    30,   30,      "Olivine pyroxene hornblendite",      "Hornblendita olivínica piroxénica",
    40, 5, 55,      "Olivine pyroxene hornblendite",      "Hornblendita olivínica piroxénica",
    5, 5, 90,       "Olivine pyroxene hornblendite",      "Hornblendita olivínica piroxénica",
    5, 47.5, 47.5,       "Olivine pyroxene hornblendite",      "Hornblendita olivínica piroxénica",
    40,    30,   30,      "Olivine pyroxene hornblendite",      "Hornblendita olivínica piroxénica",
    40,    5,   55, "Olivine hornblendite", "Hornblendita olivínica",
    5,    5,   90, "Olivine hornblendite", "Hornblendita olivínica",
    10,    0,   90, "Olivine hornblendite", "Hornblendita olivínica",
    40,    0,   60, "Olivine hornblendite", "Hornblendita olivínica",
    40,    5,   55, "Olivine hornblendite", "Hornblendita olivínica",
    10,   90,    0,         "Pyroxenite",            "Piroxenita",
    0,  100,    0,         "Pyroxenite",            "Piroxenita",
    0,   90,   10,         "Pyroxenite",            "Piroxenita",
    10,   90,    0,         "Pyroxenite",            "Piroxenita",
    5,   90,    5,              "Hornblende pyroxenite",                "Piroxenita hornbléndica",
    5,   47.5,   47.5,              "Hornblende pyroxenite",                "Piroxenita hornbléndica",
    0,   50,   50,              "Hornblende pyroxenite",                "Piroxenita hornbléndica",
    0,    90,   10,              "Hornblende pyroxenite",                "Piroxenita hornbléndica",
    5,   90,    5,              "Hornblende pyroxenite",                "Piroxenita hornbléndica",
    5, 47.5, 47.5, "Pyroxene hornblendite", "Hornblendita piroxénica",
    5, 5, 90,  "Pyroxene hornblendite", "Hornblendita piroxénica",
    0, 10, 90,  "Pyroxene hornblendite", "Hornblendita piroxénica",
    0, 50, 50,  "Pyroxene hornblendite", "Hornblendita piroxénica",
    5, 47.5, 47.5, "Pyroxene hornblendite", "Hornblendita piroxénica",
    10,    0,   90,         "Hornblendite",           "Hornblendita",
    0,   10,   90,         "Hornblendite",           "Hornblendita",
    0,    0,  100,         "Hornblendite",           "Hornblendita",
    10,    0,   90,         "Hornblendite",           "Hornblendita"
  ) %>% 
    dplyr::mutate(dplyr::across(Label.en:Label.es,forcats::as_factor))
  
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
  
  QAP_UM.ternaryAxes <- list(
    aaxis = axis("Ol"), 
    baxis = axis("Px"), 
    caxis = axis("Hbl")
  )
  
  QAP_UM.pal = viridisLite::viridis(12,direction = -1,option = 'D')
  
  if (any(output == 'ggplot' & language == 'en')) {
    QAP_UM <- ggtern::ggtern(data=tb.QAP_UM,ggtern::aes(Px,Ol,Hbl)) +
      ggplot2::geom_polygon(aes(fill=Label.en,color=Label.en,group=Label.en),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QAP_UM.pal) +
      ggplot2::scale_color_manual(values = QAP_UM.pal) +
      ggplot2::labs(fill="Ultramafic",
                    color="Ultramafic",
                    T="Ol",
                    L="Px",
                    R="Hbl")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_UM <- ggtern::ggtern(data=tb.QAP_UM,ggtern::aes(Px,Ol,Hbl)) +
      ggplot2::geom_polygon(aes(fill=Label.es,color=Label.es,group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual(values = QAP_UM.pal) +
      ggplot2::scale_color_manual(values = QAP_UM.pal) +
      ggplot2::labs(fill="Ultramáficas",
                    color="Ultramáficas",
                    T="Ol",
                    L="Px",
                    R="Hbl")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_UM = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~Ol, b = ~Px, c = ~Hbl, 
        color = ~Label.en,
        colors = QAP_UM.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = QAP_UM.ternaryAxes,
        legend = list(title=list(text='<b> Ultramafic </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Ultramafic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_UM = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~Ol, b = ~Px, c = ~Hbl, 
        color = ~Label.es,
        colors = QAP_UM.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        # line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = QAP_UM.ternaryAxes,
        legend = list(title=list(text='<b> Ultramáficas </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Ultramaficas',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(QAP_UM)
  
}
