ternary_qap_g = function(output = c('ggplot','plotly'), 
                 language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP_G = tibble::tribble(
    ~P, ~Cpx, ~Opx,              ~Label.en,                ~Label.es,
    100,    0,    0,          "Anorthosite",             "Anortosita",
    90,   10,    0,          "Anorthosite",             "Anortosita",
    90,    0,   10,          "Anorthosite",             "Anortosita",
    90,   10,    0,               "Gabbro",                  "Gabro",
    10,   90,    0,               "Gabbro",                  "Gabro",
    10,   85,    5,               "Gabbro",                  "Gabro",
    90,    5,    5,               "Gabbro",                  "Gabro",
    90,    5,    5, "Orthopyroxene gabbro",   "Gabro ortopiroxénico",
    10,   85,    5, "Orthopyroxene gabbro",   "Gabro ortopiroxénico",
    10,   45,   45, "Orthopyroxene gabbro",   "Gabro ortopiroxénico",
    90,    5,    5, "Clinopyroxene norite", "Norita clinopiroxénica",
    10,   45,   45, "Clinopyroxene norite", "Norita clinopiroxénica",
    10,    5,   85, "Clinopyroxene norite", "Norita clinopiroxénica",
    90,    5,    5,               "Norite",                 "Norita",
    10,    5,   85,               "Norite",                 "Norita",
    10,    0,   90,               "Norite",                 "Norita",
    90,    0,   10,               "Norite",                 "Norita",
    10,   90,    0,      "Clinopyroxenite",        "Clinopiroxenita",
    0,  100,    0,      "Clinopyroxenite",        "Clinopiroxenita",
    0,   90,   10,      "Clinopyroxenite",        "Clinopiroxenita",
    10,   90,    0,           "Websterite",             "Websterita",
    0,   90,   10,           "Websterite",             "Websterita",
    0,   10,   90,           "Websterite",             "Websterita",
    10,    0,   90,           "Websterite",             "Websterita",
    10,    0,   90,      "Orthopyroxenite",         "Ortopiroxenita",
    0,   10,   90,      "Orthopyroxenite",         "Ortopiroxenita",
    0,    0,  100,      "Orthopyroxenite",         "Ortopiroxenita"
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
  
  QAP_G.ternaryAxes <- list(
    aaxis = axis("P"), 
    baxis = axis("Cpx"), 
    caxis = axis("Opx")
  )
  
  QAP_G.pal = viridisLite::viridis(8,direction = -1,option = 'G')
  
  if (any(output == 'ggplot' & language == 'en')) {
    QAP_G <- ggtern::ggtern(data=tb.QAP_G,ggtern::aes(Cpx,P,Opx)) +
      ggplot2::geom_polygon(aes(fill=Label.en,group=Label.en),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('',values = QAP_G.pal) +
      ggplot2::labs(title="Gabbroic",
                    T="P",
                    L="Cpx",
                    R="Opx")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_G <- ggtern::ggtern(data=tb.QAP_G,ggtern::aes(Cpx,P,Opx)) +
      ggplot2::geom_polygon(aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('',values = QAP_G.pal) +
      ggplot2::labs(title="Gabros",
                    T="P",
                    L="Cpx",
                    R="Opx")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_G = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_G,
        a = ~P, b = ~Cpx, c = ~Opx, 
        color = ~Label.en,
        colors = QAP_G.pal,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Gabbroic"), ternary = QAP_G.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Gabbroic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_G = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_G,
        a = ~P, b = ~Cpx, c = ~Opx, 
        color = ~Label.es,
        colors = QAP_G.pal,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Gabros"), ternary = QAP_G.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Gabros',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(QAP_G)
  
}
