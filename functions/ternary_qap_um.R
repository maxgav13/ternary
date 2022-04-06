ternary_qap_um = function(output = c('ggplot','plotly'), 
                 language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP_UM = tibble::tribble(
    ~Ol, ~Opx, ~Cpx,                 ~Label.en,                   ~Label.es,
    100,    0,    0,                  "Dunite",                    "Dunita",
    90,   10,    0,                  "Dunite",                    "Dunita",
    90,    0,   10,                  "Dunite",                    "Dunita",
    90,   10,    0,             "Harzburgite",               "Harzburgita",
    40,   60,    0,             "Harzburgite",               "Harzburgita",
    40,   55,    5,             "Harzburgite",               "Harzburgita",
    90,    5,    5,             "Harzburgite",               "Harzburgita",
    90,    5,    5,              "Lherzolite",                "Lherzolita",
    40,   55,    5,              "Lherzolite",                "Lherzolita",
    40,    5,   55,              "Lherzolite",                "Lherzolita",
    90,    5,    5,                "Wehrlite",                  "Wehrlita",
    40,    5,   55,                "Wehrlite",                  "Wehrlita",
    40,    0,   60,                "Wehrlite",                  "Wehrlita",
    90,    0,   10,                "Wehrlite",                  "Wehrlita",
    40,   60,    0, "Olivine orthopyroxenite",  "Ortopiroxenita olivínica",
    10,   90,    0, "Olivine orthopyroxenite",  "Ortopiroxenita olivínica",
    5,   90,    5, "Olivine orthopyroxenite",  "Ortopiroxenita olivínica",
    40,   55,    5, "Olivine orthopyroxenite",  "Ortopiroxenita olivínica",
    40,   55,    5,      "Olivine websterite",      "Websterita olivínica",
    5,   90,    5,      "Olivine websterite",      "Websterita olivínica",
    5,    5,   90,      "Olivine websterite",      "Websterita olivínica",
    40,    5,   55,      "Olivine websterite",      "Websterita olivínica",
    40,    5,   55, "Olivine clinopyroxenite", "Clinopiroxenita olivínica",
    5,    5,   90, "Olivine clinopyroxenite", "Clinopiroxenita olivínica",
    10,    0,   90, "Olivine clinopyroxenite", "Clinopiroxenita olivínica",
    40,    0,   60, "Olivine clinopyroxenite", "Clinopiroxenita olivínica",
    10,   90,    0,         "Orthopyroxenite",            "Ortopiroxenita",
    0,  100,    0,         "Orthopyroxenite",            "Ortopiroxenita",
    0,   90,   10,         "Orthopyroxenite",            "Ortopiroxenita",
    5,   90,    5,              "Websterite",                "Websterita",
    0,   90,   10,              "Websterite",                "Websterita",
    0,   10,   90,              "Websterite",                "Websterita",
    5,    5,   90,              "Websterite",                "Websterita",
    10,    0,   90,         "Clinopyroxenite",           "Clinopiroxenita",
    0,   10,   90,         "Clinopyroxenite",           "Clinopiroxenita",
    0,    0,  100,         "Clinopyroxenite",           "Clinopiroxenita"
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
  
  QAP_UM.ternaryAxes <- list(
    aaxis = axis("Ol"), 
    baxis = axis("Opx"), 
    caxis = axis("Cpx")
  )
  
  QAP_UM.pal = viridisLite::viridis(10,direction = -1,option = 'D')
  
  if (any(output == 'ggplot' & language == 'en')) {
    QAP_UM <- ggtern::ggtern(data=tb.QAP_UM,ggtern::aes(Opx,Ol,Cpx)) +
      ggplot2::geom_polygon(aes(fill=Label.en,group=Label.en),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('',values = QAP_UM.pal) +
      ggplot2::labs(title="Ultramafic",
                    T="Ol",
                    L="Opx",
                    R="Cpx")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_UM <- ggtern::ggtern(data=tb.QAP_UM,ggtern::aes(Opx,Ol,Cpx)) +
      ggplot2::geom_polygon(aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_manual('',values = QAP_UM.pal) +
      ggplot2::labs(title="Ultramaficas",
                    T="Ol",
                    L="Opx",
                    R="Cpx")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_UM = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_UM,
        a = ~Ol, b = ~Opx, c = ~Cpx, 
        color = ~Label.en,
        colors = QAP_UM.pal,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Ultramafic"), ternary = QAP_UM.ternaryAxes
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
        a = ~Ol, b = ~Opx, c = ~Cpx, 
        color = ~Label.es,
        colors = QAP_UM.pal,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Ultramaficas"), ternary = QAP_UM.ternaryAxes
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
