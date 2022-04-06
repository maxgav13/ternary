qap_m = function(output = c('ggplot','plotly'), 
                 language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP_M = tibble::tribble(
    ~P, ~Ol, ~Px, ~Label.en, ~Label.es,
    100,0,0,"Anorthosite","Anortosita",
    90,10,0,"Anorthosite","Anortosita",
    90,0,10,"Anorthosite","Anortosita",
    90,10,0,"Troctolite","Troctolita",
    10,90,0,"Troctolite","Troctolita",
    5,90,5,"Troctolite","Troctolita",
    90,5,5,"Troctolite","Troctolita",
    90,5,5,"Olivine gabbro /
        Olivine norite","Gabro olivínoco /
        Norita olivínica",
    5,90,5,"Olivine gabbro /
        Olivine norite","Gabro olivínoco /
        Norita olivínica",
    5,5,90,"Olivine gabbro /
        Olivine norite","Gabro olivínoco /
        Norita olivínica",
    90,0,10,"Gabbro / Norite","Gabro / Norita",
    90,5,5,"Gabbro / Norite","Gabro / Norita",
    5,5,90,"Gabbro / Norite","Gabro / Norita",
    10,0,90,"Gabbro / Norite","Gabro / Norita",
    10,90,0,"Dunite","Dunita",
    0,100,0,"Dunite","Dunita",
    0,90,10,"Dunite","Dunita",
    5,90,5,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    0,90,10,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    0,42.5,57.5,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    5,40,55,"Wehrlite /
        Harzburgite","Wehrlita /
        Harzburgita",
    5,40,55,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita olivínica",
    0,42.5,57.5,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita olivínica",
    0,10,90,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita olivínica",
    5,5,90,"Olivine clino/orto-pyroxenite","Clino/Orto-piroxenita olivínica",
    10,0,90,"Clinopyroxenite /
        Orthopyroxenite","Clinopiroxenita /
        Ortopiroxenita",
    0,10,90,"Clinopyroxenite /
        Orthopyroxenite","Clinopiroxenita /
        Ortopiroxenita",
    0,0,100,"Clinopyroxenite /
        Orthopyroxenite","Clinopiroxenita /
        Ortopiroxenita") %>% 
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
  
  QAP_M.ternaryAxes <- list(
    aaxis = axis("P"), 
    baxis = axis("Ol"), 
    caxis = axis("Px")
  )
  
  QAP_M.pal = viridisLite::viridis(8,direction = -1,option = 'F')
  
  if (any(output == 'ggplot' & language == 'en')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(Ol,P,Px)) +
      ggplot2::geom_polygon(aes(fill=Label.en,group=Label.en),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual('',values = QAP_M.pal) +
      ggplot2::labs(title="Mafic",
                    T="P",
                    L="Ol",
                    R="Px")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(Ol,P,Px)) +
      ggplot2::geom_polygon(aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual('',values = QAP_M.pal) +
      ggplot2::labs(title="Maficas",
                    T="P",
                    L="Ol",
                    R="Px")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_M = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~P, b = ~Ol, c = ~Px, 
        color = ~Label.en,
        colors = QAP_M.pal,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Mafic"), ternary = QAP_M.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Mafic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    QAP_M = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~P, b = ~Ol, c = ~Px, 
        color = ~Label.es,
        colors = QAP_M.pal,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        line = list(color = "black"),
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Maficas"), ternary = QAP_M.ternaryAxes
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Maficas',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(QAP_M)
  
}
