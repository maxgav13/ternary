ternary_qap_m_ol = function(output = c('ggplot','plotly'), 
                            language = c('en','es'),
                            opacity = .5) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP_M = tibble::tribble(
    ~P, ~Px, ~Ol, ~Label.en, ~Label.es,
    100,0,0,"Anorthosite","Anortosita",
    90,10,0,"Anorthosite","Anortosita",
    90,0,10,"Anorthosite","Anortosita",
    100,0,0,"Anorthosite","Anortosita",
    90,10,0,"Gabbro/Norite","Gabro/Norita",
    10,90,0,"Gabbro/Norite","Gabro/Norita",
    10,85,5,"Gabbro/Norite","Gabro/Norita",
    90,5,5,"Gabbro/Norite","Gabro/Norita",
    90,10,0,"Gabbro/Norite","Gabro/Norita",
    90,5,5,"Olivine gabbro/
        Olivine norite","Gabro olivínoco/
        Norita olivínica",
    10,85,5,"Olivine gabbro/
        Olivine norite","Gabro olivínoco/
        Norita olivínica",
    10,5,85,"Olivine gabbro/
        Olivine norite","Gabro olivínoco/
        Norita olivínica",
    90,5,5,"Olivine gabbro/
        Olivine norite","Gabro olivínoco/
        Norita olivínica",
    90,0,10,"Troctolite","Troctolita",
    90,5,5,"Troctolite","Troctolita",
    10,5,85,"Troctolite","Troctolita",
    10,0,90,"Troctolite","Troctolita",
    90,0,10,"Troctolite","Troctolita",
    10,90,0,"Plagioclase-bearing ultramafic rocks","Rocas ultramáficas con plagioclasa",
    0,100,0,"Plagioclase-bearing ultramafic rocks","Rocas ultramáficas con plagioclasa",
    0,0,100,"Plagioclase-bearing ultramafic rocks","Rocas ultramáficas con plagioclasa",
    10,0,90,"Plagioclase-bearing ultramafic rocks","Rocas ultramáficas con plagioclasa",
    10,90,0,"Plagioclase-bearing ultramafic rocks","Rocas ultramáficas con plagioclasa") %>% 
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
  
  QAP_M.ternaryAxes <- list(
    aaxis = axis("P"), 
    baxis = axis("Px"), 
    caxis = axis("Ol")
  )
  
  QAP_M.pal = viridisLite::viridis(5,direction = -1,option = 'F',begin = .2)
  
  if (any(output == 'ggplot' & language == 'en')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(Px,P,Ol)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.en,color=Label.en,group=Label.en),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual(values = QAP_M.pal) +
      ggplot2::scale_color_manual(values = QAP_M.pal) +
      ggplot2::labs(fill="Mafic",
                    color="Mafic",
                    T="P",
                    L="Px",
                    R="Ol")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(Px,P,Ol)) +
      ggplot2::geom_polygon(ggplot2::aes(fill=Label.es,color=Label.es,group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      # custom_percent("Percent") +
      ggplot2::scale_fill_manual(values = QAP_M.pal) +
      ggplot2::scale_color_manual(values = QAP_M.pal) +
      ggplot2::labs(fill="Máficas",
                    color="Máficas",
                    T="P",
                    L="Px",
                    R="Ol")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_M = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~P, b = ~Px, c = ~Ol, 
        color = ~Label.en,
        colors = QAP_M.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = QAP_M.ternaryAxes,
        legend = list(title=list(text='<b> Mafic </b>')),
        margin = list(autoexpand=T,t=35)
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
        a = ~P, b = ~Px, c = ~Ol, 
        color = ~Label.es,
        colors = QAP_M.pal,
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself",
        mode = "lines",
        hoverinfo = 'text',
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = QAP_M.ternaryAxes,
        legend = list(title=list(text='<b> Máficas </b>')),
        margin = list(autoexpand=T,t=35)
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
