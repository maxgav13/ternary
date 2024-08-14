ternary_qap_m_hbl = function(output = c('ggplot','plotly'), 
                             language = c('en','es'),
                             opacity = .5) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.QAP_M = tibble::tribble(
    ~P, ~Px, ~Hbl, ~Label.en, ~Label.es,
    100,0,0,"Anorthosite","Anortosita",
    90,10,0,"Anorthosite","Anortosita",
    90,0,10,"Anorthosite","Anortosita",
    100,0,0,"Anorthosite","Anortosita",
    90,10,0,"Gabbro/Norite","Gabro/Norita",
    10,90,0,"Gabbro/Norite","Gabro/Norita",
    10,85,5,"Gabbro/Norite","Gabro/Norita",
    90,5,5,"Gabbro/Norite","Gabro/Norita",
    90,10,0,"Gabbro/Norite","Gabro/Norita",
    90,5,5,"Pyroxene hornblende gabbro/norite",
    "Gabro/Norita pyroxénica hornbléndica",
    10,85,5,"Pyroxene hornblende gabbro/norite",
    "Gabro/Norita pyroxénica hornbléndica",
    10,5,85,"Pyroxene hornblende gabbro/norite",
    "Gabro/Norita pyroxénica hornbléndica",
    90,5,5,"Pyroxene hornblende gabbro/norite",
    "Gabro/Norita pyroxénica hornbléndica",
    90,0,10,"Hornblende gabbro","Gabro honrbléndico",
    90,5,5,"Hornblende gabbro","Gabro honrbléndico",
    10,5,85,"Hornblende gabbro","Gabro honrbléndico",
    10,0,90,"Hornblende gabbro","Gabro honrbléndico",
    90,0,10,"Hornblende gabbro","Gabro honrbléndico",
    0,100,0,"Plagioclase-bearing pyroxenite","Piroxenita con plagioclasa",
    10,90,0,"Plagioclase-bearing pyroxenite","Piroxenita con plagioclasa",
    0,90,10,"Plagioclase-bearing pyroxenite","Piroxenita con plagioclasa",
    0,100,0,"Plagioclase-bearing pyroxenite","Piroxenita con plagioclasa",
    10,90,0,"Plagioclase-bearing hornblende pyroxenite",
    "Piroxenita hornbléndica con plagioclasa",
    10,45,45,"Plagioclase-bearing hornblende pyroxenite",
    "Piroxenita hornbléndica con plagioclasa",
    0,50,50,"Plagioclase-bearing hornblende pyroxenite",
    "Piroxenita hornbléndica con plagioclasa",
    0,90,10,"Plagioclase-bearing hornblende pyroxenite",
    "Piroxenita hornbléndica con plagioclasa",
    10,90,0,"Plagioclase-bearing hornblende pyroxenite",
    "Piroxenita hornbléndica con plagioclasa",
    10,45,45,"Plagioclase-bearing pyroxene hornblendite",
    "Hornblendita piroxénica con plagioclasa",
    10,0,90,"Plagioclase-bearing pyroxene hornblendite",
    "Hornblendita piroxénica con plagioclasa",
    0,10,90,"Plagioclase-bearing pyroxene hornblendite",
    "Hornblendita piroxénica con plagioclasa",
    0,50,50,"Plagioclase-bearing pyroxene hornblendite",
    "Hornblendita piroxénica con plagioclasa",
    10,45,45,"Plagioclase-bearing pyroxene hornblendite",
    "Hornblendita piroxénica con plagioclasa",
    10,0,90,"Plagioclase-bearing hornblendite","Hornblendita con plagioclasa",
    0,0,100,"Plagioclase-bearing hornblendite","Hornblendita con plagioclasa",
    0,10,90,"Plagioclase-bearing hornblendite","Hornblendita con plagioclasa",
    10,0,90,"Plagioclase-bearing hornblendite","Hornblendita con plagioclasa") %>% 
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
    caxis = axis("Hbl")
  )
  
  QAP_M.pal = viridisLite::viridis(8,direction = -1,option = 'F',begin = .2)
  
  if (any(output == 'ggplot' & language == 'en')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(Px,P,Hbl)) +
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
                    R="Hbl")
  } else if (any(output == 'ggplot' & language == 'es')) {
    QAP_M <- ggtern::ggtern(data=tb.QAP_M,ggtern::aes(Px,P,Hbl)) +
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
                    R="Hbl")
  } else if (any(output == 'plotly' & language == 'en')) {
    QAP_M = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.QAP_M,
        a = ~P, b = ~Px, c = ~Hbl, 
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
        a = ~P, b = ~Px, c = ~Hbl, 
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
