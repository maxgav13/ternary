pyroclastic = function(output = c('ggplot','plotly'), 
                       language = c('en','es')) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.pyro = tibble::tribble(
    ~BB, ~Lapilli, ~Ash, ~Label, ~Label.es,
    100,0,0,"Pyroclastic breccia /
        Agglomerate","Brecha piroclástica /
        Aglomerado",
    75,25,0,"Pyroclastic breccia /
        Agglomerate","Brecha piroclástica /
        Aglomerado",
    75,0,25,"Pyroclastic breccia /
        Agglomerate","Brecha piroclástica /
        Aglomerado",
    100,0,0,"Pyroclastic breccia /
        Agglomerate","Brecha piroclástica /
        Aglomerado",
    75,25,0,"Lapilli-tuff breccia","Toba brechosa",
    25,75,0,"Lapilli-tuff breccia","Toba brechosa",
    25,0,75,"Lapilli-tuff breccia","Toba brechosa",
    75,0,25,"Lapilli-tuff breccia","Toba brechosa",
    75,25,0,"Lapilli-tuff breccia","Toba brechosa",
    25,75,0,"Lapilli-stone","Lapillita",
    0,100,0,"Lapilli-stone","Lapillita",
    0,75,25,"Lapilli-stone","Lapillita",
    25,75,0,"Lapilli-stone","Lapillita",
    25,75,0,"Lapilli tuff","Toba lapillítica",
    0,75,25,"Lapilli tuff","Toba lapillítica",
    0,25,75,"Lapilli tuff","Toba lapillítica",
    25,0,75,"Lapilli tuff","Toba lapillítica",
    25,75,0,"Lapilli tuff","Toba lapillítica",
    25,0,75,"Tuff","Toba",
    0,25,75,"Tuff","Toba",
    0,0,100,"Tuff","Toba",
    25,0,75,"Tuff","Toba") %>% 
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
  
  pyro.ternaryAxes.en <- list(
    aaxis = axis("Blocks & Bombs (> 64 mm)"), 
    baxis = axis("Lapilli (2-64 mm)"), 
    caxis = axis("Ash (< 2 mm)")
  )
  
  pyro.ternaryAxes.es <- list(
    aaxis = axis("Bloques & Bombas (> 64 mm)"), 
    baxis = axis("Lapilli (2-64 mm)"), 
    caxis = axis("Ceniza (< 2 mm)")
  )

  
  if (any(output == 'ggplot' & language == 'en')) {
    pyro <- ggtern::ggtern(data=tb.pyro,ggtern::aes(Lapilli,BB,Ash)) +
      ggplot2::geom_polygon(aes(fill=Label,group=Label),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      ggplot2::labs(title="Pyroclastic",
                    fill = "",
                    T="Blocks &\nBombs (> 64 mm)",
                    L="Lapilli\n(2-64 mm)",
                    R="Ash\n(< 2 mm)")
  } else if (any(output == 'ggplot' & language == 'es')) {
    pyro <- ggtern::ggtern(data=tb.pyro,ggtern::aes(Lapilli,BB,Ash)) +
      ggplot2::geom_polygon(aes(fill=Label.es,group=Label.es),
                            color="black",alpha=0.5) +
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      ggplot2::labs(title="Piroclásticas",
                    fill = "",
                    T="Bloques &\nBombas (> 64 mm)",
                    L="Lapilli\n(2-64 mm)",
                    R="Ceniza\n(< 2 mm)")
  } else if (any(output == 'plotly' & language == 'en')) {
    pyro = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.pyro,
        a = ~BB, b = ~Lapilli, c = ~Ash, color = ~Label, 
        colors = 'Set1',
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Pyroclastic"), ternary = pyro.ternaryAxes.en
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    pyro = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.pyro,
        a = ~BB, b = ~Lapilli, c = ~Ash, color = ~Label.es, 
        colors = 'Set1',
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        annotations = label("Piroclásticas"), ternary = pyro.ternaryAxes.es
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(pyro)
  
}
