ternary_pyroclastic_type = function(output = c('ggplot','plotly'), 
                                    language = c('en','es'),
                                    opacity = .5) {
  
  # library(ggplot2)
  # library(ggtern)
  library(magrittr)
  
  # load('data/all_tibbles.rdata')
  
  tb.pyro = tibble::tribble(
    ~L, ~G, ~C, ~Label, ~Label.es,
    100,0,0,"Lithic tuff","Toba lítica",
    50,50,0,"Lithic tuff","Toba lítica",
    35,35,35,"Lithic tuff","Toba lítica",
    50,0,50,"Lithic tuff","Toba lítica",
    100,0,0,"Lithic tuff","Toba lítica",
    0,100,0,"Vitric tuff","Toba vítrica",
    50,50,0,"Vitric tuff","Toba vítrica",
    35,35,35,"Vitric tuff","Toba vítrica",
    0,50,50,"Vitric tuff","Toba vítrica",
    0,100,0,"Vitric tuff","Toba vítrica",
    0,0,100,"Crystal tuff","Toba cristalina",
    50,0,50,"Crystal tuff","Toba cristalina",
    35,35,35,"Crystal tuff","Toba cristalina",
    0,50,50,"Crystal tuff","Toba cristalina",
    0,0,100,"Crystal tuff","Toba cristalina"
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
  
  pyro.ternaryAxes.en <- list(
    aaxis = axis("Rock fragments"), 
    baxis = axis("Glass"), 
    caxis = axis("Crystals")
  )
  
  pyro.ternaryAxes.es <- list(
    aaxis = axis("Litoclastos"), 
    baxis = axis("Vitroclastos"), 
    caxis = axis("Cristaloclastos")
  )

  
  if (any(output == 'ggplot' & language == 'en')) {
    pyro <- ggtern::ggtern(data=tb.pyro,ggtern::aes(G,L,C)) +
      ggplot2::geom_polygon(aes(fill=Label,color=Label,
                                group=Label),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::labs(fill = "Pyroclastic rock",
                    color = "Pyroclastic rock",
                    T="Rock fragments",
                    L="Glass",
                    R="Crystals")
  } else if (any(output == 'ggplot' & language == 'es')) {
    pyro <- ggtern::ggtern(data=tb.pyro,ggtern::aes(G,L,C)) +
      ggplot2::geom_polygon(aes(fill=Label.es,color=Label.es,
                                group=Label.es),
                            alpha=opacity) +
      ggtern::theme_bw() + 
      ggtern::theme_arrowdefault() +
      ggtern::theme_clockwise() +
      ggplot2::scale_fill_brewer(palette = 'Set1') +
      ggplot2::scale_color_brewer(palette = 'Set1') +
      ggplot2::labs(fill = "Roca piroclástica",
                    color = "Roca piroclástica",
                    T="Litoclastos",
                    L="Vitroclastos",
                    R="Cristaloclastos")
  } else if (any(output == 'plotly' & language == 'en')) {
    pyro = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.pyro,
        a = ~L, b = ~G, c = ~C, color = ~Label, 
        colors = 'Set1',
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = pyro.ternaryAxes.en,
        legend = list(title=list(text='<b> Pyroclastic rock </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic_type',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  } else if (any(output == 'plotly' & language == 'es')) {
    pyro = plotly::plot_ly() %>% 
      plotly::add_trace(
        data = tb.pyro,
        a = ~L, b = ~G, c = ~C, color = ~Label.es, 
        colors = 'Set1',
        opacity = opacity*2,
        type = "scatterternary",
        fill = "toself", 
        mode = "lines",
        hoveron = 'fills'
      ) %>% 
      plotly::layout(
        ternary = pyro.ternaryAxes.es,
        legend = list(title=list(text='<b> Roca piroclástica </b>')),
        margin = list(autoexpand=T,t=35)
      ) %>% 
      plotly::config(
        toImageButtonOptions = list(
          format = 'svg',
          filename = 'Pyroclastic_type',
          width = 9 * 96,
          height = 6 * 96
        )
      )
  }
  
  return(pyro)
  
}
