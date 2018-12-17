#' Coloured Corrlation Matrix
#'
#' This function creates a matrix of plots, given a dataframe and column names. The diagonal is histograms of each of the selected variables, whereas the area under the diagonal consists of scatterplots. Additionally, the function can accept a parameter to map to a colour aesthetic in the scatterplots, can add geom_smooth to the scatterplots, and can calculate Pearson Product-Moment correlation coefficients, written above the diagonal.
#' @import dplyr
#' @param data A dataframe or tibble.
#' @param cor.var A list of column names of the variables to include in the correlation matrix.
#' @param col.var An optional column name of the variable to be mapped to the colour aesthetic. Can be either continuous or discrete.
#' @param cor.names An optional list of names for axis labels of the variables in the correlation matrix.
#' @param col.names An optional names for the coloured variable legend.
#' @param palette An optional vector of colours to use as a colour palette for the variable mapped to the colour aesthetic.
#' @param alpha Trasperancy value of the points in the scatterplot. Defaults to .8.
#' @param method An optional variable to add geom_smooth() to the scatterplot. Inherits from the "method" argument of geom_smooth().
#' @param pearson If TRUE (default) writes the coefficients from Pearson Product-Moment Correlations between each two variables above the diagonal. If FALSE leaves the area above the diagonal blank.
#' @export
#' @examples
#' CorMatCol(data = iris, cor.var = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), col.var = "Species")

CorMatCol <- function(data,
                      cor.var,
                      col.var = NULL,
                      cor.names = NULL,
                      col.names = NULL,
                      palette = NULL,
                      alpha = .8,
                      method = NULL,
                      pearson = TRUE){
  data <- tibble::as_tibble(data)
  nm <- as.character(c(cor.var,
                       col.var))
  if (!is.null(col.var)){
    z.col <- data[, names(data) == nm[length(nm)]] %>%
      dplyr::mutate(id = 1:n())
    names(z.col) <- c("col_var", "id")
    z <- data[, names(data) %in% nm[1:length(nm)-1]] %>%
      dplyr::mutate(id = 1:n()) %>%
      dplyr::left_join(z.col, by = "id") %>%
      tidyr::gather("cor_var", "value", 1:(length(nm)-1))
    names(z) <- c("id", "col_var", "cor_var", "value")
  }else{
    z <- data[, names(data) %in% nm] %>%
      dplyr::mutate(id=1:n()) %>%
      tidyr::gather("cor_var", "value", 1:(length(nm)))
    names(z) <- c("id", "cor_var", "value")
  }
  list.tbl <- list()
  k = 1
  for (i in 1:length(cor.var)){
    for (j in 1:length(cor.var)){
      if (i == j){
        z2 <- z %>% subset(cor_var == nm[i])
        z2 <- z2[, !names(z2) %in% c("id")]
        if (!is.null(col.var)){
          names(z2) <- c("col_var", "cor_var", "x_value")
        }else{
          names(z2) <- c("cor_var", "x_value")
        }
        list.tbl[[k]] <- ggplot2::ggplot(z2) +
          ggplot2::geom_density(ggplot2::aes(x = x_value),
                                colour = "black",
                                fill = "black",
                                alpha = .3) +
          ggplot2::theme(legend.position = "none",
                         panel.background = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_blank())
      }else{
        z2 <- z %>% subset(cor_var %in% c(nm[i], nm[j])) %>%
          tidyr::spread(cor_var, value)
        z2 <- z2[, !names(z2) %in% c("id")]
        if (!is.null(col.var)){
          z2 <- z2[, c("col_var", nm[j], nm[i])]
          names(z2) <- c("col_var", "x_value", "y_value")
        }else{
          z2 <- z2[, c(nm[i], nm[j])]
          names(z2) <- c("x_value", "y_value")
        }
        if(i < j){
          if (isTRUE(pearson)){
            z.cor <- cor.test(z2$x_value,
                              z2$y_value,
                              method = "pearson")
            z.cor.df <- tibble::data_frame(x = 1.5,
                                           y = 1.5,
                                           text = as.character(round(z.cor$estimate, 2)),
                                           type = dplyr::case_when(z.cor$estimate < 0 ~ "#FC4E07",
                                                                   z.cor$estimate > 0 ~ "#00AFBB"))
            list.tbl[[k]] <- ggplot2::ggplot(z.cor.df,
                                             ggplot2::aes(x, y, colour = type)) +
              ggplot2::geom_text(ggplot2::aes(label = text),
                                 size = 5+10^abs(z.cor$estimate)) +
              ggplot2::theme_void() +
              ggplot2::theme(legend.position = "none") +
              ggplot2::scale_color_manual(values = z.cor.df$type)
          }else{
            list.tbl[[k]] <- NULL
          }
        }else{
          list.tbl[[k]] <- ggplot2::ggplot(z2)
          if (!is.null(col.var)){
            list.tbl[[k]] <- list.tbl[[k]] +
              ggplot2::geom_point(ggplot2::aes(x = x_value,
                                               y = y_value,
                                               color = col_var),
                                  size = 2,
                                  alpha = alpha)
          }else{
            list.tbl[[k]] <- list.tbl[[k]] +
              ggplot2::geom_point(ggplot2::aes(x = x_value,
                                               y = y_value),
                                  size = 2,
                                  alpha = alpha)
          }
          if (!is.null(method)){
            list.tbl[[k]] <- list.tbl[[k]] +
              ggplot2::geom_smooth(ggplot2::aes(x = x_value,
                                                y = y_value),
                                   method = method)
          }
          list.tbl[[k]] <- list.tbl[[k]] +
            ggplot2::theme(legend.position = "none",
                           panel.background = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank(),
                           panel.grid.major = ggplot2::element_blank(),
                           axis.ticks = ggplot2::element_blank(),
                           axis.text.y = ggplot2::element_blank(),
                           axis.text.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_blank(),
                           axis.title.x = ggplot2::element_blank()
            )
          if (!is.null(col.var)){
            if (!is.null(col.names)){
              if (!is.null(palette)){
                if (is.numeric(z2$col_var)){
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_gradientn(colors = palette,
                                                    name = col.names)
                }else{
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_manual(values = palette,
                                                 name = col.names)
                }
              }else{
                if (is.numeric(z2$col_var)){
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_gradientn(name = col.names)
                }else{
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_brewer(type = "div",
                                                 name = col.names)
                }
              }
            }else{
              if (!is.null(palette)){
                if (is.numeric(z2$col_var)){
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_gradientn(colors = palette,
                                                    name = col.var)
                }else{
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_manual(values = palette,
                                                 name = col.var)
                }
              }else{
                if (is.numeric(z2$col_var)){
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_gradient(name = col.var)
                }else{
                  list.tbl[[k]] <- list.tbl[[k]] +
                    ggplot2::scale_colour_brewer(type = "div",
                                                 name = col.var)
                }
              }
            }
          }
        }
      }
      if (j == 1) {
        if(!is.null(cor.names)){
          list.tbl[[k]] <- list.tbl[[k]] +
            ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90)) +
            ggplot2::ylab(cor.names[i])
        }else{
          list.tbl[[k]] <- list.tbl[[k]] +
            ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90)) +
            ggplot2::ylab(nm[i])
        }
      }
      if (i == length(cor.var)) {
        if(!is.null(cor.names)){
          list.tbl[[k]] <- list.tbl[[k]] +
            ggplot2::theme(axis.title.x = ggplot2::element_text()) +
            ggplot2::xlab(cor.names[j])
        }else{
          list.tbl[[k]] <- list.tbl[[k]] +
            ggplot2::theme(axis.title.x = ggplot2::element_text()) +
            ggplot2::xlab(nm[j])
        }
      }
      k = k + 1
    }
  }
  if (!is.null(col.var)){
    if (is.numeric(z2$col_var)){
      legend <- cowplot::get_legend(list.tbl[[length(list.tbl)-1]] +
                                      ggplot2::theme(legend.position = "bottom") +
                                      ggplot2::guides(color = ggplot2::guide_colourbar(barwidth = 10*length(nm)))
      )
    }else{
      legend <- cowplot::get_legend(list.tbl[[length(list.tbl)-1]] +
                                      ggplot2::theme(legend.position = "bottom") +
                                      ggplot2::guides(color = ggplot2::guide_legend(keywidth = 1/length(nm),
                                                                                    label.theme = ggplot2::element_text(size = 1.5*length(nm))))
      )
    }
  }
  the.plot <- cowplot::plot_grid(plotlist = list.tbl,
                                 nrow = length(cor.var),
                                 align = "hv")
  if (!is.null(col.var)){
    the.plot <- cowplot::plot_grid(the.plot,
                                   legend, nrow = 2,
                                   rel_heights = c(20, 1))
  }
  return(the.plot)
}
