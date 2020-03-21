# This file contains the display code for all components.

output$bar_chart <- renderPlot({
  req(uc$bar_chart_var1, uc$bar_chart_var2, uc$bar_chart_group_by)
  df <- create_analysis_sample()
  df[, uc$bar_chart_var1] <- as.factor(df[, uc$bar_chart_var1])
  if (uc$bar_chart_var2 != "None")
    df[, uc$bar_chart_var2] <- as.factor(df[, uc$bar_chart_var2])
  if (uc$bar_chart_group_by != "All")
    df <- df[df[, uc$group_factor] == uc$bar_chart_group_by,]
  if (uc$bar_chart_var2 != "None" & (!uc$bar_chart_relative))
    p <- ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
    ggplot2::geom_bar(ggplot2::aes(fill=df[,uc$bar_chart_var2]), position = "stack") +
    ggplot2::labs(x = uc$bar_chart_var1, fill = uc$bar_chart_var2)
  else if (uc$bar_chart_var2 != "None")
    p <- ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
    ggplot2::geom_bar(ggplot2::aes(fill=df[,uc$bar_chart_var2]), position = "fill") +
    ggplot2::labs(x = uc$bar_chart_var1, fill = uc$bar_chart_var2, y = "Percent") +
    ggplot2::scale_y_continuous(labels = scales::percent_format())
  else p <- ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
    ggplot2::geom_bar() + ggplot2::labs(x = uc$bar_chart_var1)
  if (length(levels(df[,uc$bar_chart_var1])) > 5) {
    if (!anyNA(suppressWarnings(
      as.numeric(as.character(df[, uc$bar_chart_var1]))
    ))) {
      p <- p + ggplot2::scale_x_discrete(
        breaks = pretty(as.numeric(as.character(df[, uc$bar_chart_var1])), n = 10)
      )
    } else {
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
  }
  p
})

output$missing_values <- renderPlot({
  req(uc$missing_values_group_by)
  df <- create_analysis_sample()
  if (uc$missing_values_group_by != "All")
    df <- df[df[, uc$group_factor] == uc$missing_values_group_by,]
  prepare_missing_values_graph(df, lts_id$name)
})

output$descriptive_table_analysis <- DT::renderDataTable(
  server=FALSE,
  expr = {
    df <- create_analysis_sample()
    rowtips = as.character(sample_definition[c(lnumeric$col, llogical$col), "var_def"])
    DT::datatable({
      if (uc$desc_group_by == "All") prepare_descriptive_table(df[,c(lnumeric$col, llogical$col)])$df
      else prepare_descriptive_table(df[df[, uc$group_factor] == uc$desc_group_by, c(lnumeric$col, llogical$col)])$df
    },
    colnames=c('Variable'=1), selection=list(mode = 'multiple', target = 'row'),
    options= list(dom='t', paging=FALSE, ordering=FALSE),
    callback = DT::JS("var tips =[", paste("'", quote_escape(rowtips), "'", collapse=", ", sep=""), "],
                      firstColumn = $('#descriptive_table_analysis tr td:first-child');
                      for (var i = 0; i < tips.length; i++) {
                      $(firstColumn[i]).attr('title', jQuery('<div />').html(tips[i]).text());
                      }")
    ) %>%
      DT::formatCurrency(1, currency ="", interval=3, mark=',', digits=0) %>%
      DT::formatCurrency('Mean', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Std. dev.', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Min.', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('25 %', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Median', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('75 %', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Max.', currency = "", interval=3, mark=',', digits=3)
  }
)

output$descriptive_table_base <- DT::renderDataTable(
  server=FALSE,
  expr = {
    df <- create_base_sample()
    rowtips = as.character(bs_definition[bs_definition$type == "numeric" |
                                           bs_definition$type == "logical", "var_def"])
    DT::datatable({
      if (uc$desc_group_by == "All") prepare_descriptive_table(df[,c(as.character(bs_definition[bs_definition$type == "numeric" |
                                                                                                  bs_definition$type == "logical", "var_name"]))])$df
      else prepare_descriptive_table(df[df[, uc$group_factor] == uc$desc_group_by, c(as.character(bs_definition[bs_definition$type == "numeric" |
                                                                                                                  bs_definition$type == "logical", "var_name"]))])$df
    },
    colnames=c('Variable'=1), selection=list(mode = 'none'),
    options= list(dom='t', paging=FALSE, ordering=FALSE),
    callback = DT::JS("var tips =[", paste("'", quote_escape(rowtips), "'", collapse=", ", sep=""), "],
                      firstColumn = $('#descriptive_table_base tr td:first-child');
                      for (var i = 0; i < tips.length; i++) {
                      $(firstColumn[i]).attr('title', jQuery('<div />').html(tips[i]).text());
                      }")
    )%>%
      DT::formatCurrency(1, currency ="", interval=3, mark=',', digits=0) %>%
      DT::formatCurrency('Mean', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Std. dev.', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Min.', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('25 %', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Median', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('75 %', currency = "", interval=3, mark=',', digits=3) %>%
      DT::formatCurrency('Max.', currency = "", interval=3, mark=',', digits=3)
  }
)

output$histogram <- renderPlot({
  req(uc$hist_group_by, uc$hist_var, uc$hist_nr_of_breaks)
  df <- create_analysis_sample()
  if (uc$hist_group_by == "All") hist(as.numeric(df[,uc$hist_var]), main="", xlab = uc$hist_var, col="red", right = FALSE, breaks=uc$hist_nr_of_breaks)
  else hist(as.numeric(df[df[, uc$group_factor] == uc$hist_group_by, uc$hist_var]), main="", xlab = uc$hist_var, col="red", right=FALSE, breaks=uc$hist_nr_of_breaks)
})

output$ext_obs <- renderPrint({
  df <- create_analysis_sample()
  if (cross_sec_data()) vars <- c(lcs_id$name, uc$ext_obs_var)
  else vars <- c(lcs_id$name, lts_id$name, uc$ext_obs_var)
  if (uc$group_factor != "None") vars <- c(vars, uc$group_factor)
  df <- df[, vars]
  if (req(uc$ext_obs_period_by) != "All")
    df <- df[df[, lts_id$name] == uc$ext_obs_period_by,]
  if (req(uc$ext_obs_group_by) != "All")
    df <- df[df[, uc$group_factor] == uc$ext_obs_group_by, vars]
  df <- droplevels(df[complete.cases(df),])
  if (nrow(df) <= 10) cat("Not enough data to generate table")
  else {
    tab <- prepare_ext_obs_table(df, var = uc$ext_obs_var)
    cat(tab$kable_ret %>%
          kableExtra::kable_styling())
  }
}
)

output$by_group_bar_graph.ui <- renderUI({
  req(uc$bgbg_group_by, uc$bgbg_var, uc$bgbg_byvar)
  df <- create_analysis_sample()
  if (uc$bgbg_group_by == "All")
    bins <- length(unique(df[is.finite(df[,uc$bgbg_var]), uc$bgbg_byvar]))
  else
    bins <- length(unique(df[df[, uc$group_factor] == uc$bgbg_group_by & is.finite(df[,uc$bgbg_var]), uc$bgbg_byvar]))
  isolate(plotOutput("by_group_bar_graph",
                     height=max(400, 15 * bins)))
})

output$by_group_bar_graph <- renderPlot({
  req(uc$bgbg_var, uc$bgbg_byvar)
  q25 <- function(x, na.rm) {quantile(x, 0.25, na.rm)}
  q75 <- function(x, na.rm) {quantile(x, 0.75, na.rm)}

  df <- create_analysis_sample()
  if (uc$bgbg_group_by == "All")
    prepare_by_group_bar_graph(df[, c(uc$bgbg_byvar, uc$bgbg_var)],
                               uc$bgbg_byvar, uc$bgbg_var, get(uc$bgbg_stat), uc$bgbg_sort_by_stat)$plot +
    ggplot2::ylab(paste(uc$bgbg_stat, uc$bgbg_var))
  else
    prepare_by_group_bar_graph(df[df[, uc$group_factor] == uc$bgbg_group_by, c(uc$bgbg_byvar, uc$bgbg_var)],
                               uc$bgbg_byvar, uc$bgbg_var, get(uc$bgbg_stat), uc$bgbg_sort_by_stat)$plot +
    ggplot2::ylab(paste(uc$bgbg_stat, uc$bgbg_var))
})

output$by_group_violin_graph.ui <- renderUI({
  req(uc$bgvg_group_by, uc$bgvg_var, uc$bgvg_byvar)
  df <- create_analysis_sample()
  if (uc$bgvg_group_by == "All")
    bins <- length(unique(df[is.finite(df[,uc$bgvg_var]), uc$bgvg_byvar]))
  else
    bins <- length(unique(df[df[, uc$group_factor] == uc$bgvg_group_by & is.finite(df[,uc$bgvg_var]), uc$bgvg_byvar]))
  isolate(plotOutput("by_group_violin_graph",
                     height=max(400, 30 * bins)))
})

output$by_group_violin_graph <- renderPlot({
  req(uc$bgvg_var, uc$bgvg_byvar)
  df <- create_analysis_sample()
  if (uc$bgvg_group_by == "All")
    prepare_by_group_violin_graph(df[, c(uc$bgvg_byvar, uc$bgvg_var)],
                                  uc$bgvg_byvar, uc$bgvg_var, uc$bgvg_sort_by_stat)
  else
    prepare_by_group_violin_graph(df[df[, uc$group_factor] == uc$bgvg_group_by, c(uc$bgvg_byvar, uc$bgvg_var)],
                                  uc$bgvg_byvar, uc$bgvg_var, uc$bgvg_sort_by_stat)
})

output$trend_graph <- renderPlot({
  req(uc$trend_graph_var1,uc$trend_graph_var2,uc$trend_graph_var3)
  df <- create_analysis_sample()
  if (uc$trend_graph_group_by == "All")
    varlist <- c(lts_id$name, uc$trend_graph_var1, uc$trend_graph_var2, uc$trend_graph_var3)
  else
    varlist <- c(lts_id$name, uc$group_factor, uc$trend_graph_var1, uc$trend_graph_var2, uc$trend_graph_var3)
  varlist <- varlist[! varlist %in% "None"]
  df <- df[,varlist]
  if (uc$trend_graph_group_by == "All") prepare_trend_graph(df, lts_id$name)
  else prepare_trend_graph(df[df[, uc$group_factor] == uc$trend_graph_group_by,-which(names(df) %in% uc$group_factor)], lts_id$name)
})

output$quantile_trend_graph <- renderPlot({
  req(uc$quantile_trend_graph_var)
  df <- create_analysis_sample()
  quantiles <- as.numeric(uc$quantile_trend_graph_quantiles)
  if (uc$quantile_trend_graph_group_by == "All")
    prepare_quantile_trend_graph(df[, c(lts_id$name, uc$quantile_trend_graph_var)], lts_id$name, quantiles)
  else
    prepare_quantile_trend_graph(df[df[, uc$group_factor] == uc$quantile_trend_graph_group_by,
                                    c(lts_id$name, uc$quantile_trend_graph_var)], lts_id$name, quantiles)
})

output$by_group_trend_graph <- renderPlot({
  req(uc$bgtg_var, uc$bgtg_byvar)
  df <- create_analysis_sample()
  if (uc$bgtg_group_by == "All")
    varlist <- c(lts_id$name, uc$bgtg_byvar, uc$bgtg_var)
  else
    varlist <- c(lts_id$name, uc$group_factor, uc$bgtg_byvar, uc$bgtg_var)
  df <- df[,varlist]
  if (uc$bgtg_group_by == "All")
    prepare_by_group_trend_graph(df, lts_id$name, uc$bgtg_byvar, uc$bgtg_var)
  else prepare_by_group_trend_graph(df[df[, uc$group_factor] == uc$bgtg_group_by,
                                       -which(names(df) %in% uc$group_factor)],
                                    lts_id$name, uc$bgtg_byvar, uc$bgtg_var)
})

output$corrplot.ui <- renderUI({
  req(uc$config_parsed)
  isolate(plotOutput("corrplot", hover = hoverOpts("corrplot_hover", delay = 100, delayType = "debounce"),
                     height = max(min(length(c(lnumeric$col, llogical$col))*50,1000), 300),
                     width = max(min(length(c(lnumeric$col, llogical$col))*50,1000), 300)))
})

output$corrplot <- renderPlot({
  if (DEBUG) tictoc::tic("rendering corrplot")
  df <- create_analysis_sample()
  if (uc$corrplot_group_by != "All") df <- df[df[, uc$group_factor] == uc$corrplot_group_by,]
  vars <- c(lnumeric$col, llogical$col)
  vars <- vars[vars %in% which(!sapply(df, function(x) all(is.na(x))))]
  vars <- vars[vars %in% which(!sapply(df, function(x) (all(duplicated(x)[-1]))))]
  ret <- prepare_correlation_graph(df[, vars])
  correl_r <<- ret$df_corr
  correl_p <<- ret$df_prob
  correl_n <<- ret$df_n
  if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
})

output$corrplot_hover_info <- renderUI({
  req(input$corrplot_hover)
  hover <- input$corrplot_hover
  if (!is.null(hover)) {
    nvars <- nrow(correl_r)
    r <- rep(1:nvars,nvars)
    c <- rep(1:nvars, each=nvars)
    df <- data.frame(r,c)
    point <- nearPoints(df, hover, threshold = 10, maxpoints = 1, xvar = "c", yvar="r", addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left)) /
      hover$img_css_ratio$x
    top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top)) /
      hover$img_css_ratio$y

    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style, class = "well-sm",
      htmltools::HTML(paste0(formatC(correl_r[nvars - point$r + 1, point$c], format ="f", digits=3), " (",
                             formatC(correl_p[nvars - point$r + 1, point$c], format ="f", digits=3), ")<br> n: ",
                             formatC(correl_n[nvars - point$r + 1, point$c], format ="f", digits=0, big.mark=",")))
    )
  }
})

output$scatter_plot <- renderPlot({
  req(uc$scatter_x,uc$scatter_y,uc$scatter_size,uc$scatter_color)
  if (DEBUG) tictoc::tic("rendering scatter_plot")
  scatter_df <- create_analysis_sample()
  if (uc$scatter_group_by != "All")
    scatter_df <- scatter_df[scatter_df[, uc$group_factor] == uc$scatter_group_by,]
  varlist <- c(lcs_id$name, lts_id$name,
               uc$scatter_x, uc$scatter_y, uc$scatter_color, uc$scatter_size)
  scatter_df <- scatter_df[,varlist[!grepl("None", varlist)]]
  scatter_df <- scatter_df[complete.cases(scatter_df),]
  if (uc$scatter_color %in% lfactor$name) scatter_df[,uc$scatter_color] <- as.factor(scatter_df[,uc$scatter_color])
  if (uc$scatter_sample & (nrow(scatter_df) > 1000)) {
    set.seed(42)
    scatter_df <- dplyr::sample_n(scatter_df, 1000)
  }
  scatter_color <- ifelse(uc$scatter_color == "None", "", uc$scatter_color)
  scatter_size <- ifelse(uc$scatter_size == "None", "", uc$scatter_size)
  scatter_loess <- ifelse(uc$scatter_loess, 1, 0)
  plot <- prepare_scatter_plot(scatter_df, uc$scatter_x, uc$scatter_y,
                               scatter_color, scatter_size, scatter_loess)
  if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
  plot
})

output$hover_info <- renderUI({
  req(input$plot_hover,uc$scatter_x,uc$scatter_y,uc$scatter_size,uc$scatter_color)
  scatter_df <- create_analysis_sample()
  if (uc$scatter_group_by != "All")
    scatter_df <- scatter_df[scatter_df[, uc$group_factor] == uc$scatter_group_by,]
  varlist <- c(lcs_id$name, lts_id$name,
               uc$scatter_x, uc$scatter_y, uc$scatter_size, uc$scatter_color)
  scatter_df <- scatter_df[,varlist[!grepl("None", varlist)]]
  hover <- input$plot_hover
  if (!is.null(hover)) {
    hover$mapping$x <- uc$scatter_x
    hover$mapping$y <- uc$scatter_y
  }
  point <- nearPoints(scatter_df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  if (nrow(point) == 0) return(NULL)

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left)) /
    hover$img_css_ratio$x
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) /
    hover$img_css_ratio$y

  # create style property for tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")

  # Get rid of the factors in the data frame by converting all columns to character
  point[] <- lapply(point, as.character)

  # actual tooltip created as wellPanel
  if (cross_sec_data()) {
    wellPanel(
      style = style, class = "well-sm",
      htmltools::HTML(paste0(point[,lcs_id$name], collaspe=" "))
    )
  } else {
    wellPanel(
      style = style, class = "well-sm",
      htmltools::HTML(paste0(point[,lcs_id$name], collaspe=" "),
                      "<br>",
                      point[,lts_id$name])
    )

  }
})

output$regression <- renderPrint({
  if (DEBUG) tictoc::tic("estimating regressions")
  req(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$model, uc$reg_by)
  varlist <- c(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$reg_by)
  varlist <- varlist[!varlist %in% "None"]
  df <- create_analysis_sample()[,varlist]
  if (uc$reg_by != "None") df[, uc$reg_by] <- as.factor(df[, uc$reg_by])
  if (!uc$reg_y %in% c(lnumeric$name, llogical$name))
    df[, uc$reg_y] <- as.factor(df[, uc$reg_y])
  df <- droplevels(df[complete.cases(df),])

  feffect <- ""
  if (uc$reg_fe1 != "None") {
    feffect <- uc$reg_fe1
    if (uc$reg_fe2 != "None") feffect <- c(feffect, uc$reg_fe2)
  }
  else if (uc$reg_fe2 != "None") feffect <- uc$reg_fe2
  cluster <- ""
  if (uc$cluster == 2) cluster <- uc$reg_fe1
  if (uc$cluster == 3) cluster <- uc$reg_fe2
  if (uc$cluster == 4) cluster <- c(uc$reg_fe1, uc$reg_fe2)
  cluster <- cluster[!cluster %in% "None"]
  reg_by <- ifelse(uc$reg_by != "None", uc$reg_by, "")
  t <- prepare_regression_table(df = df, dvs = uc$reg_y, idvs = uc$reg_x,
                                feffects = feffect, clusters = cluster,
                                models = uc$model, byvar = reg_by)
  if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
  htmltools::HTML(t$table)
})
