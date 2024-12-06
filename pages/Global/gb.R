

# Naming convention -------------------------------------------------------

# gb is short for global
# Val is short for value
# Fun is short for Function

# Values ---------------------------------------------------------------

# Defined color scheme so it can be used for entire report
gbVal_colorScheme <- c(
  #Dark2 scheme Required for metrics that have more than 12 options
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
  "#a6761d", 
  
  #Paired scheme
  "#A6CEE3", "#FFFF99", "#B2DF8A", "#CAB2D6", "#FB9A99", "#FDBF6F", 
  "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A", "#1F78B4", "#B15928",
  
  # PrGn Required for metrics that have more than 20 options
  "#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#f7f7f7",
  "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"
  
)

# Defined shape scheme so it can be used for entire report
gbVal_shapeScheme <- c(
  16 # Filled circle
  , 17 # Filled triangle
  , 18 # Filled diamond
  , 25 #Inverted triangle with a border (can be filled with a color)
  , 22 #Square with a border (can be filled with a color)
  , 19 # Filled circle with a border
  , 20 # Filled circle with a smaller size
  , 21 #Circle with a border (can be filled with a color)
  , 23 # Diamond with a border (can be filled with a color)
  , 24 #Triangle with a border (can be filled with a color)
)

gbVal_chbExpLbl <- "Polk-Norman-Mahnomen Community Health Services (CHS)" # More detailed label

gbVal_chbAbbrLbl <- "Polk-Norman-Mahnomen CHS" # Less detailed label

# This is where I define what county(ies) are the drivers for this CHA process
# It is based off the MN County FIPS code
gbVal = c(
  # Don't change the order of the first three values.
  # The values can be set to anything so if in the future a standard is developed it can be changed here 
  # If I want the US standard to be 12345689, just change the 9 to it and it will be updated in the report.
  9 # Defined by Patrick Olson for this report this is for US totals 
  , 99 #Defined by Patrick Olson for this report this is for State totals
  , 999 #Defined by Patrick Olson this is for CHB totals
  , 27119  #Mahnomen
  , 27107  #Norman
  , 27087   #Polk
)

# Create an empty plot as a spacer use in cowplot for creating gap between plot and table
spacer <- ggplot2::ggplot() + ggplot2::theme_void()


# Functions ---------------------------------------------------------------

gbFun_countyFilter <- function(df, column) 
{
  df |> dplyr::filter({{ column }} %in% gbVal)
}

# Function Plot CDC Places*** ------------------------------------------------
# Function to create the plot for each location
# It will create a plot than under it will be two tables than a caption

library(patchwork) 

gbFun_CDCPlacesPlot <- function(data, colorScheme, shapeScheme) { #Not all measures have the same year so we define it with the function
  measure <- data$measure[1]
  yr <- data$brfssYr
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = reorder(locationname, -totalpopulation)  # Order by tot_population in descending order
      , y = data_value
      , color = data_value_type
    )
  ) +
    ggplot2::geom_point(size = 3,  position = ggplot2::position_dodge(width = 0.5)) + # Adjust the size of the points
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = low_confidence_limit,
        ymax = high_confidence_limit
      )
      , width = 0.2
      , linewidth = 1.5  # Adjust the thickness of the error bars
      , position = ggplot2::position_dodge(width = 0.5)  # Dodge the error bars
    ) +
    ggplot2::labs(
      title = paste(measure, yr),
      x = "",
      y = "Prevalence",
      color = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = colorScheme) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
    )
  
  
  # Split the data into two tables based on data_value_type
  table_data_age_adjusted <- data |> 
    dplyr::filter(datavaluetypeid == "AgeAdjPrv") |> 
    dplyr::arrange(-totalpopulation) |> 
    dplyr::mutate(prevelenceType = "Age-Adjusted") |> 
    dplyr::select("Type" = prevelenceType, Location = locationname, "Low CI" = low_confidence_limit, Prevalence = data_value, "High CI" = high_confidence_limit)
  
  table_data_crude <- data |> 
    dplyr::filter(datavaluetypeid == "CrdPrv") |> 
    dplyr::arrange(-totalpopulation) |>
    dplyr::mutate(prevelenceType = "Crude") |> 
    dplyr::select("Type" = prevelenceType, Location = locationname, "Low CI" = low_confidence_limit, Prevalence = data_value, "High CI" = high_confidence_limit)
  
  # Create the table grobs
  table_age_adjusted <- gridExtra::tableGrob(
    table_data_age_adjusted
    , rows = NULL
    , theme = gridExtra::ttheme_default(base_size = 10)
    )
  table_crude <- gridExtra::tableGrob(
    table_data_crude
    , rows = NULL
    , theme = gridExtra::ttheme_default(base_size = 10)
    )
  
  # Add lines to the tables
  table_age_adjusted <- gtable::gtable_add_grob(
    table_age_adjusted,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_age_adjusted), l = 1, r = ncol(table_age_adjusted)
  )
  
  table_crude <- gtable::gtable_add_grob(
    table_crude,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_crude), l = 1, r = ncol(table_crude)
  )
  
  # Combine the tables side by side
  combined_tables <- cowplot::plot_grid(table_age_adjusted, table_crude, ncol = 2, rel_widths = c(1, 1))
  
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, combined_tables, ncol = 1, rel_widths = c(3, 2))
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "PLACES. Centers for Disease Control and Prevention. Accessed 11/12/2024. https://www.cdc.gov/places",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <- cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot County Health Ranking Years Potential Lost*** -------------------------------------
# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_CHRRYPLPlot <- function(data, colorScheme, shapeScheme, titleName, yAxisTitle) { 
 maxValue <- max(unique(data$rawvalue), na.rm = TRUE)
  data <-  data |> 
  dplyr::mutate(start_year = as.numeric(sub("-.*", "", yearspan))) |> 
    dplyr::arrange(desc(start_year)) |> 
    dplyr::filter(start_year >= max(start_year)-5, state != "US")  # Select the 5 most recent year spans
  
  p <- data |> 
    ggplot2::ggplot(
      ggplot2::aes(
          x = yearspan  
          , y = rawvalue
          , color = reorder(county, fips) # Order by fip code
          , shape = reorder(county, fips) # Assign different shapes
          , fill = reorder(county, fips)  # Fill is required to fill shape's color
          , group = reorder(county, fips) # Required to connect the rawvalue dots
        )
      ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = yAxisTitle
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
      ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
      ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
      )+ # Define shapes
    ggplot2::scale_y_continuous(
      breaks = seq(0, maxValue, by = 3000)  # Increment by 1000
      , labels = function(x) paste0("1:", x)  # Prepend "1:" to y-axis values
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
      )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(start_year == max(start_year)) |> 
    dplyr::arrange(fips) |>
    dplyr::mutate(
      rawValue = paste0("1:", rawvalue)
      , cilow = paste0("1:", cilow)
      , cihigh = paste0("1:", cihigh) 
      ) |> 
    dplyr::select(
      "Date Range" = yearspan
      , location = county
      , "Lower CI" = cilow
      , Value = rawValue
      , "Upper CI" = cihigh
    )
  
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )

  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }

  # Create the caption as a text grob
  caption <- grid::textGrob(
    "University of Wisconsin Population Health Institute. County Health Rankings & Roadmaps 2024. www.countyhealthrankings.org. ",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot County Health Ranking PCP*** -------------------------------------
# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_CHRRPCPPlot <- function(data, colorScheme, shapeScheme, titleName, yAxisTitle) { 
  
  maxValue <- max(unique(data$rawvalue), na.rm = TRUE)
  data <-  data |> 
    dplyr::mutate(start_year = as.numeric(sub("-.*", "", yearspan))) |> 
    dplyr::arrange(desc(start_year)) |> 
    dplyr::filter(start_year >= max(start_year)-5, state != "US")  # Select the 5 most recent year spans
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = yearspan  
      , y = rawvalue
      , color = reorder(county, fips) # Order by fip code
      , shape = reorder(county, fips) # Assign different shapes
      , fill = reorder(county, fips)  # Fill is required to fill shape's color
      , group = reorder(county, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    # CI is not included in the plot because it makes it to busy
    # ggplot2::geom_errorbar(
    #   ggplot2::aes(
    #     ymin = cilow,
    #     ymax =cihigh
    #   )
    #   , width = 0.2
    #   , linewidth = 1.5  # Adjust the thickness of the error bars
    #   , position = ggplot2::position_dodge(width = 0.5)  # Dodge the error bars
    # ) +
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = yAxisTitle
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_y_continuous(
       labels = function(x) paste0(x, ": 1")  # Append "1:" to y-axis values
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(start_year == max(start_year)) |> 
    dplyr::arrange(fips) |>
    dplyr::mutate(rawValue = paste0(rawvalue, ": 1")) |> 
    dplyr::select(
      "Date Range" = yearspan
      , location = county
      , Value = rawValue
    )
  
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "University of Wisconsin Population Health Institute. County Health Rankings & Roadmaps 2024. www.countyhealthrankings.org. ",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot County Health Ranking Dentist*** -------------------------------------
# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_CHRRDentistPlot <- function(data, colorScheme, shapeScheme, titleName, yAxisTitle) { 
  
  maxValue <- max(unique(data$rawvalue), na.rm = TRUE)
  data <-  data |> 
    dplyr::mutate(start_year = as.numeric(sub("-.*", "", yearspan))) |> 
    dplyr::arrange(desc(start_year)) |> 
    dplyr::filter(start_year >= max(start_year)-5, state != "US")  # Select the 5 most recent year spans
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = yearspan  
      , y = rawvalue
      , color = reorder(county, fips) # Order by fip code
      , shape = reorder(county, fips) # Assign different shapes
      , fill = reorder(county, fips)  # Fill is required to fill shape's color
      , group = reorder(county, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    # CI is not included in the plot because it makes it to busy
    # ggplot2::geom_errorbar(
    #   ggplot2::aes(
    #     ymin = cilow,
    #     ymax =cihigh
    #   )
    #   , width = 0.2
    #   , linewidth = 1.5  # Adjust the thickness of the error bars
    #   , position = ggplot2::position_dodge(width = 0.5)  # Dodge the error bars
    # ) +
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = yAxisTitle
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(x, ": 1")  # Append "1:" to y-axis values
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(start_year == max(start_year)) |> 
    dplyr::arrange(fips) |>
    dplyr::mutate(rawValue = paste0(rawvalue, ": 1")) |>
    dplyr::select(
      "Date Range" = yearspan
      , location = county
      , Value = rawValue
    )
  
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "University of Wisconsin Population Health Institute. County Health Rankings & Roadmaps 2024. www.countyhealthrankings.org. ",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}


# Function Plot Food Shelf ------------------------------------------------
# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_foodShelfPlot <- function(data, colorScheme, shapeScheme, titleName) { 
  measureName <- data$measure[1]
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = rawValue
      , color = reorder(county, fips) # Order by fip code
      , shape = reorder(county, fips) # Assign different shapes
      , fill = reorder(county, fips)  # Fill is required to fill shape's color
      , group = reorder(county, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
        breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma  # Format y-axis values with commas
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::group_by(fips) |> 
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (rawValue[year == max(year)] - rawValue[year == min(year)]) 
            / rawValue[year == min(year)] # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 2
      )
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!measureName := formattable::comma(rawValue,0)
      ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = county
      , !!measureName
      , "% Difference" = percentageDifference
    )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Hunger Solutions. (n.d.). Programs. Hunger Solutions. Retrieved November 16, 2024, from https://www.hungersolutions.org/",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}


# Function Plot Public Health Data Access Portal Immunization -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_immunizationPlot <- function(data, colorScheme, shapeScheme, titleName) { 
  vaccineName <- data$vaccine[1]
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = percent
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
        labels = scales::percent  # Format y-axis values with percent
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
      , panel.spacing = ggplot2::unit(1.5, "lines")  # Adjust the space between facet
    ) +
    ggplot2::facet_wrap(~vaccine, ncol = 2)
  
  # Create a data table
  # table_data <- data |>
  #   dplyr::filter(year >= max(year)-1) |>
  #   dplyr::arrange(fips, year) |>
  #   dplyr::group_by(fips) |> 
  #   dplyr::mutate(
  #     percentageDifference = formattable::percent(
  #       ifelse(
  #         year == max(year)
  #         , (percent[year == max(year)] - percent[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
  #         ,NA
  #       )
  #       , 1
  #     )
  #   ) |>
  #   dplyr::ungroup() |> 
  #    dplyr::mutate( 
  #      !!vaccineName := formattable::percent(percent, 1)
  #    ) |>   # Create a new column with the dynamic name
  #   dplyr::select(
  #     "Year" = year
  #     , Location = location
  #     , !!vaccineName
  #     , "% Change" = percentageDifference
  #   )
  # 
  # # Define a custom theme for the table
  # custom_theme <- gridExtra::ttheme_default(
  #   core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
  #   colhead = list(fg_params = list(fontsize = 8)),
  #   rowhead = list(fg_params = list(fontsize = 8))
  # )
  # 
  # # Create the table grobs
  # table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  # 
  # # Add lines to the tables
  # table_data <- gtable::gtable_add_grob(
  #   table_data,
  #   grobs = grid::segmentsGrob(
  #     x0 = grid::unit(0, "npc"),
  #     y0 = grid::unit(0, "npc"),
  #     x1 = grid::unit(1, "npc"),
  #     y1 = grid::unit(0, "npc"),
  #     gp = grid::gpar(lwd = 2)
  #   ),
  #   t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  # )
  # 
  # # Combine the plot and the tables side by side using cowplot
  # if (knitr::is_html_output()) {
  #   # Combine the plot and the tables side by side using cowplot
  #   combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  # } else {
  #   combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  # }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2015-2023). Immunizations query:Childhood immunizations.\nAccessed from MN Public Health Access Data portal https://data.web.health.state.mn.us/immunizations-query",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(p, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}


# Function Plot Public Health Data Access Portal Dental Service*** -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_dentalPlot <- function(data, colorScheme, shapeScheme, titleName) { 
  lbName <- "Recipients"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = dentalServicePercent
      , color = reorder(geography, fips) # Order by fip code
      , shape = reorder(geography, fips) # Assign different shapes
      , fill = reorder(geography, fips)  # Fill is required to fill shape's color
      , group = reorder(geography, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with geography
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::group_by(fips) |> 
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (dentalServicePercent[year == max(year)] - dentalServicePercent[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 1
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!lbName := formattable::percent(dentalServicePercent, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = geography
      , !!lbName
      , "% Change" = percentageDifference
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2012-2020). Oral Health query:Medicaid dental service use.\nAccessed from MN Public Health Access Data portal https://data.web.health.state.mn.us/medicaid-dental-service-use-query",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}




# Function Plot MDE Third Grade Proficiency*** -------------
  
  # Function to create the plot for each location
  # It will create a plot than under it will be one tables than a caption
  # titleName is required because the measure doesn't always match the data
  gbFun_mdeThirdGradeProficiencyPlot <- function(data, colorScheme, shapeScheme, titleName, lblName) { 
    lbName <- lblName
    minYr <- min(data$year)
    maxYr <- max(data$year)
    
    p <- ggplot2::ggplot(
      data, ggplot2::aes(
        x = year  
        , y = percentage
        , color = reorder(location, fips) # Order by fip code
        , shape = reorder(location, fips) # Assign different shapes
        , fill = reorder(location, fips)  # Fill is required to fill shape's color
        , group = reorder(location, fips) # Required to connect the rawvalue dots
      )
    ) +
      ggplot2::geom_point(size = 3) + # Adjust the size of the points
      ggplot2::geom_line() +  # Connect the points with lines
      ggplot2::labs(
        title = titleName # Title passed in function
        , x = ""
        , y = ""
        , color = ""
      ) +
      ggplot2::theme_minimal() +
      #The name for color, fill, and shape have to be the same otherwise, 
      # there will be legends for each of them
      # I just set it to NA since this is easy to remember if I am not going to show the legend
      ggplot2::scale_color_manual(
        name = NA
        , values = colorScheme
      ) +
      ggplot2::scale_fill_manual(
        name = NA
        ,values = colorScheme
      ) + # Required to fill in some shapes
      ggplot2::scale_shape_manual(
        name = NA
        , values = shapeScheme
      )+ # Define shapes
      ggplot2::scale_x_continuous(
        breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent  # Format y-axis values with geography
      ) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(face = "bold", size = 10)  # Make y-axis values bold
        , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)  # Rotate x-axis labels for better readability
        , legend.title = ggplot2::element_blank()  # Hide the legend title
        , legend.position = "bottom"
      )
    
    # Create a data table
    table_data <- data |>
      dplyr::filter(year >= max(year)-1) |>
      dplyr::arrange(fips, year) |>
      dplyr::group_by(fips) |> 
      dplyr::mutate(
        percentageDifference = formattable::percent(
          ifelse(
            year == max(year)
            , (percentage[year == max(year)] - percentage[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
            ,NA
          )
          , 1
        )
      ) |>
      dplyr::ungroup() |> 
      dplyr::mutate( 
        !!lbName := formattable::percent(percentage, 1)
      ) |>   # Create a new column with the dynamic name
      dplyr::select(
        "Year" = year
        , Location = location
        , !!lbName
        , "% Change" = percentageDifference
      )
    
    # Define a custom theme for the table
    custom_theme <- gridExtra::ttheme_default(
      core = list(fg_params = list(fontsize = 7)),  # Adjust the font size here
      colhead = list(fg_params = list(fontsize = 7)),
      rowhead = list(fg_params = list(fontsize = 7))
    )
    
    # Create the table grobs
    table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
    
    # Add lines to the tables
    table_data <- gtable::gtable_add_grob(
      table_data,
      grobs = grid::segmentsGrob(
        x0 = grid::unit(0, "npc"),
        y0 = grid::unit(0, "npc"),
        x1 = grid::unit(1, "npc"),
        y1 = grid::unit(0, "npc"),
        gp = grid::gpar(lwd = 2)
      ),
      t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
    )
    
    # Combine the plot and the tables side by side using cowplot
    if (knitr::is_html_output()) {
      # Combine the plot and the tables side by side using cowplot
      combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
    } else {
      combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
    }
    
    # # Create the caption as a text grob
    caption <- grid::textGrob(
      "Minnesota Early Childhood Longitudinal Data System. (2017-2023). 3rd Grade Proficiency. https://eclds.mn.gov/#thirdGradeEdStatus/"
      , x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
    )

    # Combine the plot, tables, and caption
    final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
    
    print(final_plot)
  }

# Function Plot DEED Unemployment Rate*** -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_deedUmemploymentPlot <- function(data, colorScheme, shapeScheme, titleName) { 
  lbName <- "Unemployment Rate"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = rate
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with geography
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::group_by(fips) |> 
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (rate[year == max(year)] - rate[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 1
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!lbName := formattable::percent(rate, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Geography = location
      , !!lbName
      , "% Change" = percentageDifference
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 7)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 7)),
    rowhead = list(fg_params = list(fontsize = 7))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Employment and Economic Development (2015-2023). Minnesota Unemployment Statistics LAUS (Local Area Unemployment Statistics) Data.\nhttps://apps.deed.state.mn.us/lmi/laus/Default.aspx",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )

  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot PNM Child and Teen Checkup-------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_childTeenCheckupPlot <- function(data, colorScheme, shapeScheme, ageGroup) { 
  lbName <- ifelse(
    data$ageGroup[1] == "0-20" 
    , "CTC Participation Ratio Ages 0-20 per EPSDT Schedule (Healthcare)-line 10\nEligible/Received one initial or periodic screening"
    , "CTC Total Eligibles Receiving Preventative Dental Services Ages 3-20 per EPSDT schedule (Line 12b/8)"
  )
  titleLbl <- paste("Child and Teen Check-Up / EPSDT Schedule (", data$ageGroup[1], " Years Old)")
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = ratio
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleLbl 
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with geography
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |> 
    dplyr::mutate(ratio = formattable::percent(ratio,0)) |> 
    dplyr::select(
      "Year" = year
      , Geography = location
      , Ratio = ratio
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(lbName, x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )

  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot Kids Count Percentage of Mothers Who Smoke -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_kidsCountMomSmokePlot <- function(data, colorScheme, shapeScheme) { 
  titleName <- "Percentage of Mothers Who Smoked During Pregnancy"
  tableName <- "% of Mothers Who Smoked"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = data
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with percent
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::group_by(fips) |> 
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (data[year == max(year)] - data[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 1
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!tableName := formattable::percent(data, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = location
      , !!tableName
      , "% Change" = percentageDifference
    )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "The Annie E. Casey Foundation, KIDS COUNT Data Centerhttps://datacenter.aecf.org/data/tables/1822-births-to-mother-who-smoked-during-pregnancy?/nloc=25&loct=5#detailed/5/3827-3913/false/1095,2048,574,1729,37,871,870,573,869,133/any/3851"
   , x = 0, hjust = 0, gp = grid::gpar(fontsize = 8)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot Kids Count PNC*** -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_kidsCountMomPncPlot <- function(data, colorScheme, shapeScheme) { 
  titleName <- "Late or Inadequate Prenatal Care for Mothers"
  tableName <- "Late/Inadequate\nPNC"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = data
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with percent
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::group_by(fips) |> 
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (data[year == max(year)] - data[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 1
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!tableName := formattable::percent(data, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = location
      , !!tableName
      , "% Change" = percentageDifference
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "The Annie E. Casey Foundation, KIDS COUNT Data Center,\nhttps://datacenter.aecf.org/rawdata.axd?ind=1823&loc=25",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 8)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot MDH NAS -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_mdhNASPlot <- function(data, colorScheme, shapeScheme) { 
  titleName <- "Neonatal Abstinence Syndrome 2016-2022"
  tableRate <- "NAS Rate 2016-2022"
  tableTotal <- "NAS Total 2016-2022"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- data |> 
    dplyr::filter(year != 1, fips != 99) |> #Because these are counts MN will make it to hard to see 
      ggplot2::ggplot(
        ggplot2::aes(
          x = year  
          , y = ct
          , color = reorder(location, fips) # Order by fip code
          , shape = reorder(location, fips) # Assign different shapes
          , fill = reorder(location, fips)  # Fill is required to fill shape's color
          , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year == 1) |> #Only want the total for the 5 year span
    dplyr::arrange(fips, year) |>
    dplyr::mutate( 
      !!tableTotal := ct
      , !!tableRate := rate2016_2022
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      Location = location
      , !!tableTotal
      , !!tableRate
    )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2023). Neonatal Abstinence Syndrome (NAS) DATA BRIEF: STATEWIDE AND COUNTY TRENDS, 2016-2022.\n
    https://www.health.state.mn.us/communities/opioids/documents/2023nasdatabrief.pdf.\n
    Retrieved November 2024.",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 8)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}


# Function Plot MDH Nonfatal Drug Overdose***-------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_mdhNonfatalDrugPlot <- function(data, colorScheme, shapeScheme) { 
  titleName <- "Nonfatal Drug Overdose Age-Adjusted Rate (per 1,000 Residents)"
  tableRate <- "Age-Adjusted Rate"
  tableTotal <- "Count"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = year  
        , y = age_adjustedRatePer1000
        , color = reorder(location, fips) # Order by fip code
        , shape = reorder(location, fips) # Assign different shapes
        , fill = reorder(location, fips)  # Fill is required to fill shape's color
        , group = reorder(location, fips) # Required to connect the rawvalue dots
      )
    ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::mutate( 
      !!tableTotal := ct
      , !!tableRate := age_adjustedRatePer1000
      , Total = year
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      Year = year
      , Location = location
      , !!tableTotal
      , !!tableRate
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2023). Nonfatal Drug Overdose Dashboard. https://www.health.state.mn.us/communities/opioids/data/nonfataldata.html",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 8)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot MDH Minnesota Student Survey -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_mdhMss <- function(data, colorScheme, shapeScheme, titleName) { 
  lbName <- "MSS Indicator Pct"
   minYr <- min(data$year, na.rm = TRUE)
   maxYr <- max(data$year, na.rm = TRUE)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = pct
      , color = reorder(geography, fips) # Order by fip code
      , shape = reorder(geography, fips) # Assign different shapes
      , fill = reorder(geography, fips)  # Fill is required to fill shape's color
      , group = reorder(geography, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with geography
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-3) |>
    dplyr::arrange(fips, year) |>
    dplyr::group_by(fips) |> 
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (pct[year == max(year)] - pct[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 1
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!lbName := formattable::percent(pct, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , geography = geography
      , !!lbName
      , "% Change" = percentageDifference
    )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Education. (2024). Minnesota Student Survey Tables 2013-2022. https://public.education.mn.gov/MDEAnalytics/DataTopic.jsp?TOPICID=11",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot MDH HIV STI*** -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_mdhStiHIV <- function(data, colorScheme, shapeScheme, titleName, tableName) { 
  lbName <- tableName
  minYr <- min(data$year, na.rm = TRUE)
  maxYr <- max(data$year, na.rm = TRUE)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = value
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma  # Format y-axis values with location
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::mutate( 
      !!lbName := formattable::comma(value, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = location
      , !!lbName
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2024). STI Statistics. https://www.health.state.mn.us/diseases/stds/stats/index.html"
    , x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot Minnesota Board of Pharmacy PMP -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_boardOfPharmacyPmpPlot <- function(data, colorScheme, shapeScheme, titleName, tableName) { 
  lbName <- tableName
  minYr <- min(data$year, na.rm = TRUE)
  maxYr <- max(data$year, na.rm = TRUE)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = value
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma  # Format y-axis values with location
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::mutate( 
      !!lbName := formattable::comma(value, 1)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = location
      , !!lbName
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Board of Pharmacy. (2024). Prescription Monitoring Program. https://mn.gov/boards/pharmacy-pmp/reports/data-dashboard.jsp"
    ,x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}


# Function Plot public use files -------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_pufPlot <- function(data, colorScheme, shapeScheme, titleName, tableName) { 
  lbName <- tableName
  minYr <- min(data$year, na.rm = TRUE)
  maxYr <- max(data$year, na.rm = TRUE)
  
  p <- ggplot2::ggplot(
    data, ggplot2::aes(
      x = year  
      , y = optimal_care_rate
      , color = reorder(location, fips) # Order by fip code
      , shape = reorder(location, fips) # Assign different shapes
      , fill = reorder(location, fips)  # Fill is required to fill shape's color
      , group = reorder(location, fips) # Required to connect the rawvalue dots
    )
  ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma  # Format y-axis values with location
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year >= max(year)-1) |>
    dplyr::arrange(fips, year) |>
    dplyr::mutate( 
      !!lbName := formattable::comma(optimal_care_rate, 2)
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Location = location
      , !!lbName
    )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Statewide Quality Reporting and Measurement System Public Use File,\n
    Minnesota Department of Health, [2018-2022]. https://www.health.state.mn.us/data/hcquality/pufs.html"
    ,x = 0, hjust = 0, gp = grid::gpar(fontsize = 10)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}


# Function Plot MDH Fatal Drug Overdose-------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_mdhFatalDrugPlot <- function(data, colorScheme, shapeScheme) { 
  titleName <- "Number of Fatal Drug Overdoses by County of Residence"
  tableTotal <- "Total"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- data |> dplyr::filter(year != 1, fips != 27000) |> #State of MN overwhelms plot
    ggplot2::ggplot(
      ggplot2::aes(
        x = year  
        , y = ct
        , color = reorder(location, fips) # Order by fip code
        , shape = reorder(location, fips) # Assign different shapes
        , fill = reorder(location, fips)  # Fill is required to fill shape's color
        , group = reorder(location, fips) # Required to connect the rawvalue dots
      )
    ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.position = "bottom"
    )
  
  # Create a data table
  table_data <- data |>
    dplyr::filter(year == 1) |>
    dplyr::arrange(fips, year) |>
    dplyr::mutate( 
      !!tableTotal := ct
      , "Time Range" = "2014-2023"
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Time Range"
      , Location = location
      , !!tableTotal
    )
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
    # Combine the plot and the tables side by side using cowplot
    combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
  } else {
    combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
  }
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2024). COUNTY - LEVEL DRUG OVERDOSE DEATHS FROM 2014 - 2023. https://www2cdn.web.health.state.mn.us/communities/opioids/documents/countytotals.pdf",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 8)
  )
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

# Function Plot MDH WIC Breastfeeding***-------------

# Function to create the plot for each location
# It will create a plot than under it will be one tables than a caption
# titleName is required because the measure doesn't always match the data
gbFun_mdhWicBFPlot <- function(data, colorScheme, shapeScheme) { 
  titleName <- "P-N-M Breastfeeding Initiation and Duration Among Infants in the Minnesota WIC Program"
  tableLbl <- "Pct"
  minYr <- min(data$year)
  maxYr <- max(data$year)
  
  p <- data |> dplyr::filter(measure != "# infants") |> 
    dplyr::mutate(measureFc = factor(measure, levels = unique(measure))) |>  # Convert measure to factor
    ggplot2::ggplot(
      ggplot2::aes(
        x = year  
        , y = measureValue
        , color = reorder(measureFc, measure) # Order by fip code
        , shape = reorder(measureFc, measure) # Assign different shapes
        , fill = reorder(measureFc, measure)  # Fill is required to fill shape's color
        , group = reorder(measureFc, measure) # Required to connect the rawvalue dots
      )
    ) +
    ggplot2::geom_point(size = 3) + # Adjust the size of the points
    ggplot2::geom_line() +  # Connect the points with lines
    ggplot2::labs(
      title = titleName # Title passed in function
      , x = ""
      , y = ""
      , color = ""
    ) +
    ggplot2::theme_minimal() +
    #The name for color, fill, and shape have to be the same otherwise, 
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(
      name = NA
      , values = colorScheme
    ) +
    ggplot2::scale_fill_manual(
      name = NA
      ,values = colorScheme
    ) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(
      name = NA
      , values = shapeScheme
    )+ # Define shapes
    ggplot2::scale_x_continuous(
      breaks = seq(minYr, maxYr, by = 1)  # Ensure all years from 2015 to 2021 are shown
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent  # Format y-axis values with measureValues
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold")  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
    ) 
  # Create a data table
  table_data <- data |>
    dplyr::filter(measure != "# infants", year >= max(year) -1) |>
    dplyr::arrange(measureOrder, year) |>
    dplyr::group_by(measure) |>
    dplyr::mutate(
      percentageDifference = formattable::percent(
        ifelse(
          year == max(year)
          , (measureValue[year == max(year)] - measureValue[year == min(year)]) # *100 I don't need to times by 100 since I wrap it in formttable
          ,NA
        )
        , 1
      )
    ) |>
    dplyr::ungroup() |> 
    dplyr::mutate( 
      !!tableLbl := formattable::percent(measureValue,1)
      , Measure = measure
    ) |>   # Create a new column with the dynamic name
    dplyr::select(
      "Year" = year
      , Measure
      , !!tableLbl
      , "% Change" = percentageDifference
    )
  
  
  # Define a custom theme for the table
  custom_theme <- gridExtra::ttheme_default(
    core = list(fg_params = list(fontsize = 8)),  # Adjust the font size here
    colhead = list(fg_params = list(fontsize = 8)),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  
  # Create the table grobs
  table_data <- gridExtra::tableGrob(table_data, rows = NULL, theme = custom_theme)
  
  # Add lines to the tables
  table_data <- gtable::gtable_add_grob(
    table_data,
    grobs = grid::segmentsGrob(
      x0 = grid::unit(0, "npc"),
      y0 = grid::unit(0, "npc"),
      x1 = grid::unit(1, "npc"),
      y1 = grid::unit(0, "npc"),
      gp = grid::gpar(lwd = 2)
    ),
    t = 1, b = nrow(table_data), l = 1, r = ncol(table_data)
  )
  
  # Combine the plot and the tables side by side using cowplot
  if (knitr::is_html_output()) {
  # Combine the plot and the tables side by side using cowplot
  combined <- cowplot::plot_grid(p, table_data, ncol = 1, rel_widths = c(3, 2))  # HTML Output
} else {
  combined <- cowplot::plot_grid(p, table_data, ncol = 2, rel_widths = c(3,  1))  # Add spacer with rel_widths
}
  
  # Create the caption as a text grob
  caption <- grid::textGrob(
    "Minnesota Department of Health. (2024). WIC Breastfeeding Summary. https://www.health.state.mn.us/people/wic/localagency/reports/bf/annual.html",
    x = 0, hjust = 0, gp = grid::gpar(fontsize = 8)
  )
  
  
  # Combine the plot, tables, and caption
  final_plot <-  cowplot::plot_grid(combined, caption, ncol = 1, rel_heights = c(10, 1))
  
  print(final_plot)
}

