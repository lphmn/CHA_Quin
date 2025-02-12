


# Naming convention -------------------------------------------------------

# gb is short for global
# Val is short for value
# Fun is short for Function

# Values ---------------------------------------------------------------

# Defined color scheme so it can be used for entire report
gbVal_colorScheme <- c(
  # P-N-M
  # "#00295D", "#A9B849", "#D8C6A9", "#24ACE4", "#231f20",  "#831532",  
  
  
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
  , 26 # Filled star
  , 0 # Square
  , 1 # Circle
  , 2 # Triangle
  , 3 # Plus
  , 4 # Cross
  , 5 # Diamond
  , 6 # Inverted triangle
  , 7 # Square cross
  , 8 # Star
  , 9 # Diamond plus
  , 10 # Circle plus
  , 11 # Triangles up-down
  , 12 # Square plus
  , 13 # Circle cross
  , 14 # Square triangle
  , 15 # Filled square
  
)

gbVal_chbExpLbl <- "Quin County Community Health Services (CHS)" # More detailed label
gbVal_chbExpNoParLbl <- "Quin County Community Health Services" # Used in subtitle for ggplot function
gbVal_chbAbbrLbl <- "Quin County CHS" # Less detailed label

# This is where I define what county(ies) are the drivers for this CHA process
# It is based off the MN County FIPS code
gbVal = c(
  # Don't change the order of the first three values.
  # The values can be set to anything so if in the future a standard is developed it can be changed here 
  # If I want the US standard to be 12345689, just change the 9 to it and it will be updated in the report.
  9 # Defined by Patrick Olson for this report this is for US totals 
  , 99 #Defined by Patrick Olson for this report this is for State totals
  , 999 #Defined by Patrick Olson this is for CHB totals
  , 27069	# Kittson
  , 27089	# Marshall
  , 27113	# Pennington
  , 27125	# Red Lake
  , 27135	# Roseau
)



# Citation Generator (Value and Function (Not Perfect) --------------------
# This is required because rendering is different path than just running it in r
if (file.exists("./references.bib")) {
  bib_lines <- readLines("./references.bib") # Read the .bib file into a character vector
} else {
  bib_lines <- readLines("../references.bib") # Read the .bib file into a character vector
}


parse_bib <- function(bib_lines) { # Function to parse .bib entries
  entries <- list()  # Initialize an empty list to store entries
  current_entry <- NULL  # Variable to store the current entry being processed
  
  for (line in bib_lines) {  # Loop through each line in the .bib file
    if (grepl("^@", line)) {  # Check if the line starts with '@', indicating a new entry
      if (!is.null(current_entry)) {  # If there is a current entry being processed
        entries[[current_entry$key]] <- current_entry  # Add it to the entries list
      }
      current_entry <- list()  # Initialize a new entry
      current_entry$key <- sub("^@.*\\{([^,]+),", "\\1", line)  # Extract the entry key
    } else if (grepl("author", line)) {  # Check if the line contains 'author'
      authors <- sub(".*= \\{(.*)\\},", "\\1", line)  # Extract the authors
      current_entry$author <- strsplit(authors, " / ")[[1]]  # Split authors by '/'
    } else if (grepl("year", line)) {  # Check if the line contains 'year'
      year <- sub(".*= \\{(.*)\\},", "\\1", line)  # Extract the year
      current_entry$year <- year  # Add the year to the current entry
    }
  }
  
  if (!is.null(current_entry)) {  # Add the last entry to the list
    entries[[current_entry$key]] <- current_entry
  }
  
  return(entries)  # Return the list of entries
}

bib_entries <- parse_bib(bib_lines) # Parse the .bib file


generate_citation <- function(entry) { # Function to generate in-text citations
  # Remove curly braces and replace slashes with commas
  authors <- gsub("[{}]", "", entry$author)
  authors <- gsub("/", ",", authors)
  year <- entry$year
  citation <- paste(authors, " (", year, ")", sep = "")
  return(citation)
}

citations <- sapply(bib_entries, generate_citation) # Generate citations for all entries


for (entry in names(bib_entries)) { # Assign citations to variables based on keys
  assign(entry, citations[[entry]])
}


# Functions ---------------------------------------------------------------

gbFun_countyFilter <- function(df, column) 
{
  df |> dplyr::filter({{ column }} %in% gbVal)
}

library(patchwork)
library(DT)

# Function to create single year plot(s) for each location*** -------------

gbFun_plotSingleYrPDF <- function(
    par_data, par_x, par_xReorder1, par_y, par_toolTipTextHeader1, par_toolTipTextValue1, 
    par_toolTipTextHeader2, par_toolTipTextValue2, par_color, par_yMin, par_yMax, par_plotTitle, 
    par_plotXAxis, par_plotYAxis, par_plotColorScheme, par_fontSize) { 
  
  par_data |>
    # If data isn't arranged correctly 
    # plotly will have multiple tooltips (one for shape and one for line)
    # dplyr::arrange(!!rlang::sym(par_color), !!rlang::sym(par_x)) |>
    # dplyr::filter(!is.na(!!rlang::sym(par_y))) |> 
    ggplot2::ggplot( 
      ggplot2::aes(
        x = reorder(!!rlang::sym(par_x), !!rlang::sym(par_xReorder1))  
        , y = !!rlang::sym(par_y)
        , color = !!rlang::sym(par_color) 
        , text = paste0(
          !!rlang::sym(par_toolTipTextHeader1), !!rlang::sym(par_toolTipTextValue1),
          !!rlang::sym(par_toolTipTextHeader2), !!rlang::sym(par_toolTipTextValue2)
        ) #Used for plotly labels Defined when function is used
      )
    ) +
    ggplot2::geom_point(size = 3,  position = ggplot2::position_dodge(width = 0.5)) + # Adjust the size of the points
    ggplot2::geom_errorbar(ggplot2::aes(ymin = !!rlang::sym(par_yMin), ymax = !!rlang::sym(par_yMax))
      , width = 0.2
      , linewidth = 1.5  # Adjust the thickness of the error bars
      , position = ggplot2::position_dodge(width = 0.5)  # Dodge the error bars
    ) +
    ggplot2::labs(
      title = par_plotTitle
      , subtitle = paste("Designed by:", gbVal_chbExpNoParLbl) 
      , x = par_plotXAxis
      , y = par_plotYAxis
      , color = ""
    ) +
    ggplot2::theme_minimal() + 
    ggplot2::scale_color_manual(values = par_plotColorScheme) +
    # ggplot2::scale_y_continuous(labels = par_scaleYContinuous) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = par_fontSize + 2, face = "bold")  # Title font size and style
      , plot.subtitle = ggplot2::element_text(size = par_fontSize-2, color = "lightgrey")  # Subtitle size and color
      , axis.title.x = ggplot2::element_text(size = par_fontSize-2, color = "lightgrey")  # X-axis title font size
      , axis.title.y = ggplot2::element_text(size = par_fontSize)  # Y-axis title font size
      , axis.text.y = ggplot2::element_text(size = par_fontSize)  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(hjust = 1, size = par_fontSize)  # Center x-axis labels
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.text = ggplot2::element_text(size = par_fontSize)  # Legend text font size
      , legend.position = "bottom"
    )
}

# Function to create trend plot(s) for each location*** -------------

gbFun_plotTrendPDF <- function(
    par_data, par_x, par_y, par_reorder1, par_reorder2, par_toolTipTextHeader1, 
    par_toolTipTextValue1, par_toolTipTextHeader2, par_toolTipTextValue2, par_toolTipTextHeader3,
    par_toolTipTextValue3, par_plotTitle, par_plotXAxis, 
    par_plotYAxis, par_plotMinYr, par_plotMaxYr, par_scaleYContinuous, 
    par_plotColorScheme, par_plotShapeScheme, par_fontSize) { 
  
  par_data |>
    # If data isn't arranged correctly 
    # plotly will have multiple tooltips (one for shape and one for line)
    dplyr::arrange(!!rlang::sym(par_x)) |>
 ggplot2::ggplot(
   ggplot2::aes(
     x = !!rlang::sym(par_x)  
     , y = !!rlang::sym(par_y)
     , group = reorder(!!rlang::sym(par_reorder1), !!rlang::sym(par_reorder2))
     , color = reorder(!!rlang::sym(par_reorder1), !!rlang::sym(par_reorder2))
     , shape = reorder(!!rlang::sym(par_reorder1), !!rlang::sym(par_reorder2))
     , fill = reorder(!!rlang::sym(par_reorder1), !!rlang::sym(par_reorder2))
     , text = paste0(
       !!rlang::sym(par_toolTipTextHeader1), !!rlang::sym(par_toolTipTextValue1),
       !!rlang::sym(par_toolTipTextHeader2), !!rlang::sym(par_toolTipTextValue2),
       !!rlang::sym(par_toolTipTextHeader3), !!rlang::sym(par_toolTipTextValue3)
       ) #Used for plotly labels Defined when function is used
     )
  ) +
    ggplot2::geom_point(size = 3)+   # Adjust the size of the points
    ggplot2::geom_line(show.legend = FALSE)+  # Connect the points with lines
    ggplot2::labs(
      title = par_plotTitle
      , subtitle = paste("Designed By:", gbVal_chbExpNoParLbl) #Since gbVale_chbExpNoParLbl at the begining of this script I don't have to pass it in the function everytime
      , x = par_plotXAxis
      , y = par_plotYAxis
      , color = ""
    ) +
    ggplot2::theme_minimal() + 
    # The name for color, fill, and shape have to be the same otherwise,
    # there will be legends for each of them
    # I just set it to NA since this is easy to remember if I am not going to show the legend
    ggplot2::scale_color_manual(name = NA, values = par_plotColorScheme) +
    ggplot2::scale_fill_manual(name = NA, values = par_plotColorScheme) + # Required to fill in some shapes
    ggplot2::scale_shape_manual(name = NA, values = par_plotShapeScheme) + # Define shapes
    ggplot2::scale_x_continuous(breaks = seq(par_plotMinYr, par_plotMaxYr, by = 1))+  # Ensure all years and increment by 1 year
    ggplot2::scale_y_continuous(labels = par_scaleYContinuous) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = par_fontSize + 2)  # Title font size and style
      , plot.subtitle = ggplot2::element_text(size = par_fontSize-2, color = "lightgrey")  # Subtitle size and color
      , axis.title.x = ggplot2::element_text(size = par_fontSize-2, color = "lightgrey")  # X-axis title font size
      , axis.title.y = ggplot2::element_text(size = par_fontSize)  # Y-axis title font size
      , axis.text.y = ggplot2::element_text(size = par_fontSize)  # Make y-axis values bold
      , axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = par_fontSize)  # Rotate x-axis labels for better readability
      , legend.title = ggplot2::element_blank()  # Hide the legend title
      , legend.text = ggplot2::element_text(size = par_fontSize)  # Legend text font size
      , legend.position = "bottom"
      , legend.background = ggplot2::element_rect(
          fill = "#E2E2E2",    # Background color
          color = "#FFFFFF",   # Border color
          size = 1             # Border width
        )
     )
 
}

# Function to create HTML Plotly Plot for each location*** -------------

gbFun_plotHTML <- function(ggPlot, plotTitle) { 
  ggPlot |>
  plotly::ggplotly(tooltip = c("text")) |>
    plotly::layout(
      margin = list(
        t=-10 #Moves the top margin of the top facet so the title moves up a little
      )
      , title = list(
        text = paste(plotTitle, "<br><sup><span style='color: lightgrey;'>Designed By:", gbVal_chbExpNoParLbl, "</span></sup></br>") # Add the subtitle here
            , x = 0.5
            , xanchor = 'center'
            )
      , legend = list(
        orientation = 'h',  # Horizontal orientation
        xanchor = 'center', # Center the legend with respect to the x position
        yanchor = 'bottom', # Anchor the legend at the bottom
        x = 0.5,            # Center the legend on the x-axis
        y = -0.5,           # Position the legend below the x-axis
        bgcolor = "#E2E2E2",
        bordercolor = "#FFFFFF",
        borderwidth = 1
        , title = list(text = "") # Hide the legend title It will return NA if this isn't defined
      )
     , xaxis = list(
       title = list(
         font = list(color = "lightgrey")  # Set X-axis title color to light grey
       )
     )
    ) |>
    # MS Copilot and the following link helped with the syntax below
    # https://stackoverflow.com/questions/49115629/specify-different-filename-for-newplot-png-in-plotly
    plotly::config(
      toImageButtonOptions = list(
        format = "png", # Specify the file format
        filename = plotTitle # Specify the file name
      )
    )
}

# Function to create PDF Kable Table for each location*** -------------
gbFun_tablePDF <- function(par_data, par_colList) {
  par_data |>  
  kableExtra::kbl(
    format = "latex"
    , linesep = "" #Remove space after fifth line https://stackoverflow.com/questions/45409750/get-rid-of-addlinespace-in-kable
    , booktabs = TRUE
    , col.names = c(par_colList)
    , align = "c"
    , digits = 1
    ) |> 
    kableExtra::kable_styling(
      latex_options = "striped"
       , font_size = 10 # Adjust the font size as needed
      )
}

# Function to create HTML DT Table with CSV Download*** -------------
gbFun_tableHTML <- function(par_data, par_colList, par_DownloadCaption) { 
  DT::datatable(
    par_data,
    colnames = colList,
    extensions = 'Buttons',
    options = list(
      dom = 'rtB',
      buttons = list(
        list(
          extend = 'csv',
          text = 'Download Table',
          filename = par_DownloadCaption
        )
      )
      , pageLength = -1  # Show all rows
      #Makes header lighgrey and centers header elements
      , headerCallback = DT::JS(
        "function(thead, data, start, end, display){",
        "  $(thead).find('th').css({ 'text-align': 'center', 'background-color': 'lightgrey' });",
        "}"
      )
      # Centers the values in the cells
      , columnDefs = list(list(targets = '_all', className = 'dt-center'))
    )
    , rownames = FALSE
  )
}