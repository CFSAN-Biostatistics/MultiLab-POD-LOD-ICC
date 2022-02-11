# UI elements for Notes & Data Entry Instructions

notes <- shinydashboard::box(
  width = 12, status = "info",
  title = span("Notes", class = "notes-title"),
  solidheader = FALSE, background = NULL,
  collapsible = TRUE, collapsed = FALSE,
  HTML(
    '
    <ul class = "notes-body">
      <li class = "notes-li">
        To cite this app in publications, please see the
        <strong>References</strong> tab.
      </li>
      <li class = "notes-li">
        Please do not use the back arrow in your browser. Use the
        refresh button instead.
      </li>
      <li class = "notes-li">
        If your experiment involves missing data, please use the
        <i>File Upload</i> option for data entry.
      </li>
      <li class = "notes-li">
        See <strong>Analysis Details</strong> in the sidebar
        for information about the models and R packages used.
      </li>
      <li class = "notes-li">
        The example data are obtained from Jarvis et al. (2019) and the
        Excel tool developed by Jarvis et al. Please see
        <strong>References</strong> in the sidebar for the full citation.
      </li>
    </ul>
    '
  )
)

data_entry_instructions <- shinydashboard::box(
  title = span("Data Entry Instructions", class = "notes-title"),
  width = 12, status = "info",
  solidheader = FALSE, background = NULL,
  collapsible = TRUE, collapsed = FALSE,
  span("Manual entry", class = "notes-subheading"),
  HTML(
    '
    <ol class = "notes-body">
      <li class = "notes-li">
        Enter the experiment name, etc. in the
        <strong>Experiment Description</strong> box.
      </li>
      <li class = "notes-li">
        In the first box under <strong>Lab-Level Data</strong>:
        <ol type = "a">
          <li class = "notes-li">
            Change the lab name if desired.
          </li>
          <li class = "notes-li">
            In the first column, enter the inoculation (dilution) levels
            in CFU/test portion. Please do not use CFU/g, etc.
            Click <em>Fill other labs</em> underneath the column to
            populate the remaining labs.
          </li>
          <li class = "notes-li">
            In the second column, enter the number of tubes inoculated at
            each level. Again, click <em>Fill other labs</em>
            if applicable.
          </li>
          <li class = "notes-li">
            In the third column, enter the number of positive tubes at
            each level.
          </li>
        </ol>
      </li>
      <li>
        Fill in the missing information for the remaining boxes (labs).
      </li>
    </ol>
    '
  ),
  span("File upload", class = "notes-subheading"),
  HTML(
    '
    <ol class = "notes-body">
      <li class = "notes-li">
        Download the Excel workbook template.
      </li>
      <li class = "notes-li">
        Add your data per the instructions in the template and save the
        file to your hard drive.
      </li>
      <li class = "notes-li">
        Upload the file and verify the data shown in the preview.
      </li>
    </ol>
    '
  ),
  span("Example data", class = "notes-subheading"),
  HTML(
    '
    <ul class = "notes-body">
      No data input required.
    </ul>
    '
  )
)

analysis_instructions <- shinydashboard::box(
  title = span("Analysis Instructions", class = "notes-title"),
  width = 12, status = "info",
  solidheader = FALSE, background = NULL,
  collapsible = TRUE, collapsed = FALSE,
  HTML(
    '
    <ol class = "notes-body">
      <li class = "notes-li">
        In the sidebar, choose the level for the confidence limits.
      </li>
      <li class = "notes-li">
        In the upper right corner, choose the LOD unit. Then click the
        <em>Calculate</em> button.
      </li>
      <li class = "notes-li">
        After the calculations are complete, you will be automatically
        re-directed to the <strong>Results</strong> page, which contains
        a button to download the results and two tabs:
      </li>
        <ol type = "a">
          <li class = "notes-li">
            The first tab gives estimates of model parameters, ICC, and LOD.
          </li>
          <li class = "notes-li">
            The second tab shows a plot of POD vs. inoculation level.
          </li>
        </ol>
    </ol>
    '
  )
)
