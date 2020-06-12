.. raw:: latex

    \clearpage


.. _printing_copying:

Printing and Copying
====================


*This chapter describes how to print, copy to the Windows clipboard,
or copy to file the contents of the currently active window in the
EPANET workspace. This can include the network map, a graph, a table,
a report, or the properties of an object selected from the Browser.*

-------

.. _sec-select_printer:

Selecting a Printer
~~~~~~~~~~~~~~~~~~~

   To select a printer from among your installed Windows printers and
   set its properties:

    1. Select **File >> Page Setup** from the main menu.

    2. Click the **Printer** button on the Page Setup dialog that appears
       (see :numref:`fig-Page_Setup`).

    3. Select a printer from the choices available in the combo box in the
       next dialog that appears.

    4. Click the **Properties** button to select the printer's properties
       (which vary with choice of printer).

    5. Click **OK** on each dialog box to accept your selections.

   .. _fig-Page_Setup:
   .. figure:: media/image91.png
      :alt: Page Setup in EPANET
   
      Page Setup Dialog.
   ..

.. _sec-set_page:

Setting the Page Format
~~~~~~~~~~~~~~~~~~~~~~~

   The Page Setup dialog menu screen is shown in :numref:`fig-Page_Setup`.  To format the printed page:

    1. Select **File >> Page Setup** from the main menu.

    2. Use the Margins page of the Page Setup dialog form that appears
       (:numref:`fig-Page_Setup`) to:

       -  Select a printer

       -  Select the paper orientation (Portrait or Landscape)

       -  Set left, right, top, and bottom margins

    3. Use the Headers/Footers page of the dialog box to:

       -  Supply the text for a header that will appear on each page

       -  Indicate whether the header should be printed or not

       -  Supply the text for a footer that will appear on each page

       -  Indicate whether the footer should be printed or not

       -  Indicate whether or not pages should be numbered

    4. Click **OK** to accept your choices.

 .. _sec-print_preview:
   
Print Preview
~~~~~~~~~~~~~

   To preview a printout, select **File >> Print Preview** from the main
   menu. A Preview form will appear which shows how each page of the
   object being printed will appear when printed.


 .. _sec-print_current_view:

Printing the Current View
~~~~~~~~~~~~~~~~~~~~~~~~~

   To print the contents of the current window being viewed in the
   EPANET workspace select **File >> Print** from the main menu or click
   |image141| on the Standard Toolbar. The following views can be
   printed:

    -  Data Browser (properties of the currently selected object)

    -  Network Map (at the current zoom level)

    -  Graphs (Time Series, Profile, Contour, Frequency and System Flow
       plots)

    -  Tables (Network and Time Series tables)

    -  Status, Energy, Calibration, and Reaction Reports.

 .. _sec-copy_clipboard:

Copying to the Clipboard or to a File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   EPANET can copy the text and graphics of the current window being
   viewed to both the Windows clipboard and to a file. The Copy Network Map 
   menu screen is shown in :numref:`fig-CopyMap` below. Views that can be
   copied in this fashion include the Network Map, graphs, tables, and
   reports. To copy the current view to the clipboard or to file:

    1. Select **Edit >> Copy To** from the main menu or click |image142|.

    2. Select choices from the Copy dialog box that appears
       and click its **OK** button.

    3. If you selected to copy-to-file, enter the name of the file in the
       Save As dialog box that appears and click **OK**.


   .. _fig-CopyMap:
   .. figure:: media/image92.png
      :alt: Copy Network Map Dialog in EPANET

      Copy Network Map Dialog

   ..   
   
   Use the Copy dialog as follows to define how you want your data
   copied and to where:

    1. Select a destination for the material being copied (Clipboard or
       File)

    2. Select a format to copy in:

       -  Bitmap (graphics only)

       -  Metafile (graphics only)

       -  Data (text, selected cells in a table, or data used to construct a
          graph)

    3. Click **OK** to accept your selections or **Cancel** to cancel the
       copy request.

    

.. include:: image_subdefs.rst
