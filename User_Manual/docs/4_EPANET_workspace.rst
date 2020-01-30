.. raw:: latex

    \clearpage


.. _workspace:

EPANET's Workspace
==================


*This chapter discusses the essential features of EPANET’s workspace.
It describes the main menu bar, the tool and status bars, and the
three windows used most often – the Network Map, the Browser, and the
Property Editor. It also shows how to set program preferences.*

-------

.. _sec-overview:

Overview
~~~~~~~~

   The basic EPANET workspace is pictured in :numref:`fig-EPANET_Workspace` below. It consists of the
   following user interface elements: a Menu Bar, two Toolbars, a Status
   Bar, the Network Map window, a Browser window, and a Property Editor
   window. A description of each of these elements is provided in the
   sections that follow.

   .. _fig-EPANET_Workspace:
   .. figure:: media/image37.png
      :alt: EPANET Workspace
   
      EPANET workspace.
   ..

.. _sec-menubar:

Menu Bar
~~~~~~~~

   The Menu Bar located across the top of the EPANET workspace contains
   a collection of menus used to control the program. These include:

    -  File Menu

    -  Edit Menu

    -  View Menu

    -  Project Menu

    -  Report Menu

    -  Window Menu

    -  Help Menu


**File Menu**

   The File Menu contains commands for opening and saving data files and
   for printing.  The commands for the File Menu are shown in :numref:`table-File_Menu_Commands` below.

.. _table-File_Menu_Commands:
.. table:: File Menu Commands
   
  +---------------+------------------------------------------------------+
  | *COMMAND*     | *DESCRIPTION*                                        |
  +===============+======================================================+
  | New           | Creates a new EPANET project                         |
  +---------------+------------------------------------------------------+
  | Open          | Opens an existing project                            |
  +---------------+------------------------------------------------------+
  | Save          | Saves the current project                            |
  +---------------+------------------------------------------------------+
  | Save As       | Saves the current project under a different name     |
  +---------------+------------------------------------------------------+
  | Import        | Imports network data or map from a file              |
  +---------------+------------------------------------------------------+
  | Export        | Exports network data or map to a file                |
  +---------------+------------------------------------------------------+
  | Page Setup    | Sets page margins, headers, and footers for printing |
  +---------------+------------------------------------------------------+
  | Print Preview | Previews a printout of the current view              |
  +---------------+------------------------------------------------------+
  | Print         | Prints the current view                              |
  +---------------+------------------------------------------------------+
  | Preferences   | Sets program preferences                             |
  +---------------+------------------------------------------------------+
  | Exit          | Exits EPANET                                         |
  +---------------+------------------------------------------------------+
..

**Edit Menu**

   The Edit Menu contains commands for editing and copying.  The commands for the Edit Menu are shown in :numref:`table-Edit_Menu_Commands` below.
   
.. _table-Edit_Menu_Commands:
.. table:: Edit Menu Commands  

  +-----------------------------------+-----------------------------------+
  | *COMMAND*                         | *DESCRIPTION*                     |
  +===================================+===================================+
  | Copy To                           | Copies the currently active view  |
  |                                   | (map, report, graph or table) to  |
  |                                   | the clipboard or to file          |
  +-----------------------------------+-----------------------------------+
  | Select Object                     | Allows selection of an object on  |
  |                                   | the map                           |
  +-----------------------------------+-----------------------------------+
  | Select Vertex                     | Allows selection of link vertices |
  |                                   | on the map                        |
  +-----------------------------------+-----------------------------------+
  | Select Region                     | Allows selection of an outlined   |
  |                                   | region on the map                 |
  +-----------------------------------+-----------------------------------+
  | Select All                        | Makes the outlined region the     |
  |                                   | entire viewable map area          |
  +-----------------------------------+-----------------------------------+
  | Group Edit                        | Edits a property for the group of |
  |                                   | objects that fall within the      |
  |                                   | outlined region of the map        |
  +-----------------------------------+-----------------------------------+
..

**View Menu**

   The View Menu controls how the network map is viewed.  The controls for the View Menu are shown in :numref:`table-View_Menu_Controls` below.
   
.. _table-View_Menu_Controls:
.. table:: View Menu Controls     

  +--------------+-----------------------------------------------------------+
  | *COMMAND*    | *DESCRIPTION*                                             |
  +==============+===========================================================+
  | Dimensions   | Dimensions the map                                        |
  +--------------+-----------------------------------------------------------+
  | Backdrop     | Allows a backdrop map to be viewed                        |
  +--------------+-----------------------------------------------------------+
  | Pan          | Pans across the map                                       |
  +--------------+-----------------------------------------------------------+
  | Zoom In      | Zooms in on the map                                       |
  +--------------+-----------------------------------------------------------+
  | Zoom Out     | Zooms out on the map                                      |
  +--------------+-----------------------------------------------------------+
  | Full Extent  | Redraws the map at full extent                            |
  +--------------+-----------------------------------------------------------+
  | Find         | Locates a specific item on the map                        |
  +--------------+-----------------------------------------------------------+
  | Query        | Searches for items on the map that meet specific criteria |
  +--------------+-----------------------------------------------------------+
  | Overview Map | Toggles the Overview Map on/off                           |
  +--------------+-----------------------------------------------------------+
  | Legends      | Controls the display of map legends                       |
  +--------------+-----------------------------------------------------------+
  | Toolbars     | Toggles the toolbars on/off                               |
  +--------------+-----------------------------------------------------------+
  | Options      | Sets map appearance options                               |
  +--------------+-----------------------------------------------------------+
..

**Project Menu**

   The Project Menu includes commands related to the current project being analyzed.  The commands for the Project Menu are shown in :numref:`table-Project_Menu_Commands` below.
      
.. _table-Project_Menu_Commands:
.. table:: Project Menu Commmands     

  +-----------------------------------+-----------------------------------+
  | *COMMAND*                         | *DESCRIPTION*                     |
  +===================================+===================================+
  | Summary                           | Provides a summary description of |
  |                                   | the project's characteristics     |
  +-----------------------------------+-----------------------------------+
  | Defaults                          | Edits a project's default         |
  |                                   | properties                        |
  +-----------------------------------+-----------------------------------+
  | Calibration Data                  | Registers files containing        |
  |                                   | calibration data with the project |
  +-----------------------------------+-----------------------------------+
  | Analysis Options                  | Edits analysis options            |
  +-----------------------------------+-----------------------------------+
  | Run Analysis                      | Runs a simulation                 |
  +-----------------------------------+-----------------------------------+
..

**Report Menu**

   The Report menu has commands used to report analysis results in
   different formats. The commands for the Report Menu are shown in :numref:`table-Report_Menu_Commands` below.
      
.. _table-Report_Menu_Commands:
.. table:: Report Menu Commands     

  +-----------------------------------+-----------------------------------+
  | *COMMAND*                         | *DESCRIPTION*                     |
  +===================================+===================================+
  | Status                            | Reports changes in the status of  |
  |                                   | links over time                   |
  +-----------------------------------+-----------------------------------+
  | Energy                            | Reports the energy consumed by    |
  |                                   | each pump                         |
  +-----------------------------------+-----------------------------------+
  | Calibration                       | Reports differences between       |
  |                                   | simulated and measured values     |
  +-----------------------------------+-----------------------------------+
  | Reaction                          | Reports average reaction rates    |
  |                                   | throughout the network            |
  +-----------------------------------+-----------------------------------+
  | Full                              | Creates a full report of computed |
  |                                   | results for all nodes and links   |
  |                                   | in all time periods which is      |
  |                                   | saved to a plain text file        |
  +-----------------------------------+-----------------------------------+
  | Graph                             | Creates time series, profile,     |
  |                                   | frequency, and contour plots of   |
  |                                   | selected parameters               |
  +-----------------------------------+-----------------------------------+
  | Table                             | Creates a tabular display of      |
  |                                   | selected node and link quantities |
  +-----------------------------------+-----------------------------------+
  | Options                           | Controls the display style of a   |
  |                                   | report, graph, or table           |
  +-----------------------------------+-----------------------------------+
..

**Window Menu**

   The Window Menu contains the following commands shown in :numref:`table-Window_Menu_Commands` below.
      
.. _table-Window_Menu_Commands:
.. table:: Window Menu Commands     

  +-------------+-------------------------------------------------------------+
  | *COMMAND*   | *DESCRIPTION*                                               |
  +=============+=============================================================+
  | Arrange     | Rearranges all child windows to fit within the main window  |
  +-------------+-------------------------------------------------------------+
  | Close All   | Closes all open windows (except the Map and Browser)        |
  +-------------+-------------------------------------------------------------+
  | Window List | Lists all open windows; selected window currently has focus |
  +-------------+-------------------------------------------------------------+
..


**Help Menu**

   The Help Menu contains commands for getting help in using EPANET. The Help Menu contains the following commands shown in :numref:`table-Help_Menu_Commands` below. 
   Context-sensitive Help is also available by pressing the F1 key.
      
.. _table-Help_Menu_Commands:
.. table:: Help Menu Commands       

  +-------------+----------------------------------------------------------+
  | *COMMAND*   | *DESCRIPTION*                                            |
  +=============+==========================================================+
  | Help Topics | Displays the Help system's Help Topics dialog box        |
  +-------------+----------------------------------------------------------+
  | Units       | Lists the units of measurement for all EPANET parameters |
  +-------------+----------------------------------------------------------+
  | Tutorial    | Presents a short tutorial introducing the user to EPANET |
  +-------------+----------------------------------------------------------+
  | About       | Lists information about the version of EPANET being used |
  +-------------+----------------------------------------------------------+

..

   Context-sensitive Help is also available by pressing the F1 key.

.. _sec-toolbars:

Toolbars
~~~~~~~~

   Toolbars provide shortcuts to commonly used operations. There are two
   such toolbars:

    -  Standard Toolbar

    -  Map Toolbar


   The toolbars can be docked underneath the Main Menu bar or dragged to
   any location on the EPANET workspace. When undocked, they can also be
   re-sized. The toolbars can be made visible or invisible by selecting
   **View >> Toolbars**.


**Standard Toolbar**

   The Standard Toolbar contains speed buttons for commonly used
   commands.

   |image40| Opens a new project (**File >> New**)

   |image41| Opens an existing project (**File >> Open**)

   |image42| Saves the current project (**File >> Save**)

   |image43| Prints the currently active window (**File >> Print**)

   |image45| Copies selection to the clipboard or to a file (**Edit >> Copy To**)

   |image46| Deletes the currently selected item

   |image44| Finds a specific item on the map (**View >> Find**)

   |image48| Runs a simulation (**Project >> Run Analysis**)

   |image47| Runs a visual query on the map (**View >> Query**)

   |image49| Creates a new graph view of results (**Report >> Graph**)

   |image50| Creates a new table view of results (**Report >> Table**)

   |image51| Modifies options for the currently active view (**View >>
   Options** or **Report >> Options**)


**Map Toolbar**

   The Map Toolbar contains buttons for working with the Network Map.

   |image52| Selects an object on the map (**Edit >> Select Object**)

   |image53| Selects link vertex points (**Edit >> Select Vertex**)

   |image54| Selects a region on the map (**Edit >> Select Region**)

   |image55| Pans across the map (**View >> Pan**)

   |image56| Zooms in on the map (**View >> Zoom In**)

   |image57| Zooms out on the map (**View >> Zoom Out**)

   |image58| Draws map at full extent (**View >> Full Extent**)

   |image59| Adds a junction to the map

   |image60| Adds a reservoir to the map

   |image61| Adds a tank to the map

   |image62| Adds a pipe to the map

   |image63| Adds a pump to the map

   |image64| Adds a valve to the map

   |image65| Adds a label to the map


.. _sec-stats_bar:

Status Bar
~~~~~~~~~~

   The Status Bar appears at the bottom of the EPANET workspace and is
   divided into four sections which display the following information:

   - **Auto-Length** – indicates whether automatic computation of pipe
     lengths is turned on or off

   - **Flow Units** - displays the current flow units that are in effect

   - **Zoom Level** - displays the current zoom in level for the map (100%
     is full scale)

   - **Run Status** - a faucet icon shows:

      - No running water if no analysis results are available,

      - Running water when valid analysis results are available,

      - A broken faucet when analysis results are available but may be
        invalid because the network data have been modified.

   - **XY Location** - displays the map coordinates of the current
     position of the mouse pointer.


.. _sec-network_map:

Network Map
~~~~~~~~~~~

   The Network Map provides a planar schematic diagram of the objects
   comprising a water distribution network. The location of objects and
   the distances between them do not necessarily have to conform to
   their actual physical scale. Selected properties of these objects,
   such as water quality at nodes or flow velocity in links, can be
   displayed by using different colors. The color-coding is described in
   a Legend, which can be edited. New objects can be directly added to
   the map and existing objects can be clicked on for editing, deleting,
   and repositioning. A backdrop drawing (such as a street or
   topographic map) can be placed behind the network map for reference.
   The map can be zoomed to any scale and panned from one position to
   another. Nodes and links can be drawn at different sizes, flow
   direction arrows added, and object symbols, ID labels and numerical
   property values displayed. The map can be printed, copied onto the
   Windows clipboard, or exported as a DXF file or Windows metafile.

.. _sec-data_browser:

Data Browser
~~~~~~~~~~~~

   The Data Browser (shown in :numref:`fig-Data_Browser` below) is accessed from the Data
   tab on the Browser window. It gives access to the various objects, by
   category (Junctions, Pipes, etc.) that are contained in the network
   being analyzed. The buttons at the bottom are used to add, delete,
   and edit these objects.

   .. _fig-Data_Browser:
   .. figure:: media/image53A.jpeg
      :alt: Data Browser in EPANET
   
      Data browser.
   .. 

.. _sec-map_browser:

Map Browser
~~~~~~~~~~~

   The Map Browser (shown in :numref:`fig-Map_Browser` below) is accessed from the Map tab
   of the Browser Window. It selects the parameters and time period that
   are viewed in color-coded fashion on the Network Map. It also
   contains controls for animating the map through time.

   .. _fig-Map_Browser:
   .. figure:: media/image54A.jpeg
      :alt: Map Browser in EPANET
   
      Map browser.
   .. 
 
   The animation control pushbuttons on the Map Browser work as follows:

   |image68|   \ Rewind (return to initial time)

   |image68_2| \ Animate back through time
   
   |image68_3| \ Stop the animation
   
   |image68_4| \ Animate forward in time

.. _sec-prop_ed:

Property Editor
~~~~~~~~~~~~~~~

   The Property Editor (shown in :numref:`fig-Property_Editor` below) is used to edit
   the properties of network nodes, links, labels, and analysis options.
   It is invoked when one of these objects is selected (either on the
   Network Map or in the Data Browser) and double-clicked or the
   Browser's Edit button is clicked. The following points help explain
   how to use the Editor.
   
   .. _fig-Property_Editor:
   .. figure:: media/image57.png
      :alt: Property Editor in EPANET
   
      Property editor.
   .. 

    -  The Editor is a grid with two columns - one for the property's name
       and the other for its value.

    -  The columns can be re-sized by re-sizing the header at the top of the
       Editor with the mouse.

    -  The Editor window can be moved and re-sized via the normal Windows
       procedures.

    -  An asterisk next to a property name means that it is a required
       property -- its value cannot be left blank.

    -  Depending on the property, the value field can be one of the
       following:

      -  A text box where you type in a value

      -  A dropdown list box where you select from a list of choices

      -  An ellipsis button which you click to bring up a specialized editor

      -  A read-only label used to display computed results

    -  The property in the Editor that currently has focus will be
       highlighted with a white background.

    -  You can use both the mouse and the Up and Down arrow keys on the
       keyboard to move between properties.

    -  To begin editing the property with the focus, either begin typing
       a value or hit the Enter key.

    -  To have EPANET accept what you have entered, press the Enter key
       or move to another property; to cancel, press the Esc key.

    -  Clicking the Close button in the upper right corner of its title
       bar will hide the Editor.


.. _sec-prog_pref:

Program Preferences
~~~~~~~~~~~~~~~~~~~~

   Program preferences allow you to customize certain program features.
   To set program preferences select **Preferences** from the **File**
   menu. A Preferences dialog form will appear containing two tabbed
   pages – one for General Preferences and one for Format Preferences.


**General Preferences**

   The following preferences can be set on the General tab of the
   Preferences dialog (as shown below in :numref:`table-General_Preferences`).
   
.. _table-General_Preferences:
.. table:: General Preferences  

  +-----------------------------------+-----------------------------------+
  | *PREFERENCE*                      | *DESCRIPTION*                     |
  +===================================+===================================+
  | Blinking Map Highlighter          | Check to make the selected node,  |
  |                                   | link, or label on the map blink   |
  |                                   | on and off                        |
  +-----------------------------------+-----------------------------------+
  | Flyover Map Labeling              | Check to display the ID label and |
  |                                   | current parameter value in a      |
  |                                   | hint-style box whenever the mouse |
  |                                   | is placed over a node or link on  |
  |                                   | the network map                   |
  +-----------------------------------+-----------------------------------+
  | Confirm Deletions                 | Check to display a confirmation   |
  |                                   | dialog box before deleting any    |
  |                                   | object                            |
  +-----------------------------------+-----------------------------------+
  | Automatic Backup File             | Check to save a backup copy of a  |
  |                                   | newly opened project to disk      |
  |                                   | named with a .bak extension       |
  +-----------------------------------+-----------------------------------+
  | Clear File List                   | Clears the list of most recently  |
  |                                   | used project files from the       |
  |                                   | **File** menu                     |
  |                                   |                                   |
  +-----------------------------------+-----------------------------------+
..
    
  Note: The automatic Backup File is placed in the same directory as the input file.  However,
  the backup file (.bak) is only created for .net type input files, not the .inp text files.  The Preferences
  Menu screen for the General Tab is shown in :numref:`fig-Preferences_General` below.
	
   .. _fig-Preferences_General:
   .. figure:: media/preferences_general_tab.png
      :alt: Preferences - General Tab
   
      Preferences - general tab.
   ..

**Formatting Preferences**

   The Formats tab of the Preferences dialog box (shown in :numref:`fig-Preferences_Formats`) controls how many
   decimal places are displayed when results for computed parameters are
   reported. Use the dropdown list boxes to select a specific Node or
   Link parameter. Use the spin edit boxes to select the number of
   decimal places to use when displaying computed results for the
   parameter. The number of decimal places displayed for any particular
   input design parameter, such as pipe diameter, length, etc. is
   whatever the user enters.
   
   .. _fig-Preferences_Formats:
   .. figure:: media/image59.png
      :alt: Preferences - Formats Tab in EPANET
   
      Preferences - formats tab.
   ..
      



.. include:: image_subdefs.rst
