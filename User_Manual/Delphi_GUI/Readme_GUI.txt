This folder archives the Delphi source code for the EPANET graphical user interface.

This archive contains the Delphi source code for the Windows graphical
user interface, EPANET2W, that runs EPANET Version 2.2. EPANET is a 
pipe network analysis program that allows a user to create or import a
pipe network database, compute the hydraulic and water quality behavior
of the network, and selectively view simulation results in a variety of
different formats.

The code has been compiled successfully with the Professional Version
of Embarcadero's Delphi 10 or with the Delphi 10 Community Edition in
combination with Steema's TeeChart components. The executable produced
from this code is named epanet2w.exe and works together with the
following files that are all part of the EPANET 2 distribution package:
epanet2.dll, epanet2.chm, and tutorial.chm.

The EPANET2W application is a fairly large one, consisting of 38
forms, 11 Pascal units, and 5 custom components.

---------------------------------------------------------------------
NOTE: Before loading the EPANET2W project into the Delphi IDE you
      must install the collection of EPANET2W custom components on to
      Delphi's Component Pallette. These can be found in a separate
      archive named components.zip.
---------------------------------------------------------------------

The principal forms used by the application (with the names of their
Pascal units in parentheses) are as follows:

MainForm (Fmain.pas)
--------------------
This is the parent Multiple Document Interface (MDI) form for the
project. It houses the Main Menu and Toolbars that control program
operation.

MapForm (Fmap.pas)
------------------
This is a MDI child form that displays an editable schematic of the
pipe network being analyzed. 

BrowserForm (Fbrowser.pas)
--------------------------
An MDI child form used to navigate through the pipe network database
and control what information is viewed in color-coded fashion on
the network map.

PropEditForm (Fproped.pas)
--------------------------
A stay-on-top form that lists and edits the properties for the
current network component selected on the network map or from
the Browser. 

OVMapForm (Fovmap.pas)
----------------------
A stay-on-top form that shows the current view extent of the
network map on an outline map of the entire network.

SummaryForm (Fsummary.pas)
--------------------------
A modal dialog form that displays an editable title and notes
for the current network being analyzed, and summarizes the number
of components that comprise the network.

SimulationForm (Fsimul.pas)
---------------------------
A modal dialog form that displays the progress of the computations
for a simulation run.

StatusForm (Fstatus.pas)
------------------------
An MDI child form that displays a status report for the most
recently completed simulation run.

TableForm (Ftable.pas)
----------------------
An MDI child form that displays design and computed values for
network parameters in a tabular view.

GraphForm (Fgraph.pas)
----------------------
An MDI child form that displays design and computed values for
network parameters via X-Y plots.

ContourForm (Fcontour.pas)
--------------------------
An MDI child form that displays values of a design or computed
parameter in the form of a contour plot.

EnergyForm (Fenergy.pas)
------------------------
An MDI child form that displays a table of pumping energy
utilization.

CalibForm (Fcalib.pas)
----------------------
An MDI child form that compares simulated results against a set
of measured values in tabular and graphical formats.

In addition, there are a collection of dialog forms used to obtain
specialized information from the user. A listing of these forms
identified by their Pascal units follows:

Dabout.pas    About dialog box for the EPANET2W program
Dcalib1.pas   Used to register calibration data files with the
              project being analyzed
Dcalib2.pas   Selects parameter and locations to use for a 
              calibration report
Dcolramp.pas  Selects a map color scheme from a set of choices
Dcontour.pas  Selects contour map display options
Dcontrol.pas  Edits either the Simple Controls or Rule Based Controls
              that define how a pipe network is operated
Dcopy.pas     Selects choice of format and destination for copying
              the current view (map, graph, table, etc.)
Dcurve.pas    Edits the X-Y values for a curve (e.g., pump curve,
              pump efficiency curve, tank volume curve, etc.)
Ddataexp.pas  Selects database scenarios to save to file
Ddefault.pas  Selects default settings for the current project
Ddemand.pas   Edits multiple water demand categories at a specific 
              node of the network
Dfind.pas     Locates a specific node or link on the map by ID label
Dgraph.pas    Selects display and format options for a X-Y graph
Dgrouped.pas  Modifies a design parameter for a group of selected
              objects
Dinperr.pas   Displays a list of error messages generated when 
              importing a network description file
Dlabel.pas    Edit box used to insert a label on the network map
Dlegend.pas   Selects colors and numerical ranges for displaying
              a network parameter on the map
Dmap.pas      Selects map display options
Dmapdim.pas   Specifies the real-world dimensions the network map
Dmapexp.pas   Selects which format the network map should be saved
              to file in
Dpattern.pas  Edits the multipliers that comprise a time pattern
Dprefers.pas  Sets program preferences
Dquery.pas    Defines a condition to display on the network map
              (e.g., find all nodes with pressure below 20 psi)
Dsource.pas   Edits the properties of a water quality source
Dtable.pas    Selects options for viewing design or computed
              parameters in a table

The Pascal Units included in the project are as follows:

Udxf.pas      saves network map to file in DXF format
Uexport.pas   exports network data to file in readable text format
Ufileio.pas   reads and saves network data to file in binary format
Uglobals.pas  declarations of all global data types, classes and
              variables
Uimport.pas   imports network data from text file
Uinifile.pas  reads and writes program setup data to an ini file
Uinput.pas    handles all editing and retrieval of network design data
Umap.pas      handles all map drawing functions
Uoutput.pas   handles retrieving of all output results generated from a
              network analysis
Ureport.pas   writes design and output results to a formatted text file
Uutils.pas    collection of general purpose utility procedures

