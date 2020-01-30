.. raw:: latex

    \clearpage


.. _quickstart:

Quick Start Tutorial
====================


*This chapter provides a tutorial on how to use EPANET. If you are
not familiar with the components that comprise a water distribution
system and how these are represented in pipe network models you might
want to review the first two sections of* :ref:`network_model` *chapter first.*


-------

.. _sec-install:

Installing EPANET
~~~~~~~~~~~~~~~~~

   EPANET Version 2.2 is designed to run under the Windows 7/8/10
   operating system of an Intel-compatible personal computer. It is
   distributed as a single installer package file, **epanet2.2_setup.exe**.
   To install EPANET:

    1. Select **Run** from the Windows Start menu.

    2. Enter the full path and name of the **epanet2.2_setup.exe** file or click
       the **Browse** button to locate it on your computer.

    3. Click the **OK** button type to begin the setup process.



   The setup program will ask you to choose a folder (directory) where
   the EPANET files will be placed. The default folder is **c:\\Program
   Files (x86)\\EPANET 2.2**. After the files are installed your Start Menu will
   have a new item named EPANET 2.2. To launch EPANET simply select this
   item off of the Start Menu, then select EPANET 2.2 from the submenu that
   appears. (The name of the executable file that runs EPANET under
   Windows is **epanet2w.exe**.)

   Should you wish to remove EPANET from your computer, you can use the
   following procedure:

    1. Open **Control Panel**.

    2. Double-click on the **Add/Remove Programs** or **Uninstall a program** item.

    3. Select EPANET 2.2 from the list of programs that appears.

    4. Click the **Add/Remove** button or right click and select **uninstall**.

.. _sec-ex_network:

Example Network
~~~~~~~~~~~~~~~~


   In this tutorial we will analyze the simple distribution network
   shown in :numref:`fig-ex_pipe_network` below. It consists of a source reservoir (e.g., a
   treatment plant clearwell) from which water is pumped into a two-loop
   pipe network. There is also a pipe leading to a storage tank that
   floats on the system. The ID labels for the various components are
   shown in the figure. The nodes in the network have the
   characteristics shown in :numref:`table-ex_network_node_prop`. Pipe properties are listed in
   :numref:`table-ex_network_pipe_prop`. In addition, the pump (Link 9) can
   deliver 150 ft of head at a flow of 600 gpm, and the tank (Node 8)
   has a 60-ft diameter, a 3.5-ft water level, and a maximum level of 20
   feet.
   
   .. _fig-ex_pipe_network:
   .. figure:: media/image1.jpeg
      :alt: Example Pipe Network
   
      Example pipe network.
   ..
    
   .. _table-ex_network_node_prop:
   .. table:: Example Network Node Properties	
		
    +------+-----------+--------+
    | Node | Elevation | Demand |
    |      | (ft)      | (gpm)  |
    +======+===========+========+
    |    1 |    700    | 0      |
    +------+-----------+--------+
    |    2 |    700    | 0      |
    +------+-----------+--------+
    |    3 |    710    |    150 |
    +------+-----------+--------+
    |    4 |    700    |    150 |
    +------+-----------+--------+
    |    5 |    650    |    200 |
    +------+-----------+--------+
    |    6 |    700    |    150 |
    +------+-----------+--------+
    |    7 |    700    | 0      |
    +------+-----------+--------+
    |    8 |    830    | 0      |
    +------+-----------+--------+

   ..

   .. _table-ex_network_pipe_prop:
   .. table:: Example Network Pipe Properties
     
    +---------+----------------+----------------------+-------------+
    |    Pipe |    Length (ft) |    Diameter (inches) |    C-Factor |
    +=========+================+======================+=============+
    | 1       |    3000        |    14                |    100      |
    +---------+----------------+----------------------+-------------+
    | 2       |    5000        |    12                |    100      |
    +---------+----------------+----------------------+-------------+
    | 3       |    5000        |    8                 |    100      |
    +---------+----------------+----------------------+-------------+
    | 4       |    5000        |    8                 |    100      |
    +---------+----------------+----------------------+-------------+
    | 5       |    5000        |    8                 |    100      |
    +---------+----------------+----------------------+-------------+
    | 6       |    7000        |    10                |    100      |
    +---------+----------------+----------------------+-------------+
    | 7       |    5000        |    6                 |    100      |
    +---------+----------------+----------------------+-------------+
    | 8       |    7000        |    6                 |    100      |
    +---------+----------------+----------------------+-------------+

   ..

.. _sec-prj_setup:

Project Setup
~~~~~~~~~~~~~

   Our first task is to create a new project in EPANET and make sure
   that certain default options are selected. To begin, launch EPANET,
   or if it is already running select **File >> New** (from the menu
   bar) to create a new project. Then select **Project**
   **>> Defaults** to open the dialog form shown in :numref:`fig-prj_defaults_dialog`. We will
   use this dialog to have EPANET automatically label new objects with
   consecutive numbers starting from 1 as they are added to the network.
   On the ID Labels page of the dialog, clear all of the ID Prefix
   fields and set the ID Increment to 1. Then select the Hydraulics page
   of the dialog and set the choice of Flow Units to GPM (gallons per
   minute). This implies that US Customary units will be used for all
   other quantities as well (length in feet, pipe diameter in inches,
   pressure in psi, etc.). Also select Hazen - Williams (H-W) as the
   headloss formula. If you wanted to save these choices for all future
   new projects you could check the **Save** box at the bottom of the
   form before accepting it by clicking the **OK** button.

   .. _fig-prj_defaults_dialog:
   .. figure:: media/image2.png
      :alt: Project defaults window
   
      Project defaults dialog.
   ..
    
   Next we will select some map display options so that as we add
   objects to the map, we will see their ID labels and symbols
   displayed. Select **View >> Options** to bring up the Map Options
   dialog form. Select the Notation page on this form and check the
   settings shown in :numref:`fig-map_op_dialog` below. Then switch to the Symbols page
   and check all of the boxes. Click the **OK** button to accept these
   choices and close the dialog.

   Finally, before drawing our network we should insure that our map
   scale settings are acceptable. Select **View >> Dimensions** to bring
   up the Map Dimensions dialog. Note the default dimensions assigned
   for a new project. These settings will suffice for this example, so
   click the **OK** button.

   .. _fig-map_op_dialog:
   .. figure:: media/image3.png
      :alt: Map options window
   
      Map options dialog.
   ..

.. _sec-draw_network:

Drawing the Network
~~~~~~~~~~~~~~~~~~~

   We are now ready to begin drawing our network by making use of our
   mouse and the buttons contained on the Map Toolbar shown below. (If
   the toolbar is not visible then select **View >> Toolbars >> Map**).

    |image3|

   First we will add the reservoir. Click the Reservoir button |image4|.
   Then click the mouse on the map at the location of the reservoir
   (somewhere to the left of the map).

   Next we will add the junction nodes. Click the Junction button
   |image5| and then click on the map at the locations of nodes 2
   through 7.

   Finally add the tank by clicking the Tank button |image6| and
   clicking the map where the tank is located. At this point the Network
   Map should look something like the drawing in :numref:`fig-network_map_add_nodes`.

   .. _fig-network_map_add_nodes:
   .. figure:: media/image8.png
      :alt: Network Map after Adding Nodes
   
      Network map after adding nodes.
   ..
    
   Next we will add the pipes. Let's begin with pipe 1 connecting node 2
   to node 3. First click the Pipe button |image8| on the Toolbar. Then
   click the mouse on node 2 on the map and then on node 3. Note how an
   outline of the pipe is drawn as you move the mouse from node 2 to 3.
   Repeat this procedure for pipes 2 through 7.

   Pipe 8 is curved. To draw it, click the mouse first on Node 5. Then
   as you move the mouse towards Node 6, click at those points where a
   change of direction is needed to maintain the desired shape. Complete
   the process by clicking on Node 6.

   Finally we will add the pump. Click the Pump button |image9|, click
   on node 1 and then on node 2.

   Next we will label the reservoir, pump and tank. Select the Text
   button |image10| on the Map Toolbar and click somewhere close to the
   reservoir (Node 1). An edit box will appear. Type in the word SOURCE
   and then hit the **Enter** key. Click next to the pump and enter its
   label, then do the same for the tank. Then click the Selection button
   |image11| on the Toolbar to put the map into Object Selection mode
   rather than Text Insertion mode.

   At this point we have completed drawing the example network. Your
   Network Map should look like the map in :numref:`fig-ex_pipe_network`. If the nodes are
   out of position you can move them around by clicking the node to
   select it, and then dragging it with the left mouse button held down
   to its new position. Note how pipes connected to the node are moved
   along with the node. The labels can be repositioned in similar
   fashion. To re - shape the curved Pipe 8:

    1. First click on Pipe 8 to select it and then click the |image12|
       button on the Map Toolbar to put the map into Vertex Selection mode.

    2. Select a vertex point on the pipe by clicking on it and then drag it
       to a new position with the left mouse button held down.

    3. If required, vertices can be added or deleted from the pipe by right-
       clicking the mouse and selecting the appropriate option from the
       popup menu that appears.

    4. When finished, click |image13| to return to Object Selection mode.

.. _sec-set_obj_prop:

Setting Object Properties
~~~~~~~~~~~~~~~~~~~~~~~~~

   As objects are added to a project they are assigned a default set of
   properties. To change the value of a specific property for an object
   one must select the object into the Property Editor (:numref:`fig-prop_editor`).
   There are several different ways to do this. If the Editor is already
   visible then you can simply click on the object or select it from the
   Data page of the Browser. If the Editor is not visible then you can
   make it appear by one of the following actions:

    - Double-click the object on the map

    - Right-click on the object and select **Properties** from the pop-up
      menu that appears

    - Select the object from the Data page of the Browser window and then
      click the Browser’s Edit button |image14|


   Whenever the Property Editor has the focus you can press the F1 key
   to obtain fuller descriptions of the properties listed

   .. _fig-prop_editor:
   .. figure:: media/image15.png
      :alt: Property Editor Window
   
      Property editor.
   ..
    
   Let us begin editing by selecting Node 2 into the Property Editor as
   shown above. We would now enter the elevation and demand for this
   node in the appropriate fields. You can use the **Up** and **Down**
   arrows on the keyboard or the mouse to move between fields. We need
   only click on another object (node or link) to have its properties
   appear next in the Property Editor. (We could also press the **Page
   Down** or **Page Up** key to move to the next or previous object of
   the same type in the database.) Thus we can simply move from object
   to object and fill in elevation and demand for nodes, and length,
   diameter, and roughness (C-factor) for links.

   For the reservoir you would enter its elevation (700) in the Total
   Head field. For the tank, enter 830 for its elevation, 4 for its
   initial level, 20 for its maximum level, and 60 for its diameter. For
   the pump, we need to assign it a pump curve (head versus flow
   relationship). Enter the ID label 1 in the Pump Curve field.

   Next we will create Pump Curve 1. From the Data page of the Browser
   window, select Curves from the dropdown list box and then click the
   Add button |image16|. A new Curve 1 will be added to the database and
   the Curve Editor dialog form will appear (see :numref:`fig-curve_editor`). Enter the
   pump’s design flow (600) and head (150) into this form. EPANET
   automatically creates a complete pump curve from this single point.
   The curve’s equation is shown along with its shape. Click **OK** to
   close the Editor.

   .. _fig-curve_editor:
   .. figure:: media/image17.png
      :alt: Curve Editor Window
   
      Curve editor.
   ..

.. _sec-save_open_prjs:

Saving and Opening Projects
~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Having completed the initial design of our network it is a good idea
   to save our work to a file at this point.

    1.  From the **File** menu select the **Save As** option.

    2.  In the Save As dialog that appears, select a folder and file name
        under which to save this project. We suggest naming the file
        **tutorial.net**. (An extension of **.net** will be added to the
        file name if one is not supplied.).

    3. Click **OK** to save the project to file.

   The project data is saved to the file in a special binary format. If
   you wanted to save the network data to file as readable text, use the
   **File >> Export >> Network** command instead.

   To open our project at some later time, we would select the **Open**
   command from the **File** menu.

.. _sec-run_single_period:

Running a Single Period Analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   We now have enough information to run a single period (or snapshot)
   hydraulic analysis on our example network. To run the analysis select
   **Project >> Run Analysis** or click the Run button |image18| on the
   Standard Toolbar. (If the toolbar is not visible select **View >>
   Toolbars >> Standard** from the menu bar).

   If the run was unsuccessful then a Status Report window will appear
   indicating what the problem was. If it ran successfully you can view
   the computed results in a variety of ways. Try some of the following:

    - Select Node Pressure from the Browser’s Map page and observe how
      pressure values at the nodes become color-coded. To view the legend
      for the color-coding, select **View >> Legends >> Node** (or right-
      click on an empty portion of the map and select Node Legend from the
      popup menu). To change the legend intervals and colors, right-click
      on the legend to make the Legend Editor appear.

    - Bring up the Property Editor (double-click on any node or link) and
      note how the computed results are displayed at the end of the
      property list.

    - Create a tabular listing of results by selecting **Report >> Table**
      (or by clicking the Table button |image19| on the Standard Toolbar).
      :numref:`fig-ex_table_link_results` displays such a table for the link 
      results of this run. Note that flows with negative signs means that the 
      flow is in the opposite direction to the direction in which the pipe was drawn initially.

   .. _fig-ex_table_link_results:
   .. figure:: media/image20.png
      :alt: Example of a Table with Link Results
   
      Example table of link results.
   ..

.. _sec-run_EPS:

Running an Extended Period Analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   To make our network more realistic for analyzing an extended period
   of operation we will create a Time Pattern that makes demands at the
   nodes vary in a periodic way over the course of a day. For this
   simple example we will use a pattern time step of 6 hours thus making
   demands change at four different times of the day. (A 1-hour pattern
   time step is a more typical number and is the default assigned to new
   projects.) We set the pattern time step by selecting Options-Times
   from the Data Browser, clicking the Browser’s Edit button to make the
   Property Editor appear (if its not already visible), and entering 6
   for the value of the Pattern Time Step (as shown in :numref:`fig-time_options`
   below). While we have the Time Options available we can also set the
   duration for which we want the extended period to run. Let’s use a
   3-day period of time (enter 72 hours for the Duration property).

   .. _fig-time_options:
   .. figure:: media/image21.png
      :alt: Time Options Window
   
      Times options.
   ..

   To create the pattern, select the Patterns category in the Browser
   and then click the Add button |image22|. A new Pattern 1 will be
   created and the Pattern Editor dialog should appear (see :numref:`fig-pattern_ed`).
   Enter the multiplier values 0.5, 1.3, 1.0, 1.2 for the time periods 1
   to 4 that will give our pattern a duration of 24 hours. The
   multipliers are used to modify the demand from its base level in each
   time period. Since we are making a run of 72 hours, the pattern will
   wrap around to the start after each 24-hour interval of time.

   .. _fig-pattern_ed:
   .. figure:: media/image22.png
      :alt: Pattern Editor Window
   
      Pattern editor.
   ..
    
   We now need to assign Pattern 1 to the Demand Pattern property of all
   of the junctions in our network. We can utilize one of EPANET’s
   Hydraulic Options to avoid having to edit each junction individually.
   If you bring up the Hydraulic Options in the Property Editor you will
   see that there is an item called Default Pattern. Setting its value
   equal to 1 will make the Demand Pattern at each junction equal
   Pattern 1, as long as no other pattern is assigned to the junction.

   Next run the analysis (select **Project >> Run Analysis** or click
   the |image24| button on the Standard Toolbar). For extended period
   analysis you have several more ways in which to view results:

    - The scrollbar in the Browser’s Time controls is used to display the
      network map at different points in time. Try doing this with Pressure
      selected as the node parameter and Flow as the link parameter.

    - The buttons in the Browser can
      animate the map through time. Click the Forward button |image25|  to start the
      animation and the Stop button |image26|  to stop it.

    - Add flow direction arrows to the map (select **View >> Options**,
      select the Flow Arrows page from the Map Options dialog, and check a
      style of arrow that you wish to use). Then begin the animation again
      and note the change in flow direction through the pipe connected to
      the tank as the tank fills and empties over time.

    - Create a time series plot for any node or link. For example, to see
      how the water elevation in the tank changes with time:

      1. Click on the tank.

      2. Select **Report >> Graph** (or click the Graph button |image27|
         on the Standard Toolbar) which will display a Graph Selection
         dialog box.

      3. Select the Time Series button on the dialog.

      4. Select Head as the parameter to plot.

      5. Click **OK** to accept your choice of graph.



   Note the periodic behavior of the water elevation in the tank over
   time (:numref:`fig-ex_time_series_plot`).

   .. _fig-ex_time_series_plot:
   .. figure:: media/image26.png
      :alt: Example of a Time Series Plot
   
      Example time series plot.
   ..
 
.. _sec-run_wq:
  
Running a Water Quality Analysis
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Next we show how to extend the analysis of our example network to
   include water quality. The simplest case would be tracking the growth
   in water age throughout the network over time. To make this analysis
   we only have to select Age for the Parameter property in the Quality
   Options (select Options-Quality from the Data page of the Browser,
   then click the Browser's Edit button to make the Property Editor
   appear). Run the analysis and select Age as the parameter to view on
   the map. Create a time series plot for Age in the tank. Note that
   unlike water level, 72 hours is not enough time for the tank to reach
   periodic behavior for water age. (The default initial condition is to
   start all nodes with an age of 0.) Try repeating the simulation using
   a 240-hour duration or assigning an initial age of 60 hours to the
   tank (enter 60 as the value of Initial Quality in the Property Editor
   for the tank).

   Finally we show how to simulate the transport and decay of chlorine
   through the network. Make the following changes to the database:

     1. Select Options-Quality to edit from the Data Browser. In the
        Property Editor’s Parameter field type in the word Chlorine.

     2. Switch to Options-Reactions in the Browser. For Global Bulk
        Coefficient enter a value of -1.0. This reflects the rate at which
        chlorine will decay due to reactions in the bulk flow over time.
        This rate will apply to all pipes in the network. You could edit
        this value for individual pipes if you needed to.

     3. Click on the reservoir node and set its Initial Quality to 1.0. This
        will be the concentration of chlorine that continuously enters the
        network. (Reset the initial quality in the Tank to 0 if you had
        changed it.)



   Now run the example. Use the Time controls on the Map Browser to see
   how chlorine levels change by location and time throughout the
   simulation. Note how for this simple network, only junctions 5, 6,
   and 7 see depressed chlorine levels because of being fed by low
   chlorine water from the tank. Create a reaction report for this run
   by selecting **Report >> Reaction** from the main menu. The report
   should look like :numref:`fig-ex_reaction_report`. It shows on average how much chlorine
   loss occurs in the pipes as opposed to the tank. The term “bulk”
   refers to reactions occurring in the bulk fluid while “wall” refers
   to reactions with material on the pipe wall. The latter reaction is
   zero because we did not specify any wall reaction coefficient in this
   example.

   .. _fig-ex_reaction_report:
   .. figure:: media/image27.png
      :alt: Example of a Reaction Report
   
      Example reaction report.
   ..
   
   We have only touched the surface of the various capabilities offered
   by EPANET. Some additional features of the program that you should
   experiment with are:

   - Editing a property for a group of objects that lie within a user-
     defined area

   - Using Control statements to base pump operation on time of day or
     tank water levels

   - Exploring different Map Options, such as making node size be related
     to value

   - Attaching a backdrop map (such as a street map) to the network map

   - Creating different types of graphs, such as profile plots and contour
     plots

   - Adding calibration data to a project and viewing a calibration
     report

   - Copying the map, a graph, or a report to the clipboard or to a file

   - Saving and retrieving a design scenario (i.e., current nodal demands,
     pipe roughness values, etc.)



.. include:: image_subdefs.rst
