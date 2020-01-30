EPANET 2.2 User Manual Overview
======================================
:ref:`introduction` of this manual describes what EPANET is and its
capabilities.

:ref:`quickstart` describes how to install EPANET and offers
up a quick tutorial on its use. Readers unfamiliar with the basics of
modeling distribution systems might wish to review :ref:`Chapter 3 <network_model>`
first before working through the tutorial.

:ref:`network_model` provides background material on how EPANET
models a water distribution system. It discusses the behavior of the physical
components that comprise a distribution system as well as how
additional modeling information, such as time variations and
operational control, are handled. It also provides an overview of how
the numerical simulation of system hydraulics and water quality
performance is carried out.

:ref:`workspace` shows how the EPANET workspace is organized. It
describes the functions of the various menu options and toolbar buttons, and
how the three main windows – the Network Map, the Browser, and the
Property Editor—are used.

:ref:`projects` discusses the project files that store all of the
information contained in an EPANET model of a distribution system. It
shows how to create, open, and save these files as well as how to set
default project options. It also discusses how to register
calibration data that are used to compare simulation results against
actual measurements.

:ref:`objects` describes how one goes about building a network
model of a distribution system with EPANET. It shows how to create the various
physical objects (pipes, pumps, valves, junctions, tanks, etc.) that
make up a system, how to edit the properties of these objects, and
how to describe the way that system demands and operation change over
time.

:ref:`map` explains how to use the network map that provides a
graphical view of the system being modeled. It shows how to view
different design and computed parameters in color-coded fashion on
the map, how to re-scale, zoom, and pan the map, how to locate
objects on the map, and what options are available to customize the
appearance of the map.

:ref:`analyzing_network` shows how to run a hydraulic/water quality
analysis of a network model. It describes the various options that control how
the analysis is made and offers some troubleshooting tips to use when
examining simulation results.

:ref:`viewing_results` discusses the various ways in which the
results of an analysis can be viewed. These include different views of the
network map, various kinds of graphs and tables, and several different types
of special reports.

:ref:`printing_copying` explains how to print and copy the views
discussed in :ref:`viewing_results`.

:ref:`importing_exporting` describes how EPANET can import and
export project scenarios. A scenario is a subset of the data that characterizes
the current conditions under which a pipe network is being analyzed
(e.g., consumer demands, operating rules, water quality reaction
coefficients, etc.). It also discusses how to save a project’s entire
database to a readable text file and how to export the network map to
a variety of formats.

:ref:`questions` answers questions about how EPANET can be used
to model special kinds of situations, such as modeling pneumatic tanks,
finding the maximum flow available at a specific pressure, and
modeling the growth of disinfection by-products.

:ref:`analysis_algorithms` provides details of the procedures and
formulas used by EPANET in its hydraulic and water quality analysis algorithms.


The manual also contains several appendixes.

| :ref:`units` provides a table of units of expression for all
  design and computed parameters.
| :ref:`error_messages` is a list of error message codes and their
  meanings that the program can generate.
| :ref:`command_line` describes how EPANET can be run
  from a command line prompt within a DOS window, and discusses the
  format of the files that are used with this mode of operation.
