.. raw:: latex

    \clearpage
  	\pagenumbering{arabic}
    \setcounter{page}{1}


.. _introduction:

Introduction
============


What is EPANET
~~~~~~~~~~~~~~

   EPANET is a computer program that performs extended period simulation
   of hydraulic and water quality behavior within pressurized pipe
   networks. A network consists of pipes, nodes (pipe junctions), pumps,
   valves and storage tanks or reservoirs. EPANET tracks the flow of
   water in each pipe, the pressure at each node, the height of water in
   each tank, and the concentration of a chemical species throughout the
   network during a simulation period comprised of multiple time steps.
   In addition to chemical species, water age and source tracing can
   also be simulated.

   EPANET is designed to be a research tool for improving our
   understanding of the movement and fate of drinking water constituents
   within distribution systems. It can be used for many different kinds
   of applications in distribution systems analysis. Sampling program
   design, hydraulic model calibration, chlorine residual analysis, and
   consumer exposure assessment are some examples. EPANET can help
   assess alternative management strategies for improving water quality
   throughout a system. These can include:

    -  Altering source utilization within multiple source systems

    -  Altering pumping and tank filling/emptying schedules

    -  Use of satellite treatment, such as re-chlorination at storage tanks

    -  Targeted pipe cleaning and replacement

..

   Running under Windows, EPANET provides an integrated environment for
   editing network input data, running hydraulic and water quality
   simulations, and viewing the results in a variety of formats. These
   include color-coded network maps, data tables, time series graphs,
   and contour plots.

Hydraulic Modeling Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Full-featured and accurate hydraulic modeling is a prerequisite for
   doing effective water quality modeling. EPANET contains a
   state-of-the-art hydraulic analysis engine that includes the
   following capabilities:

    - Places no limit on the size of the network that can be analyzed

    - Computes friction headloss using the Hazen-Williams, Darcy-Weisbach,
      or Chezy-Manning formulas

    - Includes minor head losses for bends, fittings, etc.

    - Models constant or variable speed pumps

    - Computes pumping energy and cost

    - Models various types of valves including shutoff, check, pressure
      regulating, and flow control valves

    - Allows storage tanks to have any shape (i.e., diameter can vary with
      height)

    - Considers multiple demand categories at nodes, each with its own
      pattern of time variation

    - Models pressure driven flow issuing from emitters (sprinkler
      heads)

    - Models pressure driven demand at nodes

    - Can base system operation on both simple tank level or timer controls
      and on complex rule-based controls

Water Quality Modeling Capabilities
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   In addition to hydraulic modeling, EPANET provides the following
   water quality modeling capabilities:

    - Models the movement of a non-reactive tracer material through the
      network over time

    - Models the movement and fate of a reactive material as it grows
      (e.g., a disinfection by-product) or decays (e.g., chlorine residual)
      with time

    - Models the age of water throughout a network

    - Tracks the percent of flow from a given node reaching all other nodes
      over time

    - Models reactions both in the bulk flow and at the pipe wall

    - Uses n-th order kinetics to model reactions in the bulk flow

    - Uses zero or first order kinetics to model reactions at the pipe wall

    - Accounts for mass transfer limitations when modeling pipe wall
      reactions

    - Allows growth or decay reactions to proceed up to a limiting
      concentration

    - Employs global reaction rate coefficients that can be modified on a
      pipe-by-pipe basis

    - Allows wall reaction rate coefficients to be correlated to pipe
      roughness

    - Allows for time-varying concentration or mass inputs at any location
      in the network

    - Models storage tanks as being either complete mix, plug flow, or
      two-compartment reactors

..

   By employing these features, EPANET can study such water quality
   phenomena as:

    -  Blending water from different sources

    -  Age of water throughout a system

    -  Loss of chlorine residuals

    -  Growth of disinfection by-products

    -  Tracking contaminant propagation events

Steps in Using EPANET
~~~~~~~~~~~~~~~~~~~~~

   One typically carries out the following steps when using EPANET to
   model a water distribution system:

    1. Draw a network representation of your distribution system (see
       :numref:`sec-add_objs`) or import a basic description of the network placed in a
       text file (see :numref:`sec-import_partial_net`).

    2. Edit the properties of the objects that make up the system (see
       :numref:`sec-ed_visual_objs`).

    3. Describe how the system is operated (see :numref:`sec-ed_nonvisual_objs`).

    4. Select a set of analysis options (see :numref:`sec-analysis_ops`).

    5. Run a hydraulic/water quality analysis (see :numref:`sec-run_analysis`).

    6. View the results of the analysis (see :numref:`viewing_results`).
