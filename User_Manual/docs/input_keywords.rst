.. raw:: latex

    \clearpage


---------------------


[BACKDROP]
----------

**Purpose:**

  Identifies a backdrop image and dimensions for the network map.

**Format:**

  =============== =============================
  **DIMENSIONS**  *LLx LLy URx URy*
  **UNITS**       **FEET/METERS/DEGREES/NONE**
  **FILE**        *filename*
  **OFFSET**      *X Y*
  =============== =============================

**Definitions:**

  **DIMENSIONS** provides the X and Y coordinates of the lower-left and
  upper-right corners of the map’s bounding rectangle. Defaults are the
  extents of the nodal coordinates supplied in the [COORDINATES] section.

  **UNITS** specifies the units that the map’s dimensions are given in.
  Default is NONE.

  **FILE** is the name of the file that contains the backdrop image.

  **OFFSET** lists the X and Y distance that the upper-left corner of
  the backdrop image is offset from the upper-left corner of the map’s
  bounding rectangle. Default is zero offset.


**Remarks:**

  a. The [BACKDROP] section is optional and is not used at all when EPANET
     is run as a console application.

  b. Only Windows Enhanced Metafiles and bitmap files can be used as
     backdrops.

.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[CONTROLS]
----------

**Purpose:**

  Defines simple controls that modify links based on a single
  condition.

**Format:**

  One line for each control which can be of the form:

    ==== ======== ======== == ========= ======== =========== =======
    LINK *linkID* *status* IF NODE      *nodeID* ABOVE/BELOW *value*
    LINK *linkID* *status* AT TIME      *time*
    LINK *linkID* *status* AT CLOCKTIME *time*   AM/PM
    ==== ======== ======== == ========= ======== =========== =======

  where:

    | *linkID* = a link ID label
    | *status* = OPEN or CLOSED, a pump speed setting, or a control valve setting
    | *nodeID* = a node ID label
    | *value*  = a pressure for a junction or a water level for a tank
    | *time*   = a time since the start of the simulation in decimal hours or in
      hours:minutes format
    | *time* = a 12-hour clock time (hours:minutes)


**Remarks:**

  a. Simple controls are used to change link status or settings based on
     tank water level, junction pressure, time into the simulation or time
     of day.

  b. See the notes for the [STATUS] section for conventions used in
     specifying link status and setting, particularly for control valves.



**Examples:**

::

  [CONTROLS]
  ;Close Link 12 if the level in Tank 23 exceeds 20 ft.
  LINK 12 CLOSED IF NODE 23 ABOVE 20

  ;Open Link 12 if pressure at Node 130 is under 30 psi
  LINK 12 OPEN IF NODE 130 BELOW 30

  ;Pump PUMP02's speed is set to 1.5 at 16 hours into
  ;the simulation
  LINK PUMP02 1.5 AT TIME 16

  ;Link 12 is closed at 10 am and opened at 8 pm
  ;throughout the simulation
  LINK 12 CLOSED AT CLOCKTIME 10 AM
  LINK 12 OPEN AT CLOCKTIME 8 PM


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[COORDINATES]
-------------

**Purpose:**

  Assigns map coordinates to network nodes.

**Format:**

  One line for each node containing:

    -  Node ID label
    -  X-coordinate
    -  Y-coordinate



**Remarks:**

  a. Include one line for each node displayed on the map.

  b. The coordinates represent the distance from the node to an arbitrary
     origin at the lower left of the map. Any convenient units of measure
     for this distance can be used.

  c. There is no requirement that all nodes be included in the map, and
     their locations need not be to actual scale.

  d. A [COORDINATES] section is optional and is not used at all when
     EPANET is run as a console application.



**Example:**

::

  [COORDINATES]
  ;Node     X-Coord.     Y-Coord
  ;-------------------------------
    1       10023        128
    2       10056        95

.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[CURVES]
--------

**Purpose:**

  Defines data curves and their X,Y points.

**Format:**

  One line for each X,Y point on each curve containing:

    - Curve ID label
    - X value
    - Y value



**Remarks:**

  a. Curves can be used to represent the following relations:

     - Head v. Flow for pumps
     - Efficiency v. Flow for pumps
     - Volume v. Depth for tanks
     - Headloss v. Flow for General Purpose Valves

  b. The points of a curve must be entered in order of increasing X-values
     (lower to higher).

  c. If the input file will be used with the Windows version of EPANET,
     then adding a comment which contains the curve type and description,
     separated by a colon, directly above the first entry for a curve will
     ensure that these items appear correctly in EPANET’s Curve Editor.
     Curve types include PUMP, EFFICIENCY, VOLUME, and HEADLOSS. See the
     examples below.


**Example:**

::

  [CURVES]
  ;ID   Flow    Head
  ;PUMP: Curve for Pump 1 C1 0 200
  C1    1000    100
  C1    3000    0

  ;ID   Flow    Effic.
  ;EFFICIENCY:
  E1    200     50
  E1    1000    85
  E1    2000    75
  E1    3000    65

.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[DEMANDS]
---------

**Purpose:**

  Supplement to [JUNCTIONS] section for defining multiple water demands
  at junction nodes.

**Format:**

  One line for each category of demand at a junction containing:

    -  Junction ID label
    -  Base demand (flow units)
    -  Demand pattern ID (optional)
    -  Name of demand category preceded by a semicolon (optional)


**Remarks:**

  a. Only use for junctions whose demands need to be changed or
     supplemented from entries in [JUNCTIONS] section.

  b. Data in this section replaces any demand entered in [JUNCTIONS]
     section for the same junction.

  c. Unlimited number of demand categories can be entered per junction.

  d. If no demand pattern is supplied then the junction demand follows the
     Default Demand Pattern specified in the [OPTIONS] section or Pattern
     1 if no default pattern is specified. If the default pattern (or
     Pattern 1) does not exist, then the demand remains constant.


**Example:**

::

  [DEMANDS]
  ;ID    Demand   Pattern   Category
  ;---------------------------------
  J1     100      101       ;Domestic
  J1     25       102       ;School
  J256   50       101       ;Domestic


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[EMITTERS]
----------

**Purpose:**

  Defines junctions modeled as emitters (sprinklers or orifices).

**Format:**

  One line for each emitter containing:

    -  Junction ID label

    -  Flow coefficient, flow units at 1 psi (1 meter) pressure drop


**Remarks:**

  a. Emitters are used to model flow through sprinkler heads or pipe
     leaks.

  b. Flow out of the emitter equals the product of the flow coefficient
     and the junction pressure raised to a power.

  c. The power can be specified using the EMITTER EXPONENT option in the
     [OPTIONS] section. The default power is 0.5, which normally applies
     to sprinklers and nozzles.

  d. Actual demand reported in the program's results includes both the
     normal demand at the junction plus flow through the emitter.

  e. An [EMITTERS] section is optional.

.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[ENERGY]
--------

**Purpose:**

  Defines parameters used to compute pumping energy and cost.

**Format:**

  ========== ========== ======================= =======
  **GLOBAL**            **PRICE/PATTERN/EFFIC** *value*
  **PUMP**   *PumpID*   **PRICE/PATTERN/EFFIC** *value*
  **DEMAND** **CHARGE** *value*
  ========== ========== ======================= =======

**Remarks:**

  a. Lines beginning with the keyword **GLOBAL** are used to set global
     default values of energy price, price pattern, and pumping efficiency
     for all pumps.

  b. Lines beginning with the keyword **PUMP** are used to override global
     defaults for specific pumps.

  c. Parameters are defined as follows:

     - **PRICE** = average cost per kW-hour,
     - **PATTERN** = ID label of time pattern describing how energy price
       varies with time,
     - **EFFIC** = either a single percent efficiency for global setting
       or the ID label of an efficiency curve for a specific pump,
     - **DEMAND CHARGE** = added cost per maximum kW usage during the
       simulation period.

  d. The default global pump efficiency is 75% and the default global
     energy price is 0.

  e. All entries in this section are optional. Items offset by slashes (/)
     indicate allowable choices.


**Example:**

::

  [ENERGY]
  GLOBAL  PRICE      0.05   ;Sets global energy price
  GLOBAL  PATTERN    PAT1   ;and time-of-day pattern
  PUMP    23 PRICE   0.10   ;Overrides price for Pump 23
  PUMP    23 EFFIC   E23    ;Assigns effic. curve to Pump 23


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[JUNCTIONS]
-----------

**Purpose:**

  Defines junction nodes contained in the network.

**Format:**

  One line for each junction containing:

    -  ID label
    -  Elevation, ft (m)
    -  Base demand flow (flow units) (optional)
    -  Demand pattern ID (optional)


**Remarks:**

  a. A [JUNCTIONS] section with at least one junction is required.

  b. If no demand pattern is supplied then the junction demand follows the
     Default Demand Pattern specified in the [OPTIONS] section or Pattern
     1 if no default pattern is specified. If the default pattern (or
     Pattern 1) does not exist, then the demand remains constant.

  c. Demands can also be entered in the [DEMANDS] section and include
     multiple demand categories per junction.



**Example:**

::

  [JUNCTIONS]
  ;ID    Elev.   Demand   Pattern
  ;------------------------------
  J1     100     50       Pat1
  J2     120     10              ;Uses default demand pattern
  J3     115                     ;No demand at this junction


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[LABELS]
--------

**Purpose:**

  Assigns coordinates to map labels.

**Format:**

  One line for each label containing:

    -  X-coordinate
    -  Y-coordinate
    -  Text of label in double quotes
    -  ID label of an anchor node (optional)


**Remarks:**

  a. Include one line for each label on the map.

  b. The coordinates refer to the upper left corner of the label and are
     with respect to an arbitrary origin at the lower left of the map.

  c. The optional anchor node anchors the label to the node when the map
     is re-scaled during zoom-in operations.

  d. The [LABELS] section is optional and is not used at all when EPANET
     is run as a console application.


**Example:**

::

  [LABELS]
  ;X-Coord.    Y-Coord.    Label            Anchor
  ;-----------------------------------------------
  1230         3459        “Pump 1”
  34.57        12.75       “North Tank”     T22


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[MIXING]
--------

**Purpose:**

  Identifies the model that governs mixing within storage tanks.

**Format:**

  One line per tank containing:

    -  Tank ID label
    -  Mixing model (MIXED, 2COMP, FIFO, or LIFO)
    -  Compartment volume (fraction)


**Remarks:**

 a. Mixing models include:

    - Completely Mixed (MIXED)
    - Two-Compartment Mixing (2COMP)
    - Plug Flow (FIFO)
    - Stacked Plug Flow (LIFO)

 b. The compartment volume parameter only applies to the two-compartment
 model and represents the fraction of the total tank volume devoted to the inlet/outlet compartment.

 c. The [MIXING] section is optional. Tanks not described in this section 
 are assumed to be completely mixed.

**Example:**

::

  [MIXING]
  ;Tank       Model
  ;-----------------------
  T12         LIFO
  T23         2COMP    0.2


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[OPTIONS]
---------

**Purpose:**

  Defines various simulation options.

**Formats:**

  .. tabularcolumns:: |\X{2}{5}|\X{2}{5}|\X{1}{10}|

  ===================== ============================== ========
  **UNITS**             **CFS/GPM/MGD/IMGD/AFD/**
                        **LPS/LPM/MLD/CMH/CMD**
  **HEADLOSS**          **H-W/D-W/C-M**
  **HYDRAULICS**        **USE/SAVE**                   filename
  **QUALITY**           **NONE/CHEMICAL/AGE/TRACE**    id
  **VISCOSITY**         value
  **DIFFUSIVITY**       value
  **SPECIFIC GRAVITY**  value
  **TRIALS**            value
  **ACCURACY**          value
  **HEADERROR**         value
  **FLOWCHANGE**        value
  **UNBALANCED**        **STOP/CONTINUE/CONTINUE**     n
  **PATTERN**           id
  **DEMAND MODEL**      **DDA/PDA**
  **MINIMUM PRESSURE**  value
  **REQUIRED PRESSURE** value
  **PRESSURE EXPONENT** value
  **DEMAND MULTIPLIER** value
  **EMITTER EXPONENT**  value
  **TOLERANCE**         value
  **MAP**               filename
  ===================== ============================== ========


**Definitions:**

  **UNITS** sets the units in which flow rates are expressed where:

    | **CFS** = cubic feet per second
    | **GPM** = gallons per minute
    | **MGD** = million gallons per day
    | **IMGD** = Imperial MGD
    | **AFD** = acre-feet per day
    | **LPS** = liters per second
    | **LPM** = liters per minute
    | **MLD** = million liters per day
    | **CMH** = cubic meters per hour
    | **CMD** = cubic meters per day

  For **CFS, GPM, MGD, IMGD**, and **AFD** other input quantities are
  expressed in US Customary Units. If flow units are in liters or cubic
  meters then Metric Units must be used for all other input quantities
  as well. (See Appendix A. Units of Measurement). The default flow units are **GPM**.

  **HEADLOSS* selects a formula to use for computing head loss for
  flow through a pipe. The choices are the Hazen-Williams (**H-W**),
  Darcy-Weisbach (**D-W**), or Chezy-Manning (**C-M**) formulas. The
  default is **H-W**.

  **HYDRAULICS** option allows you to either **SAVE** the current
  hydraulics solution to a file or **USE** a previously saved
  hydraulics solution. This is useful when studying factors that only
  affect water quality behavior.

  **QUALITY** selects the type of water quality analysis to perform.
  The choices are **NONE, CHEMICAL, AGE**, and **TRACE**. In place of
  **CHEMICAL** the actual name of the chemical can be used followed by
  its concentration units (e.g., **CHLORINE mg/L**). If **TRACE** is
  selected it must be followed by the ID label of the node being
  traced. The default selection is **NONE** (no water quality analysis).

  **VISCOSITY** is the kinematic viscosity of the fluid being modeled
  relative to that of water at 20 deg. C (1.0 centistoke). The default value is 1.0.

  **DIFFUSIVITY** is the molecular diffusivity of the chemical being
  analyzed relative to that of chlorine in water. The default value is
  1.0. Diffusivity is only used when mass transfer limitations are
  considered in pipe wall reactions. A value of 0 will cause EPANET to ignore mass transfer limitations.

  **SPECIFIC GRAVITY** is the ratio of the density of the fluid being modeled to that of water at 4 deg. C (unitless).

  **TRIALS** are the maximum number of trials used to solve network hydraulics at each hydraulic time step of a simulation. The default
  is 200.

  **ACCURACY** prescribes the convergence criterion that determines when a hydraulic solution has been reached. The trials end when the
  sum of all flow changes from the previous solution divided by the total flow in all links is less than this number. The default is 0.001.

  **HEADERROR** augments **ACCURACY** option. Sets the maximum head loss error that any network link can have for hydraulic convergence to occur.
  A link's head loss error is the difference between the head loss found as a function of computed flow in the link (such as by the Hazen-Williams equation for a pipe) and the difference in computed
  heads for the link's end nodes. The units of this parameter are feet (US) or meters (SI). The default value of 0 indicates that no
  head error limit applies.

  **FLOWCHANGE** augments the **ACCURACY** option. Sets the largest change in flow that any network 
  element (link, emitter, or pressure driven demand) can have for hydraulic convergence to occur. It
  is specified in whatever flow units the project is using. The default value of 0 indicates that no flow change limit applies.

  **UNBALANCED** determines what happens if a hydraulic solution cannot be reached within the prescribed 
  number of **TRIALS** at some hydraulic time step into the simulation. **"STOP"** will halt the
  entire analysis at that point. **"CONTINUE"** will continue the analysis with a warning message 
  issued. **"CONTINUE n"** will continue the search for a solution for another "n" trials with the
  status of all links held fixed at their current settings. The simulation will be continued at this 
  point with a message issued about whether convergence was achieved or not. The default choice is **"STOP"**.

  **PATTERN** provides the ID label of a default demand pattern to be applied to all junctions where no 
  demand pattern was specified. If no such pattern exists in the [PATTERNS] section then by default 
  the pattern consists of a single multiplier equal to 1.0. If this option is not used, then the 
  global default demand pattern has a label of "1".

  **DEMAND MULTIPLIER** is used to adjust the values of baseline demands for all junctions and all 
  demand categories. For example, a value of 2 doubles all baseline demands, while a value of 0.5 
  would halve them. The default value is 1.0.

  **DEMAND MODEL** determines nodal demand model -- Demand Driven Analysis (**DDA**) or Pressure Driven 
  Analysis (**PDA**). DDA assumes a nodal demand at a given point in time is a fixed value :math:`D`. 
  This sometimes results in hydraulic solutions with negative pressures (a physical impossibility).
  PDA assumes the demand delivered, :math:`d`, is a function of nodal pressure, :math:`p`, as follows:

    .. math::
       d = D \left[ \frac{p - P_{min}}{P_{req} - P_{min}} \right]^{Pexp}

  where :math:`D` is the full demand required, :math:`Pmin` is the pressure below which demand is zero, 
  :math:`Preq` is the pressure required to deliver the full required demand and :math:`Pexp` is an 
  exponent. The units of the pressures are psi (US) or meters (SI). When :math:`p < Pmin` demand is 0 
  and when :math:`p > Preq` demand equals :math:`D`. The default value is **DDA**.

  **MINIMUM PRESSURE** specifies the value for :math:`Pmin`. Default value is 0.0.

  **REQUIRED PRESSURE** specifies the value for :math:`Preq`. Default value is 0.1.

  **PRESSURE EXPONENT** specifies the value for :math:`Pexp`. Default value is 0.5.

  **EMITTER EXPONENT** specifies the power to which the pressure at a junction is raised when computing 
  the flow issuing from an emitter. The default is 0.5.

  **MAP** is used to supply the name of a file containing coordinates of the network's nodes so that a 
  map of the network can be drawn. It is not used for any hydraulic or water quality computations.

  **TOLERANCE** is the difference in water quality level below which one can say that one parcel of 
  water is essentially the same as another. The default is 0.01 for all types of quality analyses
  (chemical, age (measured in hours), or source tracing (measured in percent)).

**Remarks:**

  a. All options assume their default values if not explicitly specified
     in this section.

  b. Items offset by slashes (/) indicate allowable choices.


**Example:**

::

  [OPTIONS]
  UNITS        CFS
  HEADLOSS     D-W
  QUALITY      TRACE   Tank23
  UNBALANCED   CONTINUE   10


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[PATTERNS]
----------

**Purpose:**

  Defines time patterns.

**Format:**

  One or more lines for each pattern containing:

    -  Pattern ID label
    -  One or more multipliers


**Remarks:**

  Multipliers define how some base quantity (e.g., demand) is
  adjusted for each time period.

  a. All patterns share the same time period interval as defined in the
     [TIMES] section.

  b. Each pattern can have a different number of time periods.

  c. When the simulation time exceeds the pattern length the pattern wraps
     around to its first period.

  d. Use as many lines as it takes to include all multipliers for each
     pattern.


**Example:**

::

  [PATTERNS]
  ;Pattern P1
  P1    1.1    1.4    0.9    0.7
  P1    0.6    0.5    0.8    1.0
  ;Pattern P2
  P2    1      1      1      1
  P2    0      0      1


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[PIPES]
-------

**Purpose:**

  Defines all pipe links contained in the network.

**Format:**

  One line for each pipe containing:

    -  ID label of pipe
    -  ID of start node
    -  ID of end node
    -  Length, ft (m)
    -  Diameter, inches (mm)
    -  Roughness coefficient
    -  Minor loss coefficient
    -  Status (OPEN, CLOSED, or CV)


**Remarks:**

  a. Roughness coefficient is unitless for the Hazen-Williams and
     Chezy-Manning head loss formulas and has units of millifeet (mm) for
     the Darcy-Weisbach formula. Choice of head loss formula is supplied
     in the [OPTIONS] section.

  b. Setting status to CV means that the pipe contains a check valve
     restricting flow to one direction.

  c. If minor loss coefficient is 0 and pipe is OPEN then these two items
     can be dropped form the input line.


**Example:**

::

  [PIPES]
  ;ID   Node1  Node2   Length   Diam.   Roughness  Mloss   Status
  ;-------------------------------------------------------------
   P1    J1     J2     1200      12       120       0.2    OPEN
   P2    J3     J2      600       6       110       0      CV
   P3    J1     J10    1000      12       120


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[PUMPS]
-------

**Purpose:**

  Defines all pump links contained in the network.

**Format:**

  One line for each pump containing:

    -  ID label of pump
    -  ID of start node
    -  ID of end node
    -  Keyword and Value (can be repeated)


**Remarks:**

  a. Keywords consists of:

      | **POWER** – power value for constant energy pump, hp (kW)\
      | **HEAD** - ID of curve that describes head versus flow for the pump
      | **SPEED** - relative speed setting (normal speed is 1.0, 0 means pump is off)
      | **PATTERN** - ID of time pattern that describes how speed setting varies with time

  b. Either **POWER** or **HEAD** must be supplied for each pump. The
     other keywords are optional.


**Example:**

::

  [PUMPS]
  ;ID    Node1    Node2    Properties
  ;---------------------------------------------
  Pump1   N12      N32     HEAD Curve1
  Pump2   N121     N55     HEAD Curve1  SPEED 1.2
  Pump3   N22      N23     POWER 100


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[QUALITY]
---------

**Purpose:**

  Defines initial water quality at nodes.

**Format:**

  One line per node containing:

    -  Node ID label
    -  Initial quality


**Remarks:**

  a. Quality is assumed to be zero for nodes not listed.

  b. Quality represents concentration for chemicals, hours for water age,
     or percent for source tracing.

  c. The [QUALITY] section is optional.


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[REACTIONS]
-----------

**Purpose:**

   Defines parameters related to chemical reactions occurring in the
   network.

**Format:**

  ========================= ======================= =====
  **ORDER**                 **BULK/WALL/TANK**      value
  **GLOBAL**                **BULK/WALL**           value
  **BULK/WALL/TANK**        pipeID                  value
  **LIMITING POTENTIAL**    value
  **ROUGHNESS CORRELATION** value
  ========================= ======================= =====

**Definitions:**

  **ORDER** is used to set the order of reactions occurring in the bulk
  fluid, at the pipe wall, or in tanks, respectively. Values for wall
  reactions must be either 0 or 1. If not supplied the default reaction order is 1.0.

  **GLOBAL** is used to set a global value for all bulk reaction coefficients (pipes
  and tanks) or for all pipe wall coefficients. The default value is zero.

  **BULK**, **WALL**, and **TANK** are used to override the global reaction coefficients for specific pipes
  and tanks.

  **LIMITING POTENTIAL** specifies that reaction rates are proportional to the difference between
  the current concentration and some limiting potential value.

  **ROUGHNESS CORRELATION** will make all default pipe wall reaction coefficients be related to pipe
  roughness in the following manner:

    ==================  =====================
    Head Loss Equation  Roughness Correlation
    ==================  =====================
    Hazen-Williams      :math:`F / C`
    Darcy-Weisbach      :math:`F / log(e/D)`
    Chezy-Manning       :math:`F*n`
    ==================  =====================

  where :math:`F` = roughness correlation, :math:`C` = Hazen-Williams C-factor,
  :math:`e` = Darcy-Weisbach roughness, :math:`D` = pipe diameter, and
  :math:`n` = Chezy-Manning roughness coefficient. The default value computed
  this way can be overridden for any pipe by using the **WALL** format to
  supply a specific value for the pipe.

**Remarks:**

  a. Remember to use positive numbers for growth reaction coefficients and
     negative numbers for decay coefficients.

  b. The time units for all reaction coefficients are 1/days.

  c. All entries in this section are optional. Items offset by slashes (/)
     indicate allowable choices.



**Example:**

::

  [REACTIONS]
  ORDER WALL    0    ;Wall reactions are zero-order
  GLOBAL BULK  -0.5  ;Global bulk decay coeff.
  GLOBAL WALL  -1.0  ;Global wall decay coeff.
  WALL   P220  -0.5  ;Pipe-specific wall coeffs.
  WALL   P244  -0.7


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[REPORT]
--------

**Purpose:**

  Describes the contents of the output report produced from a simulation.

**Format:**

  ============ ============================== ===============
  **PAGESIZE** value
  **FILE**     filename
  **STATUS**   **YES/NO/FULL**
  **SUMMARY**  **YES/NO**
  **ENERGY**   **YES/NO**
  **NODES**    **NONE/ALL/**/node1 node2 ...
  **LINKS**    **NONE/ALL/**/link1 link2 ...
  parameter    **YES/NO**
  parameter    **BELOW/ABOVE/PRECISION**      value
  ============ ============================== ===============


**Definitions:**

  **PAGESIZE** sets the number of lines written per page of the output report. The default is 0, meaning that no line limit 
  per page is in effect.

  **FILE** supplies the name of a file to which the output report will 
  be written (ignored by the Windows version of EPANET).

  **STATUS** determines whether a hydraulic status report should be
  generated. If **YES** is selected the report will identify all
  network components that change status during each time step of the
  simulation. If **FULL** is selected, then the status report will also
  include information from each trial of each hydraulic analysis. This
  level of detail is only useful for de-bugging networks that become
  hydraulically unbalanced. The default is **NO**.

  **SUMMARY** determines whether a summary table of number of network
  components and key analysis options is generated. The default is **YES**.

  **ENERGY** determines if a table reporting average energy usage and cost for each pump 
  is provided. The default is NO.

  **NODES** identifies which nodes will be reported on. You can either
  list individual node ID labels or use the keywords **NONE** or
  **ALL**. Additional **NODES** lines can be used to continue the list. The default is **NONE**.

  **LINKS** identifies which links will be reported on. You can either list individual link ID 
  labels or use the keywords **NONE** or **ALL**. Additional **LINKS** lines can be used to continue 
  the list. The default is **NONE**.

  The “parameter” reporting option is used to identify which quantities
  are reported on, how many decimal places are displayed, and what kind
  of filtering should be used to limit output reporting. Node
  parameters that can be reported on include:

    - **Elevation**
    - **Demand**
    - **Head**
    - **Pressure**
    - **Quality.**

  Link parameters include:

    - **Length**
    - **Diameter**
    - **Flow**
    - **Velocity**
    - **Headloss**
    - **Position** (same as status – open, active, closed)
    - **Setting** (Roughness for pipes, speed for pumps, pressure/flow setting for valves)
    - **Reaction** (reaction rate)
    - **F-Factor** (friction factor).


  The default quantities reported are **Demand, Head, Pressure**, and
  **Quality** for nodes and **Flow, Velocity**, and **Headloss** for links. The default precision
  is two decimal places.

**Remarks:**

  a. All options assume their default values if not explicitly specified
     in this section.

  b. Items offset by slashes (/) indicate allowable choices.

  c. The default is to not report on any nodes or links, so a **NODES** or
     **LINKS** option must be supplied if you wish to report results for
     these items.

  d. For the Windows version of EPANET, the only [REPORT] option
     recognized is **STATUS**. All others are ignored.


**Example:**

  The following example reports on nodes N1, N2, N3, and N17 and all
  links with velocity above 3.0. The standard node parameters (Demand,
  Head, Pressure, and Quality) are reported on while only Flow,
  Velocity, and F-Factor (friction factor) are displayed for links.

::

  [REPORT]
  NODES N1 N2 N3 N17
  LINKS ALL
  FLOW YES
  VELOCITY PRECISION 4
  F-FACTOR PRECISION 4
  VELOCITY ABOVE 3.0


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[RESERVOIRS]
------------

**Purpose:**

  Defines all reservoir nodes contained in the network.

**Format:**

  One line for each reservoir containing:

    -  ID label
    -  Head, ft (m)
    -  Head pattern ID (optional)


**Remarks:**

  a. Head is the hydraulic head (elevation + pressure head) of water in
     the reservoir.

  b. A head pattern can be used to make the reservoir head vary with time.

  c. At least one reservoir or tank must be contained in the network.


**Example:**

::

  [RESERVOIRS]
  ;ID    Head    Pattern
  ;---------------------
  R1     512               ;Head stays constant
  R2     120     Pat1      ;Head varies with time


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[RULES]
-------

**Purpose:**

   Defines rule-based controls that modify links based on a combination
   of conditions.

**Format:**

  Each rule is a series of statements of the form:

  ============ ===========
  **RULE**     ruleID
  **IF**       condition_1
  **AND**      condition_2
  **OR**       condition_3
  **AND**      condition_4
  etc.
  **THEN**     action_1
  **AND**      action_2
  etc.
  **ELSE**     action_3
  **AND**      action_4
  etc.
  **PRIORITY** value
  ============ ===========

  where:
    | ruleID  = an ID label assigned to the rule
    | conditon_n = a condition clause
    | action_n = an action clause
    | Priority = a priority value (e.g., a number from 1 to 5)


**Condition Clause Format:**

  A condition clause in a Rule-Based Control takes the form of:

  ====== == ========= ======== =====
  object id attribute relation value
  ====== == ========= ======== =====

  where:
    | object = a category of network object
    | id = the object's ID label
    | attribute = an attribute or property of the object
    | relation = a relational operator
    | value = an attribute value

  Some example conditional clauses are:

  ::

    JUNCTION 23 PRESSURE > 20
    TANK T200 FILLTIME BELOW 3.5
    LINK 44 STATUS IS OPEN
    SYSTEM DEMAND >= 1500
    SYSTEM CLOCKTIME = 7:30 AM

The Object keyword can be any of the following:

  ============= ========= ==========
  **NODE**      **LINK**  **SYSTEM**
  **JUNCTION**  **PIPE**
  **RESERVOIR** **PUMP**
  **TANK**      **VALVE**
  ============= ========= ==========

When **SYSTEM** is used in a condition no ID is supplied.

The following attributes can be used with Node-type objects:

  - **DEMAND**
  - **HEAD**
  - **PRESSURE**

The following attributes can be used with Tanks:

  - **LEVEL**
  - **FILLTIME** (hours needed to fill a tank)
  - **DRAINTIME** (hours needed to empty a tank)

These attributes can be used with Link-Type objects:

  - **FLOW**
  - **STATUS** (**OPEN**, **CLOSED**, or **ACTIVE**)
  - **SETTING** (pump speed or valve setting)

The **SYSTEM** object can use the following attributes:

  - **DEMAND** (total system demand)
  - **TIME** (hours from the start of the simulation expressed either as a decimal number or in hours:minutes format)
  - **CLOCKTIME** (24-hour clock time with **AM** or **PM** appended)

Relation operators consist of the following:

  ====== =========
  **=**  **IS**
  **<>** **NOT**
  **<**  **BELOW**
  **>**  **ABOVE**
  **<=** **>=**
  ====== =========

**Action Clause Format:**

  An action clause in a Rule-Based Control takes the form of:

  ====== == ============== == =====
  object id STATUS/SETTING IS value
  ====== == ============== == =====

  where:

    | object = LINK, PIPE, PUMP, or VALVE keyword
    | id = the object's ID label
    | value = a status condition (OPEN or CLOSED), pump speed setting, or valve
    | setting


  Some example action clauses are:

  ::

    LINK 23 STATUS IS CLOSED
    PUMP P100 SETTING IS 1.5
    VALVE 123 SETTING IS 90


**Remarks:**

  a. Only the **RULE**, **IF** and **THEN** portions of a rule are
     required; the other portions are optional.

  b. When mixing **AND** and **OR** clauses, the **OR** operator has
     higher precedence than **AND**, i.e.,

     ::

       IF A or B and C

     is equivalent to

     ::

       IF (A or B) and C.


     If the interpretation was meant to be

     ::

       IF A or (B and C)

     then this can be expressed using two rules as in

     ::

       IF A THEN ...
       IF B and C THEN ...

  c. The **PRIORITY** value is used to determine which rule applies when
     two or more rules require that conflicting actions be taken on a
     link. A rule without a priority value always has a lower priority
     than one with a value. For two rules with the same priority value,
     the rule that appears first is given the higher priority.



**Example:**

::

  [RULES]
  RULE 1
  IF TANK 1 LEVEL ABOVE 19.1
  THEN PUMP 335 STATUS IS CLOSED
  AND PIPE 330 STATUS IS OPEN

  RULE 2
  IF SYSTEM CLOCKTIME >= 8 AM
  AND SYSTEM CLOCKTIME < 6 PM
  AND TANK 1 LEVEL BELOW 12
  THEN PUMP 335 STATUS IS OPEN

  RULE 3
  IF SYSTEM CLOCKTIME >= 6 PM
  OR SYSTEM CLOCKTIME < 8 AM
  AND TANK 1 LEVEL BELOW 14
  THEN PUMP 335 STATUS IS OPEN


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[SOURCES]
---------

**Purpose:**

  Defines locations of water quality sources.

**Format:**

  One line for each water quality source containing:

    -  Node ID label
    -  Source type (**CONCEN, MASS, FLOWPACED**, or **SETPOINT**)
    -  Baseline source strength
    -  Time pattern ID (optional)


**Remarks:**

  a. For **MASS** type sources, strength is measured in mass flow per
     minute. All other types measure source strength in concentration
     units.

  b. Source strength can be made to vary over time by specifying a time
     pattern.

  c. A **CONCEN** source:

       - represents the concentration of any external source inflow to the node
       - applies only when the node has a net negative demand (water enters the network at the node)
       - if the node is a junction, reported concentration is the result of mixing the source flow and inflow from the rest of the network
       - if the node is a reservoir, the reported concentration is the source concentration
       - if the node is a tank, the reported concentration is the internal concentration of the tank
       - is best used for nodes that represent source water supplies or treatment works (e.g., reservoirs or nodes assigned a negative demand)
       - should not be used at storage tanks with simultaneous inflow/outflow.

  d. A **MASS, FLOWPACED**, or **SETPOINT** source:

       - represents a booster source, where the substance is injected directly into the network irregardless of what the demand at the node is
       - affects water leaving the node to the rest of the network in the following way:
     
         - A **MASS** booster adds a fixed mass flow to that resulting from inflow to the node
         - A **FLOWPACED** booster adds a fixed concentration to the resultant inflow concentration at the node
         - A **SETPOINT** booster fixes the concentration of any flow leaving the node (as long as the concentration resulting from the inflows is below the setpoint)
       - the reported concentration at a junction or reservoir booster source is the concentration that results after the boosting is applied; the reported concentration for a tank with a booster source is the internal concentration of the tank
       - is best used to model direct injection of a tracer or disinfectant into the network or to model a contaminant intrusion.

  e. A [SOURCES] section is not needed for simulating water age or source tracing.


**Example:**

::

  [SOURCES]
  ;Node   Type   Strength  Pattern
  ;--------------------------------
    N1      CONCEN   1.2      Pat1    ;Concentration varies with time
    N44     MASS     12               ;Constant mass injection


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[STATUS]
--------

**Purpose:**

  Defines initial status of selected links at the start of a
  simulation.

**Format:**

  One line per link being controlled containing:

    - Link ID label
    - Status or setting


**Remarks:**

  a. Links not listed in this section have a default status of **OPEN**
     (for pipes and pumps) or **ACTIVE** (for valves).

  b. The status value can be **OPEN** or **CLOSED**. For control valves
     (e.g., PRVs, FCVs, etc.) this means that the valve is either fully
     opened or closed, not active at its control setting.

  c. The setting value can be a speed setting for pumps or valve setting
     for valves.

  d. The initial status of pipes can also be set in the [PIPES] section.

  e. Check valves cannot have their status be preset.

  f. Use [CONTROLS] or [RULES] to change status or setting at some future
     point in the simulation.

  g. If a **CLOSED** or **OPEN** control valve is to become **ACTIVE**
     again, then its pressure or flow setting must be specified in the
     control or rule that re-activates it.

**Example:**

::

  [STATUS]
  ; Link   Status/Setting
  ;----------------------
    L22     CLOSED         ;Link L22 is closed
    P14     1.5            ;Speed for pump P14
    PRV1    OPEN           ;PRV1 forced open
                           ;(overrides normal operation)


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[TAGS]
------

**Purpose:**

  Associates category labels (tags) with specific nodes and links.

**Format:**

  One line for each node and link with a tag containing

    - the keyword NODE or LINK
    - the node or link ID label
    - the text of the tag label (with no spaces)


**Remarks:**

  a. Tags can be useful for assigning nodes to different pressure zones or
     for classifying pipes by material or age.

  b. If a node or link’s tag is not identified in this section then it is
     assumed to be blank.

  c. The [TAGS] section is optional and has no effect on the hydraulic or
     water quality calculations.



**Example:**

::

  [TAGS]
  ;Object  ID       Tag
  ;------------------------------
   NODE    1001     Zone_A
   NODE    1002     Zone_A
   NODE    45       Zone_B
   LINK    201      UNCI-1960
   LINK    202      PVC-1985


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[TANKS]
-------

**Purpose:**

  Defines all tank nodes contained in the network.

**Format:**

  One line for each tank containing:

    - ID label
    - Bottom elevation, ft (m)
    - Initial water level, ft (m)
    - Minimum water level, ft (m)
    - Maximum water level, ft (m)
    - Nominal diameter, ft (m)
    - Minimum volume, cubic ft (cubic meters)
    - Volume curve ID (optional)


**Remarks:**

  a. Water surface elevation equals bottom elevation plus water level.

  b. Non-cylindrical tanks can be modeled by specifying a curve of volume
     versus water depth in the [CURVES] section.

  c. If a volume curve is supplied the diameter value can be any non-zero
     number

  d. Minimum volume (tank volume at minimum water level) can be zero for a
     cylindrical tank or if a volume curve is supplied.

  e. A network must contain at least one tank or reservoir.


**Example:**

::

  [TANKS]
  ;ID   Elev.  InitLvl  MinLvl  MaxLvl  Diam  MinVol  VolCurve
  ;-----------------------------------------------------------
  ;Cylindrical tank
  T1    100     15       5       25     120    0
  ;Non-cylindrical tank with arbitrary diameter
  T2    100     15       5       25      1     0      VC1


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[TIMES]
-------

**Purpose:**

  Defines various time step parameters used in the simulation.

**Format:**

  ====================== =======================================
  **DURATION**           Value (units)
  **HYDRAULIC TIMESTEP** Value (units)
  **QUALITY TIMESTEP**   Value (units)
  **RULE TIMESTEP**      Value (units)
  **PATTERN TIMESTEP**   Value (units)
  **PATTERN START**      Value (units)
  **REPORT TIMESTEP**    Value (units)
  **REPORT START**       Value (units)
  **START CLOCKTIME**    Value (**AM/PM**)
  ---------------------- ---------------------------------------
  **STATISTIC**          **NONE/AVERAGED/MINIMUM/MAXIMUM/RANGE**
  ====================== =======================================

**Definitions:**

  **DURATION** is the duration of the simulation. Use 0 to run a single
  period snapshot analysis. The default is 0.

  **HYDRAULIC TIMESTEP** determines how often a new hydraulic state of
  the network is computed. If greater than either the **PATTERN** or
  **REPORT** time step it will be automatically reduced. The default is 1 hour.

  **QUALITY TIMESTEP** is the time step used to track changes in water
  quality throughout the network. The default is 1/10 of the hydraulic time step.

  **RULE TIMESTEP** is the time step used to check for changes in system status due 
  to activation of rule-based controls between hydraulic time steps. The default is 1/10 of the hydraulic time step.

  **PATTERN TIMESTEP** is the interval between time periods in all time patterns. The default is 1 hour.

  **PATTERN START** is the time offset at which all patterns will start. For example, a value of 6 hours
  would start the simulation with each pattern in the time period that corresponds to hour 6. The default is 0.

  **REPORT TIMESTEP** sets the time interval between which output results are reported. The default is 1 hour.

  **REPORT START** is the length of time into the simulation at which output results begin to be reported. The default is 0.

  **START CLOCKTIME** is the time of day (e.g., 3:00 PM) at which the simulation begins. The default is 12:00 AM midnight.

  **STATISTIC** determines what kind of statistical post-processing should be done on the time series of
  simulation results generated. **AVERAGED** reports a set of time-averaged results, **MINIMUM** reports
  only the minimum values, **MAXIMUM** the maximum values, and **RANGE** reports the difference between
  the minimum and maximum values. **NONE** reports the full time series for all quantities for all nodes and links and is the default.

**Remarks:**

  a. Units can be **SECONDS (SEC), MINUTES (MIN), HOURS**, or **DAYS**. The default is hours.

  b. If units are not supplied, then time values can be entered as decimal hours or in hours:minutes notation.

  c. All entries in the [TIMES] section are optional. Items offset by slashes (/) indicate allowable choices.


**Example:**

::

  [TIMES]
  DURATION           240 HOURS
  QUALITY TIMESTEP   3 MIN
  REPORT START       120
  STATISTIC          AVERAGED
  START CLOCKTIME    6:00 AM


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[TITLE]
-------

**Purpose:**

  Attaches a descriptive title to the network being analyzed.

**Format:**

  Any number of lines of text.

**Remarks:**

  The [TITLE] section is optional.


.. raw:: latex

    \newpage

.. only:: html

---------------------

..

[VALVES]
--------

**Purpose:**

  Defines all control valve links contained in the network.

**Format:**

  One line for each valve containing:

    - ID label of valve
    - ID of start node
    - ID of end node
    - Diameter, inches (mm)
    - Valve type
    - Valve setting
    - Minor loss coefficient


**Remarks:**

  a. Valve types and settings include:

    ================================== =========================
    Valve Type                         Setting
    PRV (pressure reducing valve)      Pressure, psi (m)
    PSV (pressure sustaining valve)    Pressure, psi (m)
    PBV (pressure breaker valve)       Pressure, psi (m)
    FCV (flow control valve)           Flow (flow units)
    TCV (throttle control valve)       Loss Coefficient
    GPV (general purpose valve)        ID of head loss curve
    ================================== =========================

  b. Shutoff valves and check valves are considered to be part of a pipe,
     not a separate control valve component (see [PIPES]).


.. raw:: latex

    \newpage

.. only:: html

---------------------

..


[VERTICES]
----------

**Purpose:**

  Assigns interior vertex points to network links.

**Format:**

  One line for each point in each link containing such points that
  includes:

    -  Link ID label
    -  X-coordinate
    -  Y-coordinate


**Remarks:**

  a. Vertex points allow links to be drawn as polylines instead of simple
     straight-lines between their end nodes.

  b. The coordinates refer to the same coordinate system used for node and
     label coordinates.

  c. A [VERTICES] section is optional and is not used at all when EPANET
     is run as a console application.


**Example:**

::

  [VERTICES]
  ;Link      X-Coord.     Y-Coord
  ;-------------------------------
   1          10023       128
   2          10056       95

.. raw:: latex

    \newpage

.. only:: html

---------------------

..