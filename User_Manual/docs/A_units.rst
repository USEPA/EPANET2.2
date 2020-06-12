.. raw:: latex

    \renewcommand{\thetable}{\Alph{chapter}.\arabic{table}}
    \renewcommand{\theliteralblock}{\Alph{chapter}.\arabic{literalblock}}

.. _units:

Units of Measurement
=======================

:numref:`table-units` list EPANET parameters and their associated US and SI units.


.. tabularcolumns:: |p{4cm}|p{5.5cm}|p{5cm}|

.. _table-units:
.. table:: EPANET Parameters and Associated Units	

  +-----------------------+-----------------------+-----------------------+
  |    *PARAMETER*        | *US CUSTOMARY*        | *SI METRIC*           |
  +=======================+=======================+=======================+
  |    Concentration      | mg/L or ug/L          | mg/L or ug/L          |
  +-----------------------+-----------------------+-----------------------+
  |    Demand             | (see Flow units)      | (see Flow units)      |
  +-----------------------+-----------------------+-----------------------+
  |    Diameter (Pipes)   | inch                  | millimeter            |
  +-----------------------+-----------------------+-----------------------+
  |    Diameter (Tanks)   | foot                  | meter                 |
  +-----------------------+-----------------------+-----------------------+
  |    Efficiency         | percent               | percent               |
  +-----------------------+-----------------------+-----------------------+
  |    Elevation          | foot                  | meter                 |
  +-----------------------+-----------------------+-----------------------+
  |    Emitter            | flow unit/            | flow unit/            |
  |    Coefficient        | (psi)\ :sup:`1/2`     | (meter)\ :sup:`1/2`   |
  +-----------------------+-----------------------+-----------------------+
  |    Energy             | kilowatt - hour       | kilowatt - hour       |
  +-----------------------+-----------------------+-----------------------+
  |    Flow               |  CFS (cu foot/sec)    |  LPS (liter/sec)      |
  |                       |                       |                       |
  |                       |  GPM (gal/min)        |  LPM (liter/min)      |
  |                       |                       |                       |
  |                       |  MGD (Million gal/day)|  MLD (Megaliter/day)  |
  |                       |                       |                       |
  |                       |  IMGD (Imperial MGD)  |  CMH (cubic meter/hr) |
  |                       |                       |                       |
  |                       |  AFD (ac-foot/day)    |  CMD (cubic meter/day)|
  +-----------------------+-----------------------+-----------------------+
  |    Friction Factor    | unitless              | unitless              |
  +-----------------------+-----------------------+-----------------------+
  |    Hydraulic Head     | foot                  | meter                 |
  +-----------------------+-----------------------+-----------------------+
  |    Length             | foot                  | meter                 |
  +-----------------------+-----------------------+-----------------------+
  | Minor Loss Coefficient| unitless              | unitless              |
  +-----------------------+-----------------------+-----------------------+
  |    Power              | horsepower            | kilowatt              |
  +-----------------------+-----------------------+-----------------------+
  |    Pressure           | pounds per square     | meter                 |
  |                       | inch                  |                       |
  +-----------------------+-----------------------+-----------------------+
  | Reaction Coefficient  | 1st-order   1/day     | 1st-order    1/day    |
  | (Bulk)                |                       |                       |
  +-----------------------+-----------------------+-----------------------+
  | Reaction Coefficient  | 0-order  mass/L/day   | 0-order  mass/L/day   |
  | (Wall)                |                       |                       |
  |                       | 1st-order  ft/day     | 1st-order  meter/day  |
  +-----------------------+-----------------------+-----------------------+
  | Roughness Coefficient | Darcy-Weisbach        | Darcy-Weisbach        |
  |                       | 10\ :sup:`-3`\ foot   | millimeter            |
  |                       |                       |                       |
  |                       | Otherwise  unitless   | Otherwise  unitless   |
  +-----------------------+-----------------------+-----------------------+
  |    Source Mass        | mass/minute           | mass/minute           |
  |    Injection          |                       |                       |
  +-----------------------+-----------------------+-----------------------+
  |    Velocity           | foot/second           | meter/second          |
  +-----------------------+-----------------------+-----------------------+
  |    Volume             | cubic foot            | cubic meter           |
  +-----------------------+-----------------------+-----------------------+
  |    Water Age          | hour                  | hour                  |
  +-----------------------+-----------------------+-----------------------+

..

  **Note**: US Customary units apply when CFS, GPM, AFD, or MGD is
  chosen as flow units. SI Metric units apply when flow units are
  expressed using either liters or cubic meters.