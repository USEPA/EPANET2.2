.. raw:: latex

    \clearpage


.. _error_messages:

Error Messages
=================

:numref:`table-errors` list EPANET error codes and their explanation.

.. tabularcolumns:: |p{2cm}|p{12.5cm}|

.. _table-errors:
.. table:: EPANET Error Codes	
  :class: longtable

  +-----------------------------------+-----------------------------------+
  |    *ID*                           |    *EXPLANATION*                  |
  +===================================+===================================+
  |    101                            | An analysis was terminated due to |
  |                                   | insufficient memory available.    |
  +-----------------------------------+-----------------------------------+
  |    110                            | An analysis was terminated        |
  |                                   | because the network hydraulic     |
  |                                   | equations could not be solved.    |
  |                                   | Check for portions of the network |
  |                                   | not having any physical links     |
  |                                   | back to a tank or reservoir or    |
  |                                   | for unreasonable values for       |
  |                                   | network input data.               |
  +-----------------------------------+-----------------------------------+
  |    200                            | One or more errors were detected  |
  |                                   | in the input data. The nature of  |
  |                                   | the error will be described by    |
  |                                   | the 200-series error messages     |
  |                                   | listed below.                     |
  +-----------------------------------+-----------------------------------+
  |    201                            | There is a syntax error in a line |
  |                                   | of the input file created from    |
  |                                   | your network data. This is most   |
  |                                   | likely to have occurred in .INP   |
  |                                   | text created by a user outside of |
  |                                   | EPANET.                           |
  +-----------------------------------+-----------------------------------+
  |    202                            | An illegal numeric value was      |
  |                                   | assigned to a property.           |
  +-----------------------------------+-----------------------------------+
  |    203                            | An object refers to undefined     |
  |                                   | node.                             |
  +-----------------------------------+-----------------------------------+
  |    204                            | An object refers to an undefined  |
  |                                   | link.                             |
  +-----------------------------------+-----------------------------------+
  |    205                            | An object refers to an undefined  |
  |                                   | time pattern.                     |
  +-----------------------------------+-----------------------------------+
  |    206                            | An object refers to an undefined  |
  |                                   | curve.                            |
  +-----------------------------------+-----------------------------------+
  |    207                            | An attempt is made to control a   |
  |                                   | check valve. Once a pipe is       |
  |                                   | assigned a Check Valve status     |
  |                                   | with the Property Editor, its     |
  |                                   | status cannot be changed by       |
  |                                   | either simple or rule-based       |
  |                                   | controls.                         |
  +-----------------------------------+-----------------------------------+
  |    208                            | Reference was made to an          |
  |                                   | undefined node. This could occur  |
  |                                   | in a control statement for        |
  |                                   | example.                          |
  +-----------------------------------+-----------------------------------+
  |    209                            | An illegal value was assigned to  |
  |                                   | a node property.                  |
  +-----------------------------------+-----------------------------------+
  |    210                            | Reference was made to an          |
  |                                   | undefined link. This could occur  |
  |                                   | in a control statement for        |
  |                                   | example.                          |
  +-----------------------------------+-----------------------------------+
  |    211                            | An illegal value was assigned to  |
  |                                   | a link property.                  |
  +-----------------------------------+-----------------------------------+
  |    212                            | A source tracing analysis refers  |
  |                                   | to an undefined trace node.       |
  +-----------------------------------+-----------------------------------+
  |    213                            | An analysis option has an illegal |
  |                                   | value (an example would be a      |
  |                                   | negative time step value).        |
  +-----------------------------------+-----------------------------------+
  |    214                            | There are too many characters in  |
  |                                   | a line read from an input file.   |
  |                                   | The lines in the .INP file are    |
  |                                   | limited to 255 characters.        |
  +-----------------------------------+-----------------------------------+
  |    215                            | Two or more nodes or links share  |
  |                                   | the same ID label.                |
  +-----------------------------------+-----------------------------------+
  |    216                            | Energy data were supplied for an  |
  |                                   | undefined pump.                   |
  +-----------------------------------+-----------------------------------+
  |    217                            | Invalid energy data were supplied |
  |                                   | for a pump.                       |
  +-----------------------------------+-----------------------------------+
  |    219                            | A valve is illegally connected to |
  |                                   | a reservoir or tank. A PRV, PSV   |
  |                                   | or FCV cannot be directly         |
  |                                   | connected to a reservoir or tank. |
  |                                   | Use a length of pipe to separate  |
  |                                   | the two.                          |
  +-----------------------------------+-----------------------------------+
  |    220                            | A valve is illegally connected to |
  |                                   | another valve. PRVs cannot share  |
  |                                   | the same downstream node or be    |
  |                                   | linked in series, PSVs cannot     |
  |                                   | share the same upstream node or   |
  |                                   | be linked in series, and a PSV    |
  |                                   | cannot be directly connected to   |
  |                                   | the downstream node of a PRV.     |
  +-----------------------------------+-----------------------------------+
  |    221                            | A rule-based control contains a   |
  |                                   | misplaced clause.                 |
  +-----------------------------------+-----------------------------------+
  |    223                            | There are not enough nodes in the |
  |                                   | network to analyze. A valid       |
  |                                   | network must contain at least one |
  |                                   | tank/reservoir and one junction   |
  |                                   | node.                             |
  +-----------------------------------+-----------------------------------+
  |    224                            | There is not at least one tank or |
  |                                   | reservoir in the network.         |
  +-----------------------------------+-----------------------------------+
  |    225                            | Invalid lower/upper levels were   |
  |                                   | specified for a tank (e.g., the   |
  |                                   | lower lever is higher than the    |
  |                                   | upper level).                     |
  +-----------------------------------+-----------------------------------+
  |    226                            | No pump curve or power rating was |
  |                                   | supplied for a pump. A pump must  |
  |                                   | either be assigned a curve ID in  |
  |                                   | its Pump Curve property or a      |
  |                                   | power rating in its Power         |
  |                                   | property. If both properties are  |
  |                                   | assigned then the Pump Curve is   |
  |                                   | used.                             |
  +-----------------------------------+-----------------------------------+
  |    227                            | A pump has an invalid pump curve. |
  |                                   | A valid pump curve must have      |
  |                                   | decreasing head with increasing   |
  |                                   | flow.                             |
  +-----------------------------------+-----------------------------------+
  |    230                            | A curve has non-increasing        |
  |                                   | X-values.                         |
  +-----------------------------------+-----------------------------------+
  |    233                            | A node is not connected to any    |
  |                                   | links.                            |
  +-----------------------------------+-----------------------------------+
  |    302                            | The system cannot open the        |
  |                                   | temporary input file. Make sure   |
  |                                   | that the EPANET Temporary Folder  |
  |                                   | selected has write privileges     |
  |                                   | assigned to it.                   |
  |                                   |                                   |
  +-----------------------------------+-----------------------------------+
  |    303                            | The system cannot open the status |
  |                                   | report file. See Error 302.       |
  +-----------------------------------+-----------------------------------+
  |    304                            | The system cannot open the binary |
  |                                   | output file. See Error 302.       |
  +-----------------------------------+-----------------------------------+
  |    308                            | Could not save results to file.   |
  |                                   | This can occur if the disk        |
  |                                   | becomes full.                     |
  +-----------------------------------+-----------------------------------+
  |    309                            | Could not write results to report |
  |                                   | file. This can occur if the disk  |
  |                                   | becomes full.                     |
  +-----------------------------------+-----------------------------------+

..