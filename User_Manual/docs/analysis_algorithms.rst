.. raw:: latex

    \clearpage


.. _analysis_algorithms:

Analysis Algorithms
===================

*EPANET uses a variety of algorithms for the hydraulic and water quality analysis. This chapter describes the two different demand models used for hydraulics analysis and the algorithms for the water quality analysis.*

-------

.. _sec-analysis_alg_hyd:

Hydraulics
~~~~~~~~~~

  The method used in EPANET to solve the flow continuity and headloss equations
  that characterize the hydraulic state of the pipe network at a given point in
  time can be termed a hybrid node-loop approach. Todini and Pilati (1987) and
  later Salgado et al. (1988) chose to call it the "Gradient Method". Similar
  approaches have been described by Hamam and Brameller (1971) (the "Hybrid
  Method) and by Osiadacz (1987) (the "Newton Loop-Node Method"). The only
  difference between these methods is the way in which link flows are updated
  after a new trial solution for nodal heads has been found. Because Todini's
  approach is simpler, it was chosen for use in EPANET.

  Todini (2003) describes mathematically how the iterative formulation of the Gradient Method could be 
  extended to simulate pressure driven demands (PDD). The latest version of EPANET hydraulic engine 
  has been updated to include PDD modeling capability. A water distribution pipe network
  can now be analyzed two ways, 1) assuming demand, and 2) assuming
  pressure driven demands. The subsections that follow provide a technical
  description for these two demand models.


**Fixed Demand Model**

  Assume we have a pipe network with :math:`{N}` junction nodes and :math:`{NF}`
  fixed grade nodes (tanks and reservoirs). Let the flow-headloss relation in a
  pipe between nodes :math:`i` and :math:`j` be given as:

  .. math::
     :label: eq:pipe_headloss

     H_{i} - H_{j} = h_{ij} = rQ_{ij}^{n} + mQ_{ij}^{2}

  where :math:`H` = nodal head, :math:`h` = headloss, :math:`r` = resistance
  coefficient, :math:`Q` = flow rate, :math:`n` = flow exponent, and :math:`m`
  = minor loss coefficient. The value of the resistance coefficient will depend
  on which friction headloss formula is being used (see below). For pumps,
  the headloss (negative of the head gain) can be represented by a
  power law of the form

  .. math::
     {h}_{ij} = {-\omega}^{2} ( {h}_{0} - r { ( {Q}_{ij}/{\omega} )}^{n} )

  where :math:`h_{0}` is the shutoff head for the pump, :math:`\omega` is a
  relative speed setting, and :math:`r` and :math:`n` are the pump curve
  coefficients. The second set of equations that must be satisfied is flow
  continuity around all nodes:

  .. math::
     :label: eq:node_continuity

        \sum_{j} {Q}_{ij} - {D}_{i} = 0  \\
        \mathit{for\ i = 1,... N}

  where :math:`D_{i}` is the flow demand at node :math:`i` and by convention,
  flow into a node is positive. For a set of known heads at the fixed grade
  nodes, we seek a solution for all heads :math:`H_{i}` and flows :math:`Q_{ij}`
  that satisfy Eqs. :eq:`eq:pipe_headloss` and :eq:`eq:node_continuity`.

  The Gradient solution method begins with an initial estimate of flows
  in each pipe that may not necessarily satisfy flow continuity. At
  each iteration of the method, new nodal heads are found by solving
  the matrix equation:

  .. math::
     :label: eq:matrix_form

     \boldsymbol{AH} = \boldsymbol{F}

  where :math:`A` = an :math:`(N \times N)` Jacobian matrix, :math:`H` = an
  :math:`(N \times 1)` vector of unknown nodal heads, and :math:`F` = an :math:`(N \times 1)`
  vector of right hand side terms.

  The diagonal elements of the Jacobian matrix are:

  .. math::
     {A}_{ij}= \sum_{j} \frac{1}{g_{ij}}


  while the non-zero, off-diagonal terms are:

  .. math::
     {A}_{ij} = -\frac{1}{g_{ij}}

  where :math:`g_{ij}` is the derivative of the headloss in the link
  between nodes :math:`i` and :math:`j` with respect to flow. For pipes, when
  resistance coefficient is not a function of flow rate,

  .. math::
     {g}_{ij} = nr {{ | Q_{ij} | }^{n - 1}} + 2m | Q_{ij} |

  when resistance coefficient is a function of the flow rate, specifically as
  in Darcy-Weisbach head loss equation with turbulent flow condition,

  .. math::
     {g}_{ij} = nr {{ | Q_{ij} | }^{n - 1}} + \frac{\partial r}{\partial Q_{ij}}|Q_{ij}|^n + 2m | Q_{ij} |

  Zero flows can cause numerical instability in the GGA solver (Gorev et al., 2013; Elhay and Simpson, 2011).
  When flow approaches zero, a linear relationship is assumed between head loss and
  flow to prevent :math:`{g}_{ij}` from reaching zero. The value of :math:`{g}_{ij}`
  is capped at a specific value when the flow is smaller than what is defined by the specified minimum :math:`{g}`.

  For pumps, the derivative is:

  .. math::
     {g}_{ij} = n \omega^{2} r ({Q}_{ij}/{\omega} )^{n-1}


  Each right hand side term consists of the net flow imbalance at a
  node plus a flow correction factor:

  .. math::
     :label: eq:matrix_rhs

     {F}_{i} = \sum_{{j}} \left( Q_{ij} + y_{ij} \right) - {D}_{i} + \sum_{f} \frac{H_{f}}{g_{ij}}

  where the last term applies to any links connecting node :math:`i` to a fixed
  grade node :math:`f` and the flow correction factor :math:`y_{ij}` is:

  .. math::
     y_{ij} = \frac{( r{ | {Q}_{ij} | }^{n} + m { | {Q}_{ij} | }^{2} )sgn ( {Q}_{ij} )}{g_{ij}}


  for pipes and

  .. math::
     y_{ij} =  \frac{- {\omega}^{2} ( {h}_{0} - r { ( { Q}_{ij }/{\omega} ) }^{n} )}{g_{ij}}

  for pumps, where :math:`sgn(x)` is :math:`1` if :math:`x > 0` and :math:`-1`
  otherwise (:math:`Q_{ij}` is always positive for pumps).

  After new heads are computed by solving Eq. :eq:`eq:matrix_form`,
  new flows are found from:

  .. math::
     :label: eq:flow_update

     {Q}_{ij} = {Q}_{ij} - \left( y_{ij}-\frac{({H}_{i} - {H}_{j})}{g_{ij}} \right)

  The flow update formula always results in flow continuity around each
  node after the first iteration.

  Iterations continue until some suitable convergence criterion based on
  residual errors associated with :eq:`eq:pipe_headloss` and
  :eq:`eq:node_continuity` is met. If convergence does not occur then
  Eqs. :eq:`eq:matrix_form` and :eq:`eq:flow_update` are solved again.

  EPANET uses several different hydraulic convergence criteria. Versions 2.0
  and earlier based accuracy on the absolute flow changes relative to the
  total flow in all links. Gorev et al. (2013), however, observed that this
  criterion did not guarantee convergence towards the exact solution and
  proposed two new ones based on max head error and max flow change.
  These two criteria have been added to EPANET v2.2 as options that provide more
  rigorous control over hydraulic convergence.


.. _press_driven_analysis:

**Pressure Driven Demand Model**

  Now consider the case where the demand at a node :math:`i`, :math:`d_{i}`,
  depends on the pressure head :math:`p_{i}` available at the node (where
  pressure head is hydraulic head :math:`h_{i}` minus elevation :math:`E_{i}`).
  There are several different forms of pressure dependency that have been
  proposed. Here we use Wagner’s equation (Wagner et al., 1988):

  .. math::
     :label: eq:wagners

     d_{i} =
       \left\{
         \begin{array}{l l}
           D_{i}                                                           & p_{i} \ge P_{f}     \\
           D_{i} \left( \frac{p_{i} - P_{0}}{P_{f} - P_{0}} \right) ^{e}   & P_{0} < p_i < P_{f} \\
           0                                                               & p_{i} \le P_{0}
         \end{array}
       \right.

  :math:`D_{i}` is the full normal demand at node :math:`i` when the pressure
  :math:`p_{i}` equals or exceeds :math:`P_{f}`, :math:`P_{0}` is the pressure
  below which the demand is 0, and :math:`e` is an exponent usually set equal
  to 0.5 (to mimic flow through an orifice).

  Eq. :eq:`eq:wagners` can be inverted to express head loss through a virtual
  link as a function of the demand flowing out of node :math:`i` to a virtual
  reservoir with fixed pressure head :math:`P_{0} + E_{i}`:

  .. math::
     :label: eq:inv_wagner

       h_{i} - P_{0} - E_{i} = R_{di} d_{i}^{e}

  where :math:`E_{i}` is the node’s elevation and
  :math:`R_{di} = (P_{f} - P_{0})/D_{i}^{e}` is the link’s resistance
  coefficient. This expression can be folded into the GGA matrix equations,
  where the pressure driven demands :math:`d_{i}` are treated as the
  unknown flows in the virtual links that honor constraints in Eq.
  :eq:`eq:wagners`.

  The head loss :math:`h_{d}` and its gradient :math:`g_{d}` through the
  virtual link can be evaluated as follows (with node subscripts
  suppressed for clarity):

  1. If the current demand flow :math:`d` is greater than the full Demand
     :math:`D`:

     .. math::
        \begin{gathered}
          g_{d} = R_{\text{HIGH}} \\
          h_{d} = R_{d} D^{e} + R_{\text{HIGH}}(d - D)
        \end{gathered}

     where :math:`R_{\text{HIGH}}` is a large resistance factor
     (e.g. 10\ :sup:`9`).

  2. If the current demand flow :math:`d` is less than zero:

      .. math::
        \begin{gathered}
          g_{d} = R_{\text{HIGH}} \\
          h_{d} = R_{\text{HIGH}}d 
        \end{gathered}

  3. Otherwise Eq. :eq:`eq:inv_wagner` is used to evaluate the head loss and
     gradient:

     .. math::
        \begin{gathered}
          g_{d} = e R_{d} \left| d \right|^{e - 1} \\
          h_{d} = g_{d} d / e
        \end{gathered}

     when the calculated gradient :math:`g_d` is smaller than the low flow resistance threshold :math:`RQtol` (with default value :math:`10^{-7}`, the gradient is fixed and 
     linear relationship between the gradient and head loss is used:

     .. math::
        \begin{gathered}
          g_{d} = RQtol \\
          h_{d} = g_{d} d
        \end{gathered}


  These head loss and gradient values are then incorporated into the normal
  set of GGA matrix equations as follows:

  1. For the diagonal entry of :math:`A` corresponding to node *i*:

     .. math::
        A_{ii} = A_{ii} + 1/g_{di}

  2. For the entry of :math:`F` corresponding to node :math:`i`:

     .. math::
        F_{i} = F_{i} + D_{i} - d_{i} + \left( h_{di} + E_{i} + P_{0} \right) / g_{di}


  Note that :math:`D_{i}` is added to :math:`F_{i}` to cancel out having
  subtracted it from the original :math:`F_{i}` value appearing in Eq.
  :eq:`eq:matrix_rhs`.

  After a new set of nodal heads is found, the demands at node :math:`i` are
  updated using Eq. :eq:`eq:flow_update` which takes the form:

  .. math::
     d_{i} = d_{i} - ( h_{di} - h_{i} + E_{i} + P_{0} ) / g_{di}


  The following assumptions apply to the implementation of PDD in EPANET:

  -  A global set of minimum :math:`P_{0}` and full :math:`P_{f}` (or nominal)
     pressure limits apply to all nodes.

  -  In extended period analysis, where the full demands change at
     different time periods, the same pressure driven demand function
     is applied to the current full demand (instead of changing :math:`P_{f}`
     to accommodate changes in :math:`D_{f}` ).


**EPANET Implementation**

  EPANET implements the fixed demand and PDD models for hydraulics using the
  procedure outlined in this section.

  #. The linear system of equations :eq:`eq:matrix_form` is solved using
     a sparse matrix method based on multiple minimum-degree node re-ordering
     (Liu 1985). After re-ordering the nodes to minimize the amount of fill-
     in for matrix :math:`A`, a symbolic factorization is carried out so that
     only the non-zero elements of A need be stored and operated on in memory.
     For extended period simulation this re-ordering and factorization is only
     carried out once at the start of the analysis.

  #. For the very first iteration, the flow in a pipe is chosen equal to
     the flow corresponding to a velocity of 1 ft/sec, while the flow
     through a pump equals the design flow specified for the pump. For closed links, the flow is 
     set to be :math:`{10}^{-6}` cfs. (All computations are made with head in feet and flow in cfs).

  #. The resistance coefficient for a pipe (:math:`r`) is computed as described
     in Table 3.1. For the Darcy-Weisbach headloss equation, the friction
     factor :math:`f` is computed by different equations depending on the flow’s
     Reynolds Number (:math:`Re`):

     Hagen – Poiseuille formula for :math:`Re < 2,000` (Bhave, 1991):

     .. math::
        f = \frac{64}{Re}


     Swamee and Jain approximation to the Colebrook - White equation for
     Re > 4,000 (Bhave, 1991):

     .. math::
        f = \frac{0.25}{{ \left[ \log_{10} \left( \frac{\epsilon}{3.7d} + \frac{5.74}{{Re}^{0.9} } \right) \right] }^{2}}


     Cubic Interpolation From Moody Diagram for :math:`2,000 < Re < 4,000`
     (Dunlop, 1991):

     .. math::
        \begin{gathered}
           f = (X1 + R  (X2 + R (X3 + X4))) \\
           R = \frac{Re}{2000}
        \end{gathered}

     .. math::
        \begin{gathered}
           X1 = 7FA - FB \\
           X2 = 0.128 - 17 FA + 2.5 FB \\
           X3 = -0.128 + 13 FA - 2 FB \\
           X4 = R ( 0.032 - 3 FA + 0.5 FB ) \\
           FA = { ( Y3 )}^{-2} \\
           FB = FA ( 2 - \frac{0.00514215}  {( Y2 )  ( Y3 ) } ) \\
           Y2 = \frac{\epsilon} {3.7d} + \frac{5.74}{{4000}^{0.9}} \\
           Y3 = -0.86859 \ln \left( \frac{\epsilon}{3.7d} + \frac{5.74}{{4000}^{0.9}} \right)
        \end{gathered}

     where :math:`\epsilon` = pipe roughness and :math:`d` = pipe diameter.

     Based on friction factor equations described above and Darcy-Weisbach equation in :numref:`table-pipe_headloss_formulas`,
     resistance coefficient is not a function of flow and linear relationship exists between head loss
     and flow when Re > 2000. If Re > 2000, resistance coefficient depends on pipe flow and the
     sensitivity of resistance  coefficient to flow needs to be computed in order to calculate :math:`{g}_{ij}`
     for the pipe.


  #. The minor loss coefficient based on velocity head (:math:`K`) is converted
     to one based on flow (:math:`m`) with the following relation:

     .. math::

        m = \frac{ 0.02517K} {d^{4}}


  #. Emitters at junctions are modeled as a fictitious pipe between the
     junction and a fictitious reservoir. The pipe’s headloss parameters
     are :math:`n = (1/\gamma)`, :math:`r = (1/C)^n`, and :math:`m = 0` where
     :math:`C` is the emitter’s discharge coefficient and :math:`\gamma` is its
     pressure exponent. The head at the fictitious reservoir is the elevation of
     the junction. The computed flow through the fictitious pipe becomes the
     flow associated with the emitter.

  #. Open valves with no minor loss are assigned a low resistance factor and linear headloss relationship,
     i.e., :math:`h = 10^{-6} Q`, so that the gradient :math:`g = 10^{-6}` and the flow correction factor :math:`y=Q`.
     For open valves with minor loss, the value of :math:`r` is assumed to be zero. When the valve flow is smaller than a threshold value,
     low resistance factor and linear head loss relationship are assumed: :math:`g = RQtol` and :math:`y = Q`; otherwise
     quadratic head loss relationship is used: :math:`g = 2.0mQ` and :math:`y = Q/2`. The low flow resistance tolerance :math:`RQtol`
     defines the flow threshold value for using linear or quadratic headloss relationship. As mentioned above, the default value of :math:`RQtol` is :math:`10^{-7}`.
     Closed links are assumed to obey a linear headloss relation with
     a large resistance factor: :math:`h = 10^{8} Q`, meaning :math:`g =
     10^{8}` and :math:`y = Q`. 

  #. Status checks on pressure control valves (PRVs and PSVs) are made after each iteration; while
     status checks on pumps, check valves (CVs), flow control valves, and
     pipes connected to full/empty tanks by default are made after every other
     iteration up until the 10th iteration. After this, status checks are made only after initial convergence is achieved.

  #. During status checks, pumps are closed if the head gain is greater
     than the shutoff head (to prevent reverse flow). Similarly, check
     valves are closed if the headloss through them is negative (see
     below). When these conditions are not present, the link is re-opened.
     A similar status check is made for links connected to empty/full
     tanks. Such links are closed if the difference in head across the
     link would cause an empty tank to drain or a full tank to fill. If the tank allows overflow, the links filling the
     full tank are not closed. The closed links are re-opened at the next status check if such conditions no longer
     hold.

  #. Simply checking if :math:`h < 0` to determine if a check valve should be
     closed or open was found to cause cycling between these two states in
     some networks due to limits on numerical precision. The following
     procedure was devised to provide a more robust test of the status of
     a check valve (CV):

      ::

        if |h| > Htol then
          if h < -Htol then     status = CLOSED
          if Q < -Qtol then     status = CLOSED
          else                  status = OPEN

        else
          if Q < -Qtol then   status = CLOSED
          else                  status = unchanged

     where by default Htol = 0.0005 ft and Qtol = 0.0001 cfs.

  #. If the status check closes an open pump, pipe, or CV, large resistance factor and linear head loss relationship are applied: 
     :math:`g = 10^{8}` and :math:`y=Q`. If a pump is re-opened, its flow is
     computed by applying the current head gain to its characteristic
     curve. If a pipe or CV is re- opened, its flow is determined by
     solving Eq. :eq:`eq:pipe_headloss` for :math:`Q` under the current
     headloss :math:`h`.

  #. Matrix coefficients for pressure breaker valves (PBVs) are set to
     the following: :math:`g = 10^{-8}` and :math:`y = 10^{8} Hset`,
     where :math:`Hset` is the pressure drop setting for the valve (in feet).
     Throttle control valves (TCVs) are treated as valves with :math:`g` and :math:`y` as
     described in item 6 above and :math:`m` taken as the converted value of
     the valve setting (see item 4 above).

  #. Matrix coefficients for pressure reducing, pressure sustaining, and
     flow control valves (PRVs, PSVs, and FCVs) are computed after all
     other links have been analyzed. Status checks on PRVs and PSVs are
     made as described in item 7 above. These valves can either be
     completely open, completely closed, or active at their pressure or
     flow setting.

  #. The logic used to test the status of a PRV is as follows:

        ::

          If current status = ACTIVE then
            if Q < -Qtol              then new status = CLOSED
            if Hi < Hset + Hml – Htol then new status = OPEN
                                      else new status = ACTIVE

          If curent status = OPEN then
            if Q < -Qtol              then new status = CLOSED
            if Hj >= Hset + Htol      then new status = ACTIVE
                                      else new status = OPEN

          If current status = CLOSED then
            if  Hi >= Hset + Htol
            and Hj <  Hset – Htol     then new status = Active

            if  Hi < Hset - Htol
            and Hi > Hj + Htol        then new status = OPEN
                                      else new status = CLOSED

        where Q is the current flow through the valve, Hi is its upstream
        head, Hj is its downstream head, Hset is its pressure setting
        converted to head, Hml is the minor loss when the valve is fully opened (=
        mQ\ :sup:`2`), and Htol and Qtol are the same values used for check
        valves in item 9 above. A similar set of tests is used for PSVs, except that
        when testing against Hset, the i and j subscripts are switched as are
        the > and < operators.

  #. Flow through an active PRV is maintained to force continuity at its
     downstream node while flow through a PSV does the same at its
     upstream node. For an active PRV from node i to j:

     .. math::
        \frac{1}{g_{ij}} = 0

     .. math::
        {F}_{j} = {F}_{j} + {10}^{8} Hset

     .. math::
        {A}_{jj} = {A}_{jj} + {10}^{8}

     This forces the head at the downstream node to be at the valve
     setting Hset. Flow balance is foreced at the downtream node.
     An equivalent assignment of coefficients is made for an
     active PSV except the subscript for F and A is the upstream node i and the flow balance is forced at 
     the upstream node. Coefficients for open/closed PRVs and PSVs are handled in the same
     way as for valves.

  #. For an active FCV from node i to j with flow setting Qset, Qset is
     added to the flow leaving node i and entering node j, and is
     subtracted from *F\ i* and added to *F\ j*. If the head at node i is
     less than that at node j, then the valve cannot deliver the flow and
     it is treated as an open pipe.

  #. Initial convergence is checked at every iteration and after initial convergence is achieved 
     (no change in status for PRVs and PSVs plus flow convergence based 
     on total flow change, optional maximum head error and maximum flow change, and the check of unexpected 
     negative demand and negative pressure when PDD is used in analysis), another status check on pumps,
     CVs, FCVs, and links to tanks is made. Also, the status of links
     controlled by pressure switches (e.g., a pump controlled by the
     pressure at a junction node) is checked. If any status change
     occurs, the iterations continue. Otherwise, a final solution has been obtained.

  #. For extended period simulation (EPS), the following procedure is
     implemented:

     a. After a solution is found for the current time period, the time
        step for the next solution is the minimum of:

       -  The time until a new demand period begins

       -  The shortest time for a tank to fill or drain,

       -  The shortest time until a tank level reaches a point that
          triggers a change in status for some link (e.g., opens or
          closes a pump) as stipulated in a simple control

       -  The next time until a simple timer control on a link kicks in

       -  The next time at which a rule-based control causes a status
          change somewhere in the network

       In computing the times based on tank levels, the latter are assumed
       to change in a linear fashion based on the current flow solution. The
       activation time of rule-based controls is computed as follows:

       -  Starting at the current time, rules are evaluated at a rule time
          step. Its default value is 1/10 of the normal hydraulic time step
          (e.g., if hydraulics are updated every hour, then rules are evaluated
          every 6 minutes).

       -  Over this rule time step, clock time is updated, as are the water
          levels in storage tanks (based on the last set of pipe flows
          computed).

       -  If a rule's conditions are satisfied, then its actions are added to a
          list. If an action conflicts with one for the same link already on
          the list then the action from the rule with the higher priority stays
          on the list and the other is removed. If the priorities are the same
          then the original action stays on the list.

       -  After all rules are evaluated, if the list is not empty then the new
          actions are taken. If this causes the status of one or more links to
          change then a new hydraulic solution is computed and the process
          begins anew.

       -  If no status changes were called for, the action list is cleared and
          the next rule time step is taken unless the normal hydraulic time
          step has elapsed.

     b. Time is advanced by the computed time step, new demands are found,
        tank levels are adjusted based on the current flow solution, and link
        control rules are checked to determine which links change status.

     c. A new set of iterations with Eqs. :eq:`eq:matrix_form` and
        :eq:`eq:flow_update` are begun at the current set of flows.


.. _sec-analysis_alg_WQ:

Water Quality
~~~~~~~~~~~~~

  The governing equations for EPANET’s water quality solver are based
  on the principles of conservation of mass coupled with reaction
  kinetics. The following phenomena are represented (Rossman et al.,
  1993; Rossman and Boulos, 1996):

**Advective Transport in Pipes**


  A dissolved substance will travel down the length of a pipe with the
  same average velocity as the carrier fluid while at the same time
  reacting (either growing or decaying) at some given rate.
  Longitudinal dispersion is usually not an important transport
  mechanism under most operating conditions. This means there is no
  intermixing of mass between adjacent parcels of water traveling down
  a pipe. Advective transport within a pipe is represented with the
  following equation:

  .. _eq-advec_trans:
  .. math::
     :label: eq:advec_trans

     \frac{ \partial {C}_{i}} {\partial t} = - u_{i} \frac{\partial{C}_{i}}{\partial x} + r({C}_{i})
  ..

  where :math:`C_i` = concentration (mass/volume) in pipe :math:`i` as a
  function of distance :math:`x` and time :math:`t`, :math:`u_i` = flow
  velocity (length/time) in pipe :math:`i`, and :math:`r` = rate of reaction
  (mass/volume/time) as a function of concentration.
 

**Mixing at Pipe Junctions**

  At junctions receiving inflow from two or more pipes, the mixing of
  fluid is taken to be complete and instantaneous. Thus the
  concentration of a substance in water leaving the junction is simply
  the flow-weighted sum of the concentrations from the inflowing pipes.
  For a specific node :math:`k` one can write:

  .. math::
     :label: eq:nodal_mixing

     C_{i|x=0} = \frac{\sum_{ j \in I_k} Q_{j} C_{j|x= L_j}+Q_{k,ext} C_{k,ext}} {\sum_{j \in I_k} Q_j + Q_{k,ext}}

  where :math:`i` = link with flow leaving node :math:`k`, :math:`I_k` = set
  of links with flow into :math:`k`, :math:`L_j` = length of link :math:`j`,
  :math:`Q_j` = flow (volume/time) in link :math:`j`, :math:`Q_{k,ext}` =
  external source flow entering the network at node :math:`k`, and
  :math:`C_{k,ext}` = concentration of the external flow entering at node
  :math:`k`. The notation :math:`C_{i|x=0}` represents the concentration at
  the start of link :math:`i`, while :math:`C_{i|x=L}` is the concentration
  at the end of the link.


**Mixing in Storage Facilities**

   -  Complete Mixing Model

      It is convenient to assume that the contents of storage facilities
      (tanks and reservoirs) are completely mixed (:numref:`fig-complete_mix`). This is a reasonable
      assumption for many tanks operating under fill-and-draw conditions
      providing that sufficient momentum flux is imparted to the inflow
      (Rossman and Grayman, 1999). Under completely mixed conditions the
      concentration throughout the tank is a blend of the current contents
      and that of any entering water. At the same time, the internal
      concentration could be changing due to reactions. The following
      equation expresses these phenomena:

      .. math::
         :label: eq:tank_mixing

         \frac{\partial ({V}_{s} {C}_{s}) }{\partial t} = \sum_{i \in I_{s}} {Q}_{i}{C}_{i | x={L}_{i}} - \sum_{j \in O_{s}} {Q}_{j}{C}_{s} + r({V}_{s}{C}_{s})

      where :math:`V_s` = volume in storage at time :math:`t`, :math:`C_s` =
      concentration within the storage facility, :math:`I_s` = set of links
      providing flow into the facility, and :math:`O_s` = set of links withdrawing
      flow from the facility, and :math:`r` = rate of reaction (mass/time) in storage.


   - Two Compartment Mixing Model

     It is assumed that both the inlet-outlet zone (first compartment) and main zone (second compartment) of the storage facility are completely mixed (:numref:`fig-two_comp_mix`)
     For the inlet-outlet zone concentration :math:`{C}_{s1}`,

     .. math::
         :label: eq:tank_mixing21

         \frac{\partial ({V}_{s1} {C}_{s1}) }{\partial t} = \sum_{i \in I_{s}} {Q}_{i}{C}_{i | x={L}_{i}} - \sum_{j \in O_{s}} {Q}_{j}{C}_{s1} - {Q}_{12}{C}_{s1} + {Q}_{21}{C}_{s2}+r({V}_{s1}{C}_{s1})

     For the main zone concentration :math:`{C}_{s2}`,

      .. math::
         :label: eq:tank_mixing22

         \frac{\partial ({V}_{s2} {C}_{s2}) }{\partial t} = {Q}_{12}{C}_{s1} - {Q}_{21}{C}_{s2}+r({V}_{s2}{C}_{s2})
  
     where :math:`{V}_{s1}` = the inlet-outlet zone water volume, :math:`{V}_{s2}` = main zone volume, :math:`Q_{12}` = rate of flow from the inlet-outlet zone to the main zone, and :math:`Q_{21}` = rate of flow 
     from the main zone to the inlet-outlet zone. When the facility is filling, water flows into the inlet-outlet zone and if the inlet-outlet zone is full, water flows from it into the main zone. 
     So :math:`Q_{21}=0` when the facility is filing. When the facility is emptying, water flows out of the inlet-otlet zone and if the main zone is not empty, water flows from 
     the main zone into the inlet-outlet zone. :math:`Q_{12}=0` when the storage facility is emptying.


   - First-In First-Out Plug Flow Model
   
     FIFO storage facility is shown in :numref:`fig-FIFO_plug`. Water enters the last segment of the facility when it fills and water in the first segment leaves the facility when it drains.
     The stodage facility water quality is represented by the water quality in first segment. 
     For the first segment concentration :math:`{C}_{sf}`,

     .. math::
         :label: eq:tank_mixing3f

         \frac{\partial ({V}_{sf} {C}_{sf}) }{\partial t} = -\sum_{j \in O_{s}} {Q}_{j}{C}_{sf}  + r({V}_{sf}{C}_{sf})

     For the last segment concentration :math:`{C}_{sl}`,

      .. math::
         :label: eq:tank_mixing3l

         \frac{\partial ({V}_{sl} {C}_{sl}) }{\partial t} = \sum_{i \in I_{s}} {Q}_{i}{C}_{i | x={L}_{i}}+ r({V}_{sl}{C}_{sl})
 

   - Last-In First-Out Plug Flow Model

     LIFO storage facility is shown in :numref:`fig-LIFO_plug`. Water enters the last segment of the facility when it fills and leaves the last segment when it drains.
     The storage facility water quality is represented by the water quality in the last segment.   
     
     .. math::
         :label: eq:tank_mixing4

         \frac{\partial ({V}_{sl} {C}_{sl}) }{\partial t} = \sum_{i \in I_{s}} {Q}_{i}{C}_{i | x={L}_{i}} - \sum_{j \in O_{s}} {Q}_{j}{C}_{sl} + r({V}_{sl}{C}_{sl})


**Bulk Flow Reactions**

  While a substance moves down a pipe or resides in storage it can
  undergo reaction with constituents in the water column. The rate of
  reaction can generally be described as a power function of concentration:

  .. math::
     r = k{ C}^{n }

  where :math:`k` = a reaction constant and :math:`n` = the reaction order.
  When a limiting concentration exists on the ultimate growth or loss of a
  substance then the rate expression becomes

  .. math::
     \begin{gathered}
       R = {K}_{b} ({C}_{L}-C) {C}^{n-1} \ \ \ \ \ \ \ \ \ \mathit{for\ n > 0, K_b > 0}
     \end{gathered}

  .. math::
     \begin{gathered}
       R = {K}_{b} (C - {C}_{L} ) {C}^{n - 1} \ \ \ \ \ \ \ \ \ \mathit{for\ n > 0, K_b < 0}
     \end{gathered}

  where :math:`C_L` = the limiting concentration.

  Some examples of different reaction rate expressions are:

    - *Simple First-Order Decay* (:math:`C_L = 0, K_b < 0, n = 1`):

       .. math::
          R = {K}^{b}C

       The decay of many substances, such as chlorine, can be modeled
       adequately as a simple first-order reaction.

    - *First-Order Saturation Growth* (:math:`C_L > 0, K_b > 0, n = 1`):

       .. math::
          R = {K}_{b} ( {C}_{L} - C )

       This model can be applied to the growth of disinfection by-products,
       such as trihalomethanes, where the ultimate formation of by-product
       (:math:`C_L`) is limited by the amount of reactive precursor present.

    - *Two-Component, Second Order Decay* (:math:`C_L \neq 0, K_b < 0, n = 2`):

       .. math::
          R = {K}_{b} C({C}_{L} - C)

       This model assumes that substance A reacts with substance B in some
       unknown ratio to produce a product P. The rate of disappearance of A
       is proportional to the product of A and B remaining. :math:`C_L` can be
       either positive or negative, depending on whether either component A
       or B is in excess, respectively. Clark (1998) has had success in
       applying this model to chlorine decay data that did not conform to
       the simple first-order model.

    - *Michaelis-Menton Decay Kinetics* (:math:`C_L > 0, K_b < 0, n < 0`):

       .. math::
          R = \frac{{K}_{b}C} {{C}_{L} - C}

       As a special case, when a negative reaction order *n* is specified,
       EPANET will utilize the Michaelis-Menton rate equation, shown above
       for a decay reaction. (For growth reactions the denominator becomes
       :math:`C_L + C`.) This rate equation is often used to describe
       enzyme-catalyzed reactions and microbial growth. It produces first-
       order behavior at low concentrations and zero-order behavior at
       higher concentrations. Note that for decay reactions, :math:`C_L` must
       be set higher than the initial concentration present.

       Koechling (1998) has applied Michaelis-Menton kinetics to model
       chlorine decay in a number of different waters and found that both
       :math:`K_b` and :math:`C_L` could be related to the water’s organic
       content and its ultraviolet absorbance as follows:

       .. math::
          {K}_{b} = -0.32\ UVA^{1.365 }\frac{( 100\ UVA )} {DOC}

       .. math::
          {C}_{L} = 4.98\ UVA - 1.91\ DOC


       where :math:`UVA` = ultraviolet absorbance at 254 nm (1/cm) and
       :math:`DOC` = dissolved organic carbon concentration (mg/L).

       Note: These expressions apply only for values of :math:`K_b` and
       :math:`C_L` used with Michaelis-Menton kinetics.


    - *Zero-Order growth* (:math:`C_L = 0, K_b = 1, n = 0`)

       .. math::
          R = 1.0

       This special case can be used to model water age, where with each
       unit of time the “concentration” (i.e., age) increases by one unit.

       The relationship between the bulk rate constant seen at one
       temperature (T1) to that at another temperature (T2) is often
       expressed using a van’t Hoff - Arrehnius equation of the form:

       .. math::
          {K}_{b2}={K}_{b1}{\theta}^{T2 - T1}

       where :math:`\theta` is a constant. In one investigation for chlorine,
       :math:`\theta` was estimated to be 1.1 when :math:`T1` was 20 deg. C
       (Koechling, 1998).


**Pipe Wall Reactions**

  While flowing through pipes, dissolved substances can be transported
  to the pipe wall and react with material such as corrosion products
  or biofilm that are on or close to the wall. The amount of wall area
  available for reaction and the rate of mass transfer between the bulk
  fluid and the wall will also influence the overall rate of this
  reaction. The surface area per unit volume, which for a pipe equals 2
  divided by the radius, determines the former factor. The latter
  factor can be represented by a mass transfer coefficient whose value
  depends on the molecular diffusivity of the reactive species and on
  the Reynolds number of the flow (Rossman et. al, 1994). For first-
  order kinetics, the rate of a pipe wall reaction can be expressed as:

  .. math::
     r = \frac{ 2 k_w k_f C } { R (k_w + k_f) }

  where :math:`k_w` = wall reaction rate constant (length/time),
  :math:`k_f` = mass transfer coefficient (length/time), and :math:`R` = pipe
  radius. For zero-order kinetics the reaction rate cannot be any higher
  than the rate of mass transfer, so

  .. math::
     r = \min ( k_w, k_f C) ( 2/R )

  where :math:`k_w` now has units of mass/area/time.

  Mass transfer coefficients are usually expressed in terms of a
  dimensionless Sherwood number (:math:`Sh`):

  .. math::
     {k}_{f} = Sh \frac{D}{d}

  in which :math:`D` = the molecular diffusivity of the species being
  transported (length :sup:`2` /time) and :math:`d` = pipe diameter. In
  fully developed laminar flow, the average Sherwood number along the length
  of a pipe can be expressed as

  .. math::
     Sh = 3.65 + \frac{0.0668 ( d/L )Re\ Sc} {1 + 0.04{ [ ( d/L )Re\ Sc ]}^{2/3}}

  in which :math:`Re` = Reynolds number and :math:`Sc` = Schmidt number
  (kinematic viscosity of water divided by the diffusivity of the chemical)
  (Edwards et.al, 1976). For turbulent flow the empirical correlation
  of Notter and Sleicher (1971) can be used:

  .. math::
     Sh = 0.0149{Re}^{0.88}{Sc}^{1/3}


**System of Equations**

  When applied to a network with complete mixing storage facilities, Eqs. :eq:`eq:advec_trans` - :eq:`eq:tank_mixing` represent a
  coupled set of differential/algebraic equations with time-varying
  coefficients that must be solved for :math:`C_i` in each pipe :math:`i`
  and :math:`C_s` in each storage facility :math:`s`. This solution is
  subject to the following set of externally imposed conditions:

  - Initial conditions that specify :math:`C_i` for all :math:`x` in each
    pipe :math:`i` and :math:`C_s` in each storage facility :math:`s` at
    time 0

  - Boundary conditions that specify values for :math:`C_k,ext` and
    :math:`Q_{k,ext}` for all time :math:`t` at each node :math:`k` which
    has external mass inputs

  - Hydraulic conditions which specify the volume :math:`V_s` in each
    storage facility :math:`s` and the flow :math:`Q_i` in each link
    :math:`i` at all times :math:`t`

  For storage facilities that can not be assumed to be complete mixing, :eq:`eq:tank_mixing` needs to be replaced by the equation of the
  corresponding mixing model.

**Lagrangian Transport Algorithm**

  EPANET’s water quality simulator uses a Lagrangian time-based
  approach to track the fate of discrete parcels of water as they move
  along pipes and mix together at junctions between fixed-length time
  steps (Liou and Kroon, 1987). These water quality time steps are
  typically much shorter than the hydraulic time step (e.g., minutes
  rather than hours) to accommodate the short times of travel that can
  occur within pipes. As time progresses, the size of the most upstream
  segment in a pipe may increase as water enters the pipe while an equal
  loss in size of the most downstream segment occurs as water leaves
  the link; therefore, the total volume of all the segments within a pipe
  does not change and the size of the segments between these leading and
  trailing segments remains unchanged (see :numref:`fig-transport`).


   .. _fig-transport:
   .. figure:: media/transport.png
      :alt: Behavior of Segments in the Lagrangian Solution Method
      :align: center
      :scale: 80%
	  
      Behavior of segments in the Lagrangian solution method.
   ..


  The following steps occur within each such time step:

  #. The water quality in each segment is updated to reflect any reaction
     that may have occurred over the time step.

  #. For each node in topological order (from upstream to downstream):

     a. If the node is a junction or tank, the water from the leading
        segments of the links with flow into it, if not zero, is blended
        together to compute a new water quality value. The volume
        contributed from each segment equals the product of its link’s
        flow rate and the time step. If this volume exceeds that of the
        segment, then the segment is destroyed and the next one in line
        behind it begins to contribute its volume.

     b. If the node is a junction its new quality is computed as its total
        mass inflow divided by its total inflow volume. If it is a tank,
        its quality is updated depending on the method used to model
        mixing in the tank (see below).

     c. The node’s concentration is adjusted by any contributions made by
        external water quality sources.

     d. A new segment is created in each link with flow out of the node.
        Its volume equals the product of the link flow and the time step
        and its quality equals the new quality value computed for the node.

  To cut down on the number of segments, new ones are only created if
  the new node quality differs by a user-specified tolerance from that of
  the last segment in the outflow link. If the difference in quality is
  below the tolerance, then the size of the current last segment in the
  link is simply increased by the volume flowing into the link over the
  time step and the segment quality is a volume-weighted average of the
  node and segment quality.

  This process is then repeated for the next water-quality time step. At
  the start of the next hydraulic time step any link experiencing a flow
  reversal has the order of its segments reversed and if any flow
  reversal occurs the network’s nodes are re-sorted topologically, from
  upstream to downstream. Sorting the nodes topologically allows the
  method to conserve mass and reduce the potential mass balance error experienced with EPANET 2.0 (Davis et al., 2018) 
  even when very short pipes or zero-length pumps
  and valves are encountered. Initially each pipe in the network consists
  of a single segment whose quality equals the initial quality assigned to
  the upstream node.



.. include:: image_subdefs.rst
