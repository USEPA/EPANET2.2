Quality Assurance Project Plan
==============================

Research and Development on U.S. EPA’s Software Application for Modeling Hydraulic and Water Quality Behavior of Drinking Water Distribution Systems:  EPANET
---------------------------------------------------------------------------

#### US EPA / ORD / CESER / Water Infrastructure Division / Drinking Water Management Branch

#### January 2020

**List of Acronyms**

ASC - Advanced Simulation & Computing

CESER - Center for Environmental Solutions and Emergency Response

EPA - Environmental Protection Agency

GIS - Graphical Information System

GPU - Graphics Processing Unit

LOF - Level of Formality

ORD - Office of Research and Development

OW - Office of Water

OWA - Open Water Analytics

QA - Quality Assurance

QAPP - Quality Assurance Project Plan

QC - Quality Control

RAP - Research Action Plan

UI - User Interface

U.S. EPA - United States Environmental Protection Agency

VOTD - Version of the Day

WDN - Water Distribution Network

WDSA - Water Distribution System Analysis

WST - Water Security Tool

WNTR - Water Network Tool for Resilience

**Table of Contents**

-   Introduction

-   Project Management

-   Model and Algorithm Development

-   Software Engineering

-   Data Acquisition and Management

-   Software Verification

Introduction
============

This document describes the Quality Assurance (QA) program for U.S.
Environmental Protection Agency (EPA) Office of Research and Development
(ORD)'s development and maintenance of its software application for modeling 
hydraulic and water quality behavior of drinking water distribution systems, 
known as EPANET. Development of tools such as EPANET for the simulation of 
hydraulics and water quality in the nations drinking water systems is central 
to EPA's mission of protecting public health.  EPANET is widely used to plan, 
design, analyze, and regulate drinking water distribution systems.  EPANET is 
also an important component of CESER's water security and resilience tools and 
has been adopted by third party commercial vendors as the computational core 
of their derivative products.

ORD developed EPANET in the 1990's out of a need to improve understanding of 
hydraulic and water quality processes that occur in WDNs. Major work
on EPANET ceased in the early 2000's. EPANET was last updated (minor) in
2008. It is important that EPANET be well engineered and maintained
software that is fully capable of supporting needed important stakeholder
activities. CESER will work in close cooperation with EPANET developer and 
User Community to develop and maintain EPANET software. This project is focused 
on advancing EPANET development and maintenance through community cooperation,
modernizing the code base, and integrating new features in response to
EPA mission driven decisions and stakeholder needs.

This project is performed under the authority granted by Congress to the
EPA within the Safe Drinking Water Act. This work directly supports
drinking water research activities within ORD and water regulatory
activities of the Office of Water (OW). This project seeks to help communities
efficiently direct WDN infrastructure investments by providing utilities
with modeling tools for planning and design.

This document provides a description of the practices employed in this
project to ensure the quality of the research and development activities
conducted under it. The practices described here are adapted from the
software quality practices described in the Advanced Simulation &
Computing (ASC) Software Quality Plan (SNL, 2004), EPA Requirements for
Quality Assurance Project Plans (EPA, 2001), EPA Guidance for Quality
Assurance Project Plans for Modeling (EPA, 2002), and ORD Policy and
Procedures Manual Section 13.9 Modeling Quality Assurance and
Documentation (EPA, 2015). ASC is a Department of Energy (DOE) program
that is focused on developing advanced modeling and simulation
capabilities that leverage high-performance computing resources. This
ASC derivative plan has been successfully used to manage project quality
in two EPA water distribution system modeling software projects, specifically
WST and WNTR.

Project Description
-------------------

This project focuses on extending and maintaining EPANET in partnership 
with the community of stakeholders. To accomplish these tasks an extensible 
software architecture is needed that facilitates the development of modular
feature sets by stakeholders inside and outside the EPA.

Some of the projects that may be undertaken include accelerating EPANET
hydraulic and water quality algorithms through parallelization using
commercially available software technologies, integrating EPANET-MSX, and
features from the WST and WNTR toolkits and other EPANET-based software
tools. Features from other toolkits may be modified to adapt them to the
general planning, design, and daily operational analysis tasks for which
EPANET is commonly used. EPA will also work with the broader Water
Distribution System Analysis (WDSA) research community and our
stakeholders to encourage the development of innovative new features for
the EPANET.

Approaches developed as part of this research will be added to EPANET
and its user interface (UI) application. As part of this effort,
software implementations of algorithms and methodologies will be
delivered. In particular, these technologies will be encapsulated in
modules with clearly defined interfaces and parameters. Documentation
will be provided for the algorithm implementation in software, as well
as the installation and use of the software for these applications. This
includes a description of the dependency on any third-party software.

Project Objectives
------------------

The primary objective of this project is to perform in house research
and development on the EPANET UI application and computational engines and their
features and to work with open-source development communities and other
stakeholder organizations on integrating their feature contributions.
EPA project team will employ software development best practices related
to design, development, testing, documentation, delivery, installation,
and maintenance to ensure cost effectiveness of this project and
performance of the resulting software applications.

This research project comprises three major objectives:

1.  To deliver EPANET's new UI Application with graphical information
    system (GIS) functionality, Python scripting capabilities, and
    support for third party plugins.

2.  To modernize and maintain EPANET's computational engines, e.g., 
    ensuring the use of modular and extensible software library that utilizes 
    multi-threading and graphics processing unit (GPU) computing capabilities to accelerate
    innovative WDN analysis applications by EPA, our stakeholders, and third party commercial developers.

3.  To harness EPANET as a technology transfer channel for delivering
    EPA's EPANET-based tools to a wide audience of potential users.

Project Schedule
----------------

The goal of this project is to establish and coordinate a development regimen for
EPANET that results in a cyclical schedule of periodic feature releases
and software patches as needed. Periodic feature releases will integrate
EPA and community feature development. Software patches will be
developed in response to software quality issues as they are identified.
Best practices in software development, quality assurance, and software
verification as described in this document will be utilized to minimize
the incidence and severity of software quality issues.

A bi-annual software release schedule is envisioned where EPANET UI and
engine feature releases will occur frequently. Software patches
will be developed and released on demand as the need arises. Separating
feature release and software patches in this manner will help improve
software quality since defects are frequently introduced along with new
features. Establishing a release schedule will better serve our
stakeholders who prefer release stability over a longer time horizon
allowing them to better coordinate their own internal development
activities with EPA's software releases.

Task Breakdown of Objectives
----------------------------

Task 0. Quality Assurance Plan

Project Task 1. EPANET Engine Development\
Subtask 1.1 EPANET regression testing\
Subtask 1.2 EPANET release\
Subtask 1.3 EPANET refactoring\
Subtask 1.4 EPANET feature development

Project Task 2. EPANET UI Application Development\
Subtask 2.1 Acceptance testing\
Subtask 2.2 UI debugging\
Subtask 2.3 UI release\
Subtask 2.3 UI refactoring\
Subtask 2.4 UI feature development

Quality Definition and Goals
----------------------------

Quality is defined as the degree of excellence of something. Software
quality can be measured in several meaningful ways:

> • Develop robust, reliable WDN modeling tools that can be effectively
> applied to realistic large-scale problems,\
> • Solve real-world water problems as described by our stakeholders,\
> • Satisfy the stated and implied needs, budget, and schedules of the
> EPA and our stakeholders,\
> • Be technically innovative, cost efficient, and\
> • Ensure continual quality improvement of the ongoing research and
> development activities.

The quality assurance/quality control (QA/QC) goals for this software
development project include the following:

> • Objectivity---each step in the software development cycle should use
> methods that are explicitly stated and adhered to

> • Thoroughness---all elements of the study should be carried out and
> documented in a thorough manner

> • Consistency---all work should be performed and documented in a
> consistent manner

> • Transparency---the programming code, testing scripts, and
> documentation should clearly describe the assumptions and methods used
> and verify the capabilities specified in the functional requirements.

Guidelines and procedures are needed to govern software development. Our
goal is to create EPANET software of the highest utility and quality.
This goal motivates the following QA practices.

Quality Assurance Practices
---------------------------

This document adopts the breakdown of QA practices that are similar to
those adopted in WST and WNTR projects QA/QC documentation and EPA Requirements
for Quality Assurance Project Plans (QAPPs) for Modeling:

> • Project Management\
> • Computational Modeling and Algorithm Development\
> • Software Engineering\
> • Data Generation and Acquisition\
> • Model and Software Verification\
> • Training

The Assessment and Oversight category included in the EPA QAPP was
intentionally omitted here. In its place, each practice is associated
with artifacts that reflect how those practices will be assessed.

Each of these QA categories is described in the remaining sections of
this document. An overview description provides a high-level discussion
of the practices that are involved in each category, along with
associated artifacts. The practices are specific research, development,
and deployment activities, which generate artifacts: deliverables and
work products that can be used to quantify compliance with QA goals.
Whenever possible, metrics and measurements are used to provide
quantitative insight into the effective quality of the process that is
being followed.

Project Management
==================

Software development can be thought of as a process not unlike technical
writing, where a first draft is written and then through an iterative
process of revision a final version is produced. Exploratory feature
development is analogous to a rough first draft. Subsequent revisions
improve on the initial version in a process of continual improvement.
Project management is the systematic approach for balancing the project
work to be done, resources required, methods used, procedures to be
followed, schedules to be met, and the way that the project is
organized. The unique aspects of the software development process need
to be reflected in the project management strategy to efficiently
achieve desired outcomes.

Determination of Applicable Practices and Level of Formality
------------------------------------------------------------

The level of formality (LOF) for a project relates to how important it
is to perform practices in detail given their consequences. For example,
a basic research project will likely have a low LOF, since it is
exploratory in nature. However, a project working on a capability that
directly impacts the potential loss of human life would have a high LOF.
As such, it is very important to maintain QA documentation for many
practices.

High formality requires detailed documentation covering all aspects of
the project; formal reviews inviting all stakeholders and other
necessary experts; detailed and approved test plans; and customer
waivers for deviation from any required practice or specification. Low
formality allows documentation such as project notebooks and emails;
less formal team-reviews; limited testing of research code until it is
determined that the code will become a deliverable; and other relaxed
procedures as approved by the project officer. Medium LOF is in between
these two extremes.

#### PR1. Perform a risk-based assessment to determine level of formality and applicable practices.

Exploratory research on feature development will have a low LOF. White
papers and research prototype software codes are the expected
deliverables. Communication on these activities will occur during team
meetings and will be informally documented. Mature software applications
will have a medium-high LOF. This same risk based approach will also be
applied to open source contributions as they are integrated with the
code base. These applications will include software testing, version
control, and documentation. Communication on these applications will
occur during team meetings and on various Github.com software repository sites
managed by U.S. EPA, e.g., <https://github.com/USEPA/EPANET2.2>. and at recognized
community development sites, e.g., <https://github.com/OpenWaterAnalytics/EPANET>,
and through documented meetings.

Requirements Analysis
---------------------

The purpose of requirements analysis practices is to capture, develop,
validate, track, and control the project requirements. These
requirements typically span hardware, software, operations, support,
documentation, product training, and other aspects. Requirements are
based upon project mission, stakeholders' stated and implied needs, and
organizational commitments. Although needs are not requirements they are
considered along with requirements in order to improve quality.
Requirements are inputs to other practice areas.

#### PR2. Identify, gather and manage stakeholders' expectations and requirements.

EPA and community members are important stakeholders of this project and help to
define and implement the project requirements. Members of the EPA project team will be
regularly updated through team meetings. These meetings might be open to
other stakeholders (e.g., open source community members, such as Open
Water Analytics (OWA) at <https://github.com/OpenWaterAnalytics/EPANET/wiki>, 
EPA's OW staff or water utility staff) as needed.

#### PR3. Derive, negotiate, manage, and trace requirements. 

Project requirements are broadly stated in the Statement of Work for
project tasks, by and in cooperation with the community stakeholders, e.g., OWA, and this QAPP. 
These requirements will be refined as the project continues. Negotiation, management, 
and tracking of these requirements will occur during research group meetings, documented in meeting notes,
and formalized and implemented through the software repositories of the U.S. EPA and the community 
contributors and stakeholders (e.g., <https://github.com/OpenWaterAnalytics/EPANET>).

Risk Management
---------------

Risk management is the activity of identifying, addressing, and
mitigating sources of risk before they become threats to successful
completion of a project. A risk is a combination of the consequence and
likelihood of an event. Risk management spans the lifetime of the
project. This practice area seeks to identify only primary and
reasonably likely risks in the following areas: organizational,
regulatory, technical, and project management. Risk management is
intended to mitigate consequences and/or likelihood of these identified
risk events. For example, the project team would like to mitigate the
risks of not completing a task on time.

#### PR4. Identify and analyze risk events.

Risk events will be identified during the writing of the Statement of
Work and in project meetings and will continue to be discussed as this
project continues. Risks to this project include: unexpected reductions
in budget; tasks taking more hours than anticipated; unanticipated
software bugs that are not easy to fix; and team members leaving this
project for the short or long term. Additional risk events will be
identified and analyzed in meetings and documented as necessary.

#### PR5. Define, monitor, and implement the risk response.

Tasks are included in this project in order to mitigate risks, and these
include: maintaining the software in a version control repository;
continuous improvements of test coverage; performing a standard suite of
tests prior to each public release; routine updates to the GitHub site
(https://github.com/USEPA/Water-Distribution-Network-Model) and EPA's
EPANET website (<https://www.epa.gov/water-research/epanet>) to
communicate new features; regular communication on budget and personnel
issues between EPA project officers and EPA management.

Project Planning, Tracking, and Oversight
-----------------------------------------

The purpose of project planning, tracking, and oversight is to guide
project implementation while balancing, monitoring, and analyzing
project quality, cost (including cost of quality), schedule, and
performance. Project planning includes preparing a plan that describes
how the project will be performed and managed. The plan typically
includes at least a scope of work, project constraints and goals,
project deliverables, a project timeline, an assessment of required
resources, and the availability of the resources. Tracking and oversight
includes taking corrective actions as necessary. Corrective actions
bring projected accomplishments and results back into compliance.
Corrective actions could include adding resources to meet schedules,
modifying the schedule, adding project budget, modifying cost criteria,
and re-negotiating requirements or acceptance criteria.

#### PR6. Create and manage the project plan

The "project plan" is the aggregate of the individual tasks in the scope
of work. The team will review and discuss the project plan during team
meetings.

#### PR7. Track project performance versus project plan and implement needed (corrective) actions.

Any significant deviations from the project plan will be reported to EPA
management and discussed during team meetings. EPA project officers will
determine any necessary corrective actions.

Model and Algorithm Development
===============================

Modeling and analysis capabilities can be applied to gain insight into
an application. In some contexts, these activities can be separated,
such as when the goal of a project is focused simply on developing a
detailed model, or when a given model is assumed and the focus is on
developing algorithms that can provide insight into this model. More
generally, modeling and algorithmic development are often closely
related activities. In many contexts, algorithmic issues arise in the
design/implementation of software that can effectively model large-scale
systems. Similarly, in combinatorial applications modeling and
algorithmic design are often closely related because the combinatorial
structure is used to design the algorithm. However, these activities can
be distinguished from software engineering efforts, which are more
specifically focused on ensuring that software generated has high
quality itself.

Model and Algorithmic Design
----------------------------

The model design process includes activities like theoretical
development, mathematical formulation, and identification of input data.
Algorithmic design is often closely coupled with model design, as
algorithmic issues arise when deciding how to formulate models and how
to analyze their properties. This practice area focuses on activities
that ensure that these design activities accurately reflect and abstract
the properties of the underlying physical or conceptual process that is
being modeled.

Modeling assumptions, related algorithmic formulations, and the
limitations of these capabilities will be reviewed and critiqued
internally during team meetings or dedicated peer reviews depending upon
the required LOF. Preliminary reviews focus on initial ideas or
direction for a specific work product. These reviews seek both a "sanity
check" and consider possible alternatives. Detailed reviews focus on the
completed work product to ensure its acceptability and typically will
invite customer participation. Additionally, external reviews are
conducted through peer-reviewed journal articles and conferences.

#### PR8. Document designs for models and algorithms

Designs for new models and algorithms will be documented in the user
manuals, white papers, conference proceedings, and/or peer reviewed
journal articles.

#### PR9. Conduct peer reviews of modeling assumptions and algorithmic formulations

All new models incorporated into software produced under this project
will be peer reviewed during the traditional journal peer review
processes, will be reproduced from already published peer review
articles, or developed and approved by the open source community (e.g.,
OWA).

Preliminary Software Development
--------------------------------

Preliminary software development efforts are targeted at developing
"proof of concept" demonstrations that modeling and algorithmic
techniques are effective for research purposes. These efforts typically
employ small-scale or synthetic data sets to demonstrate the
capabilities of the software. Furthermore, software design processes are
generally minimized in favor of rapidly generating a basic modeling or
analytic capability. Preliminary software development is done at low LOF
unless specifically (by task and work breakdown) required to be at
higher LOF.

#### PR10. Document preliminary software implementation

Preliminary software implementations will be documented in the code
itself, with command-line help features, and/or simple 'readme' files.
As the implementations mature, they will be documented in the software
user manuals. The extent and formality of the software's documentation
will evolve along with the software itself.

Testing Models and Algorithmic Techniques
-----------------------------------------

Model testing is needed to characterize the uncertainty that can be
expected in modeling outputs given uncertainties in model designs along
with uncertainties in data used to apply models. Similarly, algorithmic
tests are needed to confirm that analytical predictions match expected
values. The following practices ensure that testing will be done to
validate models and algorithms in this manner.

#### PR11. Unit testing and peer-review of modeling and algorithmic outputs

Automated unit tests will be developed to test individual modules of
software packages against a suite of test problems with known solutions.
All modeling and algorithmic outputs will be reviewed by the project
team and external users.

Software Engineering
====================

Software engineering is a systematic approach to the specification,
design, development, test, operation, support, and retirement of
software. The software engineering activities identified in this section
are software development, integration of third party software,
configuration management, and release and distribution management. Note
that preliminary software development was addressed earlier and does not
follow this section.

Software Development
--------------------

The purpose of software development processes is to generate a correctly
working product for the stakeholder; this product is often, but not
always, software. Generally, software development processes include
design, implementation, and testing of the software products or reuse of
existing implementations. The specific instantiation of these practices
depends on the LOF. Preliminary reviews of work products are done within
the team. Final reviews invite external participation and are generally
more formal.

#### PR12. Communicate and review design

Software development design will be discussed during team meetings,
described in the software documentation, and in white papers when
appropriate. All changes to the code will be automatically tracked at
GitHub.com/USEPA website with open access to the public
(https://github.com/USEPA/Water-Distribution-Network-Model).

#### PR13. Create required software and product documentation

As changes to the software are made, documentation will be updated. All
changes to the code will be recorded by the GitHub websites. Whenever a
change is made, the GitHub site instantaneously documents all changes to
the code, the person who made the changes, and the date and time of the
change. User manuals will be updated on a bi-annual basis to reflect the
latest release. Software and product documentation will be available at
all times to all users via GitHub
(https://github.com/USEPA/Water-Distribution-Network-Model) and EPA's
EPANET (https://www.epa.gov/water-research/epanet) websites.

#### PR14. Create and implement software tests

Tests have been developed (and will continue to be developed) to ensure
that the software codes (e.g., EPANET and the UI application) build
effectively and perform correctly against a set of test input files.
These tests are conducted automatically and test results will be
available to the public via the web. Additional tests are being
developed as part of this project, e.g., code-coverage analysis will be
used to quantitatively ensure that software tests are comprehensive.

Integration of Third Party or Other Software
--------------------------------------------

Some software uses or incorporates third party or other existing
software products in order to satisfy needed capabilities without
incurring the cost of redeveloping those capabilities. Such software
might be a simple library, an integrated set of libraries, compilers and
linkers, or even an operating system. Sources of such software might be
commercial, open source, other EPA projects, or research efforts. This
practice area focuses on integration activities such as identifying,
tracking, establishing trust in, assimilating, or honoring agreements
(for example, protecting intellectual property) for third party or other
existing software products.

#### PR15. Identify and track third party software products and follow applicable agreements

Third party products necessary for this project include operating
systems, compilers, run-time libraries, and related tools. These are
trusted products from reputable vendors and open source projects. In
addition, the software developed for this project will be run on
multiple platforms (and hence using different hardware, operating
systems, compilers, and run-time libraries) which will allow comparisons
to detect any significant third party product problems which might
impact this project. EPA may use third party products obtained from
reliable vendors. These are considered to be state-of-the-art modeling
and analysis tools by the academic and business communities. EPA assumes
no "ownership" of these third party products. If it is necessary to
modify an existing third-party GitHub project in this effort, EPA will
make a GitHub fork of that project to contain our changes. This will
allow us to easily incorporate new changes made in the original project.
If we make improvements of interest to the original project, this
strategy will easily support offering our changes to them. All releases
of the software (e.g., EPANET) will include applicable licenses for
these third party tools and users must agree to the terms and conditions
of all software packages.

Configuration Management
------------------------

The purpose of configuration management is to provide a controlled
environment for development, production, and support activities.
Configuration management includes identifying which software product
artifacts are to be managed; maintaining version controlled baselines of
these artifacts; providing an issue tracking system for recording
associated issues or change requests related to product artifacts; and
tracking the status of these issues throughout the project's lifetime.
Configuration management must ensure retrieval of any baseline artifact
over the project's lifetime.

#### PR16. Perform version control of identified software product artifacts

A software repository has been set up for EPANET using GitHub and can be
found at https://github.com/USEPA/Water-Distribution-Network-Model and
at https://github.com/USEPA/SWMM-EPANET\_User\_Interface.

#### PR17. Record and track issues associated with the software product.

EPA will document and manage issues for EPANET using the GitHub Issue
manager. Issues might include software bugs, new feature requests, or
other problems identified by team members or outside users. The GitHub
manager automatically records all submitted issues, assigns them to a
project team member to solve, and sends automated email reminders. These
issues will be discussed regularly during team meetings.

#### PR18. Ensure backup and disaster recovery of software product artifacts.

Backup and disaster recovery processes are performed by the system
administrators. Software and related artifacts are stored on EPA
workstations that are backed up on a weekly basis.

Release and Distribution Management
-----------------------------------

The purpose of the release and distribution practices is to manage
versions of the software product that are distributed to stakeholders
and other external users. Release management includes handling the
requests for a release as well as preparation of the release. A release
might include all elements of the product or a defined subset of the
product. When the project team has completed all artifacts necessary for
a release, the team creates a baseline in preparation for distribution.
The baseline product undergoes release certification before being
distributed and supported. Release certification ensures that all
release criteria are satisfied, that identified release artifacts are
adequately reviewed, and that all planned testing is completed and
satisfactory.

#### PR19. Plan and generate the release package.

Official release versions of EPANET software will be produced at least
bi-annually. The release includes updated documentation, updated GitHub
websites, and testing reports. The most current version of the software
under active development is continuously available via GitHub. The
software repository will include instructions for developers interested
in working with the latest unstable release.

#### PR20. Certify that the software product (code and its related artifacts) is ready for release and distribution.

EPA will use software tests to certify that software is ready for
release and distribution. Software tests will be run automatically when
changes are made to the repository and performance metrics will be
defined that must be satisfied prior to release. For example, best
software practices suggest that code coverage tests should evaluate 90%
of files and 60% of lines of code. EPA will provide a software testing
report documenting the results tests and testing metrics. Additionally,
EPA will manually evaluate the software release before public
distribution and perform cross-platform portability tests. EPA staff
will also review all updates to user manuals.

Data Acquisition and Management
===============================

Input data for model development and application efforts are typically
collected outside of the modeling effort or generated by other models or
processing software. These data need to be properly assessed to verify
that a model characterized by these data would yield predictions with an
acceptable level of uncertainty. To this end, the following practices
address various aspects of data acquisition, the calibration of the
model based on these data, management of the data, and the
software/hardware configuration needed for data processing.

Model Calibration
-----------------

Models used for computational analyses require input data that relates
to a particular application context. These models often require
calibration of modeling parameters using this input data. These
practices document the procedures for calibrating the model that will
perform the designated predictive task, including records for how
calibration is performed and maintained.

The objective of model calibration is to determine a set of model
parameters that provide a fit of the model to the observed data that is
somehow optimal under initial and boundary conditions that would be
expected in normal operations of the model. Identification of these
model parameters can be accomplished by trial and error calibration or
inverse parameter estimation. Typically, there is not a unique set of
model parameters that will provide the optimal fit, and acceptance
criteria defining the acceptable level of mismatch between the model and
the observed data are determined. These criteria incorporate the
repeatability of the instruments that created the data set and this
repeatability information is obtained from the source of the data.
Calibration practices include recording the mismatch in the data as a
function of different input parameter values. The frequency with which
model calibration must be conducted is dependent on the data, the model
and the intended use of the mode; however, any time model parameters are
varied, the calibration process is repeated.

#### PR21. Document objectives and methods of model calibration activities \[acceptance criteria, frequency, method of assessing goodness-of-fit\]

Model calibration is not required for this stage of research. If any new
model calibration activities arise, the project plan will be modified to
define the required model calibration activities, including the
objectives and methods that are required.

#### PR22. Document sources of input data used for calibration.

Model calibration is not anticipated for this project. If it does occur,
all input sources of data will be documented.

Non-Direct Measurements
-----------------------

Some types of data needed for project implementation or decision making
are obtained from non-measurement sources such as computer databases,
programs, literature files, and historical databases. The following
practices document these data sources and describe the intended use of
this data.

#### PR23. Identify requirements for non-direct data and how this data will be acquired \[e.g., quality standards required for this data\]

A very small amount of non-direct data is needed for network analysis,
such as population statistics, water usage rates, and health data. This
data is obtained from peer reviewed journals and expert sources. Any
non-direct data needed for this research will be maintained under
version control and documented in user manuals or peer reviewed journal
articles.

Data Management
---------------

Data management occurs at many stages of a project, including initial
data acquisition, data transmission within the project team, data
processing, and final use. The following practices document the
procedures for data management to help ensure high confidence in final
analyses based on this data.

#### PR24. Develop processes for managing data \[e.g., labeling process, archiving policy, addressing data sensitivities\]

Test datasets are currently maintained in the version controlled
software repository for each project. Test datasets include network
models and water quality data sets. Output files from specific analyses
are also maintained on the software repository along with the input
specifications to replicate results. Backups of all data will be made on
EPA computers. Datasets that are not released as part of the software
release are maintained in separate private project related software
repositories on EPA computers.

#### PR25. Document hardware and software used to process data.

The research has been carried out on Windows workstations. The primary
compute engine for this effort is a 64-bit Windows workstation. Support
for other platforms such as Linux and Mac OS will be developed and
documented.

Software Verification
=====================

The purpose of software verification is to ensure (1) that
specifications are adequate with respect to intended use and (2) that
specifications are accurately, correctly, and completely implemented.
Software verification also attempts to ensure product characteristics
necessary for safe and proper use are addressed. Software verification
occurs throughout the entire product lifecycle.

Software verification activities are an integral part of software
development, operation, and support practices. In this context, the goal
is to detect potential problems as early as possible. Software artifacts
to be verified typically include specifications, requirements, design,
code, third party libraries, software verification plan, test cases,
product documentation, and training package. If these artifacts are
changed, retesting and reevaluation of the changes will need to occur.

#### PR26. Develop and maintain a software verification plan.

In this project, software verification includes: team review of models,
algorithms, and code; peer review of user manuals, tutorial documents,
and webinars; automated software tests and documentation (included in
the software release packages); issue tracking through the GitHub site.

#### PR27. Conduct tests to demonstrate that acceptance criteria are met and to ensure that previously tested capabilities continue to perform as expected.

Software products will be evaluated with a suite of tests that ensure
that software capabilities perform as expected. These tests will be
automatically applied after software updates are made, and they will
also be used to verify the behavior of software releases. Software tests
will include unit testing, functional testing, integration testing, and
code coverage testing. Baselines are established to ensure that the
software performs as expected after changes are made to the code base.
Software will not be released if tests indicate that the software is not
performing as expected.

#### PR28. Conduct independent technical reviews to evaluate adequacy with respect to requirements.

EPA will define the independent technical reviews that are required for
software verification, including peer reviews of user manuals and other
training materials. For this project, technical reviews of documentation
and training materials are completed by team members and by technical
peer review. Software is tested by team members and outside users. The
GitHub sites are used to communicate reviews of the software to the
development team.

Training
========

The goal of training practices is to enhance the skills and motivation
of a staff that is already highly trained and educated in the areas of
mathematical modeling, scientific software development, algorithms,
engineering, and/or computer science. This practice addresses training
needs of the project teams especially for, but not limited to, following
the project teams' process implementation. The purpose of training is to
develop the skills and knowledge of individuals and teams so they can
fulfill their process and technical roles and responsibilities. Project
teams need to ensure that the training needs of the project are
satisfied in accordance with their project plan.

#### PR29. Determine project team training needed to fulfill assigned roles and responsibilities.

The development of advanced capabilities for water simulation problems
requires advanced training in mathematics and computer science, as well
as an understanding of water systems (particularly water distribution
systems). The principal training requirements for the EPA staff will be
developing a greater familiarity of water distribution systems and
related applied mathematical techniques. The technical staff will keep
up-to-date on research pertaining to water distribution networks by
attending technical conferences, reading scientific journals, and
participating in team meetings.

References
==========

1\. EPA, 2001. EPA Requirements for Quality Assurance Project Plans, EPA
QA/R-5, available at http://www.epa.gov/quality/qs-docs/r5-final.pdf.

2\. EPA, 2002. Guidance for Quality Assurance Project Plans for Modeling,
EPA QA/G-5M, EPA/240/R-02/007, available at
http://www.epa.gov/QUALITY/qs-docs/g5m-final.pdf.

3\. EPA, 2015. ORD Policy and Procedures Manual Section 13.9 Modeling
Quality Assurance and Documentation, available at
http://intranet.ord.epa.gov/about-ord/section-1309-modeling-quality-assurance-and-documentation.

4\. SNL, 2005. Sandia National Laboratories Advanced Simulation and
Computing (ASC) Software Quality Plan, Part 1: ASC Software Quality
Engineering Practices, Version 1.0, Sandia Report SAND2004-6602.
