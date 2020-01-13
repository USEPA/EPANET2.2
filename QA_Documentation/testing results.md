# Testing Results 
EPANET 2.2.0 (tested via https://github.com/USEPA/epanet-solver, which is cloned from <https://github.com/OpenWaterAnalytics/EPANET/releases/tag/v2.2>)

EPANET 2.2.0 Final Release is from <https://github.com/OpenWaterAnalytics/EPANET/releases/tag/v2.2> which is archived at:  <https://github.com/USEPA/EPANET2.2>

# Introduction

EPANET is a tool for the simulation of hydraulics and water quality in drinking water distribution systems. EPANET is widely used to plan, design, analyze, and regulate WDNs. It is also an important component of water security tools and has been adopted by third party commercial vendors as the computational core of their derivative products. It is of the utmost importance that EPANET is well engineered software that is fully capable of supporting these important stakeholder activities.

At nearly 30 years old, EPANET is a mature application. Historically, EPANET has been used by engineers to perform fire flow calculations for new subdivisions, design master plans for asset management, and gain insights into their systems operation. Today, EPANET is evolving from an offline planning tool to a continuous online operational tool. This evolution demands a higher degree or reliability going forward. Despite all the modern advances in computing we enjoy, however, software development is still an expensive and error prone activity. Guaranteeing 100% software reliability is simply not possible. Quality assurance is an important aspect of software project management and can be used to target an appropriate level of reliability considering its intended use.

EPA has invested heavily in the development of EPANET software and makes large investments in various research software prototypes to deliver value to ORD and its customers. The quality of EPANET software has always been adequate for its intended purpose. In the sections that follow, the results of testing performed on EPANET version 2.2.0 is reported.

# Quality Management

The EPANET Project Team practices Adaptive Software Quality Management. The central idea is that project management actions are adaptive and based in part on the current state of software quality and the desired project objectives. Testing is used as a metric for software quality and thus feeds into project management decisions. Testing is integral to preliminary algorithmic development, software development and maintenance, and release management activities within our quality management plan.

The highest level of efficacy can be achieved when testing is integrated early within the software lifecycle. For example, by emphasizing testing in preliminary development, deficiencies to be identified and corrected early when they are easiest to identify and the least expensive to fix. As a software application matures, new features are developed by adding to or modifying the existing source code base. During this phase of the lifecycle testing can be used to detect and prevent regressions in existing application functionality during feature development and software maintenance activities. Finally, when an application is ready for release to external users, testing can be used to certify that it meets requirements and is indeed ready for use.

## Software Under Test

App: epanet-solver v2.2.0
Platform: win32,
SUT Build ID: local,
Version: 80fe19e;

# Component Testing

As stated previously, the main objectives of our current testing activities are to insure correct operation of algorithms under development, flag regressions during feature development and maintenance activities, and when delivering software to certify that it is ready for release.

The EPANET project contains several modular components. The most significant of these are the hydraulic and water quality solvers. Going forward we will refer to these components as the EPANET solvers or solvers for short. The solvers have been written as a software library to facilitate use by other applications. For example, the solver library is used by the US EPA's EPANET UI application.  It is also used by other ORD research projects such as WNTR, WIST, and TEVA SPOT. And by third party software vendors in their derivative software products.

The solver library's API has been expanded significantly in the latest release -- going from 50 and 120 functions respectively and consists of 5400 lines of code. The API allows developers to access the solvers' data model, build models programmatically, control hydraulic and water quality solver execution, modify model element parameters, access simulation results, and much more. With complex functionality spread over hundreds of functions and thousands of lines of code testing is critical. Component testing has been employed to ensure that each API function call operates correctly in isolation and under realistic usage scenarios. A testing framework has been developed to write, build, and execute our component tests. Scripts have been written to make it easy to build and execute component tests.

## Component Testing Results

Test Artifact: ctest-out.txt

7/7 Tests – PASS

# Regression Testing

The main purpose of the solvers is to perform hydraulic and water quality simulations. At a basic level it uses numerical methods to solve systems of partial differential equations that describe the physics of water movement (hydraulics) and basic water chemistry (water quality) in pipe networks. In this respect, it is a prototypical example of a scientific and engineering software application. From a software quality assurance perspective, these types of applications are particularly challenging to develop and maintain. These applications by their nature are abstract and highly complex making the source code difficult to understand. The applications must work for each user&#39;s unique Water Distribution Network (WDN) model but by necessity is written to solve the general case. The source code for numerical methods can be highly sensitive to seemingly insignificant changes.

To address some of these challenges we employ regression testing to run a suite of WDN (i.e., network models in the form of EPANET .inp files) examples and scrutinize the results. The suite currently contains approximately 70 examples. These examples taken together exercise a broad set of application features. The simulation output for each example is compared to a known &quot;good&quot; result called a benchmark. If any change in results gets detected, the developer is alerted and should determine if it is nominal or indicative of a potential defect in the software. Custom components were written for an Open Source regression testing framework, command scripts, and SOPs to make running and maintaining the tests, and managing benchmarks as straight forward as possible.

## Regression Testing Results

Test Artifact: receipt-win32.json

70/70 Tests - PASS

# Release Testing

Preparing a software release involves finalizing changes made to the source code, fixing defects, testing, documentation, and installation packaging. The purpose of release testing is to certify that the software is ready for production. We use a release test suite of approximately 270 EPANET examples that have been contributed by users over the history of the project. The regression test framework described previously is used to run the release suite and monitor results for failures, crashes, and other anomalous behavior. Because the release suite is separate from the regression suite and has been contributed by users, running it indicates how the software will perform in production. From our release testing results we estimate that the EPANET v2.2.0 solver can successfully run 99% of user's networks.

## Release Testing Results

Test Artifact: manifest.json

269/272 Tests - PASS
3/272 Tests - FAIL (Error 110: cannot solve network hydraulic equations)

# Recommendation

Again, EPANET 2.2.0 (<https://github.com/OpenWaterAnalytics/EPANET/releases/tag/v2.2>) was tested via its clone at Epanet-solver v2.2.0 and found to be acceptable.  EPANET 2.2.0 meets all requirements for production use.

EPA Project Team will monitor feedback from users closely and address any failures identified. EPA will continue to work on improving solver solution stability as more information is gathered from users.