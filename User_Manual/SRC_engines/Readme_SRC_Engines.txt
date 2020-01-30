This folder archives the EPA/OWA and community collaboration source code files for the EPANET hydraulic and water quality engines for v2.2.0.

It contains the source code for the EPANET 2.2 network hydraulic and water quality solver. 
It is written in ANSI-compatible C and can be compiled into either a Windows Dynamic Link Library of functions
or into a command-line executable.

The DLL version of the solver (epanet2.dll) is used with
the EPANET 2 user interface executable (epanet2w.exe) to
form a complete Windows modeling package. It also serves
as the function library for the EPANET Programmer's Toolkit,
allowing developers to construct their own customized pipe
network analysis applications.

All the files except main.c are needed to build the DLL version of the solver.  
The file main.c needs to be incudled for building a command-line executable. 
Epanet2.def should be inculded as a module definition file
for building the DLL that can be used in EPANET user inferface (epanet2w.exe).  

