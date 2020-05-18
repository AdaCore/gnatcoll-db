PostgreSQL Server Extensions Modules in Ada
-------------------------------------------

This directory contains the binding to develop extension modules for PostgreSQL
server. They are loaded by the database backend and provide functions that are
called from SQL queries.

Installation
============

To use this binding, the user should copy all code in the source directory
inside source code of his own project.

Binding structure
=================

PGXS                - declaration of internal types
PGXS.ABI            - the function to declare the version of calling convention of extension functions
PGXS.Call_Info      - subprograms to get values of arguments and set the return value
PGXS.Composites     - subprograms to manipulate by composite types
PGXS.Datums         - subprograms to get value of Datum and create Datum from value
PGXS.Generic_Bytea  - generic package to manipulate with BYTEA values as Ada record
PGXS.Logs           - error reporting and logging
PGXS.Pools          - memory management
PGXS.Pools.Defaults - default memory pool
PGXS.Return_Sets    - subprograms and utilities to return sets
PGXS.Types          - declarations of types used as representation of SQL types
PGXS.Varlen         - subprograms to manipulate by varlena objects

Declaring exported functions
============================

Each exported extension function should have following declaration:

    function My_Function
      (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
       with Export, Convention => C, Link_Name => "my_function";

It should have a companion function to declare calling conventions:

    function My_Function_Info
      return not null access PGXS.ABI.Function_Info_Record
        is (PGXS.ABI.Ada_Function_Info_1)
      with Export, Convention => C, Link_Name => "pg_finfo_my_function";

Note that calling convention declaration function should have its link name
constructed as "pg_finfo_" prefix and link name of the user function.

Building the module
===================

The module should be built as a standalone shared library. Here is an example
project file with only the important attributes and packages, others may need
to be provided.

    library project My_Module is

       PostgreSQL_IncludeDir_Server := "/usr/include/pgsql/server";
       --  A Path to server extensions header files reported by
       --
       --  $ pg_config --includedir-server

       for Languages use ("C", "Ada");
       for Object_Dir use "obj";
       for Source_Dirs use ("source/pgxs", "source/my_module");

       for Library_Name use "my_module";
       for Library_Kind use "relocatable";
       for Library_Dir use "lib";
       for Library_Options use ("-Wl,--version-script=source/pgxs.sym");
       --  Linker script to be used when linking extension module
       for Library_Interface use ("My_Module", "PGXS", ...);

       package Compiler is
          for Switches ("Ada") use ("-fPIC");
          for Switches ("C") use
            ("-fPIC", "-I" & PostgreSQL_IncludeDir_Server);
       end Compiler;

    end My_Module;
