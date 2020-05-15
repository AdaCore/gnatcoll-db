------------------------------------------------------------------------------
--               PostgreSQL server extension modules binding                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  Type declarations for data types of PostgreSQL server extension modules
--  API not directly related to SQL data types.

private with System;

package PGXS is

   pragma Preelaborate;

   type Function_Call_Info is limited private;
   --  Function call information: arguments and return value.

   type Datum is private;
   --  Generic container of the value for some SQL data type.

   type Heap_Tuple_Header is private;
   --  Container of the composite object

   type Tuple_Desc is private;

private

   type Function_Call_Info is limited null record
     with Convention => C;

   type Datum is new System.Address;

   type Heap_Tuple_Header_Data is limited null record
     with Convention => C;

   type Heap_Tuple_Header is access all Heap_Tuple_Header_Data;

   type Struct_Tuple_Desc is limited null record
     with Convention => C;

   type Tuple_Desc is access all Struct_Tuple_Desc;

end PGXS;
