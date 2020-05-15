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
--  Base subprograms to manipulate variable length data

with System.Storage_Elements;

package PGXS.Varlen is

   type Varlen_A is private;

   function Allocate
     (Size : System.Storage_Elements.Storage_Count) return Varlen_A;
   --  Allocates new object of given size

   function Size
     (Item : Varlen_A) return System.Storage_Elements.Storage_Count;
   --  Returns size of the object

   function Data (Item : Varlen_A) return System.Address
     with Import, Convention => C, Link_Name => "__ada_VARDATA_ANY";
   --  Returns pointer of the first storage element of the data

private

   type Varlen_Record is null record
     with Convention => C;

   type Varlen_A is access all Varlen_Record;

end PGXS.Varlen;
