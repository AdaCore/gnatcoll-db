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
--  Subprograms to obtain and to construct values of composite types.

with Interfaces.C;

with PGXS.Types;

package PGXS.Composites is

   type Attribute_Count is
     new PGXS.Types.Int_16 range 0 .. PGXS.Types.Int_16'Last;

   subtype Attribute_Number is Attribute_Count range 1 .. Attribute_Count'Last;

   function Get_Attribute_By_Name
     (Item    : PGXS.Heap_Tuple_Header;
      Name    : Interfaces.C.char_array;
      Is_Null : out PGXS.Types.Bool) return PGXS.Datum
     with Import, Convention => C, Link_Name => "GetAttributeByName";
   --  Return value of the attribute by name. Note, C-style nul terminated
   --  string should be used as Name.

   function Get_Attribute_By_Number
     (Item    : PGXS.Heap_Tuple_Header;
      Number  : Attribute_Number;
      Is_Null : out PGXS.Types.Bool) return PGXS.Datum
     with Import, Convention => C, Link_Name => "GetAttributeByNum";
   --  Return value of the attribute by number. Attribute numbers start at 1.

end PGXS.Composites;
