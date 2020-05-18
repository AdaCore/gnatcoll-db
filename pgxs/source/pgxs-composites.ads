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

with Interfaces.C.Extensions;

private with PGXS.Pools;
with PGXS.Types;

package PGXS.Composites is

   type Attribute_Count is
     new PGXS.Types.Int_16 range 0 .. PGXS.Types.Int_16'Last;

   subtype Attribute_Number is Attribute_Count range 1 .. Attribute_Count'Last;

   type Attributes is private;

   function Allocate
     (Descriptor : PGXS.Tuple_Desc;
      Size       : Attribute_Count) return Attributes;
   --  Allocate memory to store given number of values.

   procedure Set_Value
     (Self  : in out Attributes;
      Index : Attribute_Number;
      To    : PGXS.Datum);
   --  Set value of the attribute at givent index to given value.

   procedure Set_Null
     (Self  : in out Attributes;
      Index : Attribute_Number);
   --  Set value of the attribute at given index to null.

   function Return_Value
     (Args : Function_Call_Info; Item : Attributes) return PGXS.Datum;
   --  Creates internal representation of the given set of attributes and
   --  converts them to be returned from the user defined function.

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

   function Bless_Tuple_Desc (Item : PGXS.Tuple_Desc) return PGXS.Tuple_Desc
     with Import, Convention => C, Link_Name => "BlessTupleDesc";
   --  Complete tuple descriptor by initially missing information to return
   --  values from the user defined extension function.

private

   type Datum_Array is array (Attribute_Number range <>) of aliased PGXS.Datum;

   type Bool_Array is
     array (Attribute_Number range <>) of aliased PGXS.Types.Bool;

   type Attributes_Arrays (Size : Attribute_Count) is record
      Descriptor : PGXS.Tuple_Desc;
      Datums     : Datum_Array (1 .. Size);
      Nulls      : Bool_Array (1 .. Size) :=
        (others => Interfaces.C.Extensions.True);
   end record;

   Default_Pool : PGXS.Pools.Memory_Context_Pool;

   type Attributes is access all Attributes_Arrays
     with Storage_Pool => Default_Pool;

end PGXS.Composites;
