------------------------------------------------------------------------------
--               PostgreSQL server extension modules binding                --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with PGXS.Types;

package body PGXS.Composites is

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Descriptor : PGXS.Tuple_Desc;
      Size       : Attribute_Count) return Attributes is
   begin
      return
        new Attributes_Arrays'
              (Size => Size, Descriptor => Descriptor, others => <>);
   end Allocate;

   ----------------------------------
   -- Relation_Name_Get_Tuple_Desc --
   ----------------------------------

   function Relation_Name_Get_Tuple_Desc
     (Relname : String) return PGXS.Tuple_Desc
   is
      function Imported
        (Relname : Interfaces.C.char_array) return PGXS.Tuple_Desc
        with Import, Convention => C, Link_Name => "RelationNameGetTupleDesc";

   begin
      return Imported (Interfaces.C.To_C (Relname));
   end Relation_Name_Get_Tuple_Desc;

   ------------------
   -- Return_Value --
   ------------------

   function Return_Value
     (Args : Function_Call_Info; Item : Attributes) return PGXS.Datum
   is

      function Heap_From_Tuple
        (Tupdesc : PGXS.Tuple_Desc;
         Values  : not null access constant PGXS.Datum;
         Isnull  : not null access constant PGXS.Types.Bool)
         return PGXS.Heap_Tuple
        with Import, Convention => C, Link_Name => "heap_form_tuple";

      function To_Datum (Item : PGXS.Heap_Tuple) return PGXS.Datum
        with Import,
             Convention => C,
             Link_Name => "__ada_PG_HeapTupleGetDatum";

   begin
      return
        To_Datum
          (Heap_From_Tuple
             (Item.Descriptor,
              Item.Datums (Item.Datums'First)'Access,
              Item.Nulls (Item.Nulls'First)'Access));
   end Return_Value;

   --------------
   -- Set_Null --
   --------------

   procedure Set_Null
     (Self  : in out Attributes;
      Index : Attribute_Number) is
   begin
      Self.Nulls (Index) := Interfaces.C.True;
   end Set_Null;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Self  : in out Attributes;
      Index : Attribute_Number;
      To    : PGXS.Datum) is
   begin
      Self.Nulls (Index) := Interfaces.C.False;
      Self.Datums (Index) := To;
   end Set_Value;

end PGXS.Composites;
