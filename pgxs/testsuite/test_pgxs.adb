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

with Interfaces.C;

with PGXS.Call_Info;
with PGXS.Composites;
with PGXS.Datums;
with PGXS.Types;

package body Test_PGXS is

   use all type PGXS.Types.Bool;

   --------------
   -- Num_Args --
   --------------

   function Num_Args
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum is
   begin
      return PGXS.Call_Info.Return_Value
        (Args, PGXS.Call_Info.Arguments (Args));
   end Num_Args;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum is
   begin
      return
        PGXS.Call_Info.Return_Value
          (Args, PGXS.Call_Info.Get_Arg_Is_Null (Args, 0));
   end Is_Null;

   -------------
   -- Inverse --
   -------------

   function Inverse
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum is
   begin
      return
        PGXS.Call_Info.Return_Value
          (Args, not PGXS.Call_Info.Get_Arg (Args, 0));
   end Inverse;

   -------------------
   -- Add_One_Int16 --
   -------------------

   function Add_One_Int16
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      use type PGXS.Types.Int_16;

   begin
      return
        PGXS.Call_Info.Return_Value
          (Args, PGXS.Call_Info.Get_Arg (Args, 0) + 1);
   end Add_One_Int16;

   -------------------
   -- Add_One_Int32 --
   -------------------

   function Add_One_Int32
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      use type PGXS.Types.Int_32;

   begin
      return
        PGXS.Call_Info.Return_Value
          (Args, PGXS.Types.Int_32'(PGXS.Call_Info.Get_Arg (Args, 0)) + 1);
   end Add_One_Int32;

   --------------------
   -- Add_One_Float4 --
   --------------------

   function Add_One_Float4
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      use type PGXS.Types.Float_4;

   begin
      return
        PGXS.Call_Info.Return_Value
          (Args, PGXS.Call_Info.Get_Arg (Args, 0) + 1.0);
          --  (Args, PGXS.Types.Int_32'(PGXS.Call_Info.Get_Arg (Args, 0)) + 1);
   end Add_One_Float4;

   --------------------
   -- Add_One_Float8 --
   --------------------

   function Add_One_Float8
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      use type PGXS.Types.Float_8;

   begin
      return
        PGXS.Call_Info.Return_Value
          (Args, PGXS.Types.Float_8'(PGXS.Call_Info.Get_Arg (Args, 0)) + 1.0);
   end Add_One_Float8;

   --------------
   -- Overpaid --
   --------------

   function Overpaid
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      use type Interfaces.C.char_array;
      use type PGXS.Types.Int_32;

      Emp     : PGXS.Heap_Tuple_Header := PGXS.Call_Info.Get_Arg (Args, 0);
      Limit   : PGXS.Types.Int_32 := PGXS.Call_Info.Get_Arg (Args, 1);
      Is_Null : PGXS.Types.Bool;
      Salary  : PGXS.Datum;

   begin
      Salary :=
        PGXS.Composites.Get_Attribute_By_Name
          (Emp, "salary" & Interfaces.C.nul, Is_Null);

      if Is_Null then
         return PGXS.Call_Info.Return_Value (Args, False);

      else
         return
           PGXS.Call_Info.Return_Value
             (Args, PGXS.Types.Bool (PGXS.Datums.To_Value (Salary) > Limit));
      end if;
   end Overpaid;

end Test_PGXS;
