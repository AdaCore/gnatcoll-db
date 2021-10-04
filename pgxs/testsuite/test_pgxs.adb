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

with Interfaces.C;

with PGXS.Call_Info;
with PGXS.Composites;
with PGXS.Datums;
with PGXS.Pools.Defaults;
with PGXS.Return_Sets;
with PGXS.Logs;

package body Test_PGXS is

   use type Interfaces.C.char_array;
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

   ------------------
   -- Pos_From_Bin --
   ------------------

   function Pos_From_Bin
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      Value        : constant Pos := Pos_Bytea.Get_Arg (Args, 0);

      Result_Oid   : PGXS.Types.Oid;
      Result_Desc  : PGXS.Tuple_Desc;
      Result_Class : PGXS.Call_Info.Func_Type_Class;

      Coord_Desc   : PGXS.Tuple_Desc;
      Result_Coord : PGXS.Composites.Attributes;
      Result       : PGXS.Composites.Attributes;

   begin
      --  Obtain Tuple_Desc for Coord type and return types.

      Result_Class :=
        PGXS.Call_Info.Get_Call_Result_Type (Args, Result_Oid, Result_Desc);

      Coord_Desc :=
        PGXS.Composites.Relation_Name_Get_Tuple_Desc
          ("coord" & Interfaces.C.nul);
      Result_Coord :=
        PGXS.Composites.Allocate
          (PGXS.Composites.Bless_Tuple_Desc (Coord_Desc), 2);
      Result :=
        PGXS.Composites.Allocate
          (PGXS.Composites.Bless_Tuple_Desc (Result_Desc), 2);

      --  Construct return value

      PGXS.Composites.Set_Value
        (Result_Coord, 1, PGXS.Datums.To_Datum (Value.C.X));
      PGXS.Composites.Set_Value
        (Result_Coord, 2, PGXS.Datums.To_Datum (Value.C.Y));

      PGXS.Composites.Set_Value
        (Result, 1, PGXS.Composites.Return_Value (Args, Result_Coord));
      PGXS.Composites.Set_Value
        (Result, 2, PGXS.Datums.To_Datum (Value.H));

      return PGXS.Composites.Return_Value (Args, Result);
   end Pos_From_Bin;

   ---------------
   -- Composite --
   ---------------

   function Composite
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      use type Interfaces.C.char_array;
      use all type PGXS.Call_Info.Func_Type_Class;

      X : PGXS.Types.Int_32 := PGXS.Call_Info.Get_Arg (Args, 0);
      Y : PGXS.Types.Int_32 := PGXS.Call_Info.Get_Arg (Args, 1);
      T : PGXS.Types.Oid;
      D : PGXS.Tuple_Desc;
      C : PGXS.Call_Info.Func_Type_Class;

      A : PGXS.Composites.Attributes;
      --    PGXS.Composites.Allocate
      --      (PGXS.Composites.Bless_Tuple_Desc

   begin
      C := PGXS.Call_Info.Get_Call_Result_Type (Args, T, D);

      if C /= Typefunc_Composite then
         PGXS.Logs.Report
           (PGXS.Logs.Error,
            "function returning record called in context that cannot accept"
            & " type record" & Interfaces.C.nul);
      end if;

      A := PGXS.Composites.Allocate (PGXS.Composites.Bless_Tuple_Desc (D), 2);
      PGXS.Composites.Set_Value (A, 1, PGXS.Datums.To_Datum (X));
      PGXS.Composites.Set_Value (A, 2, PGXS.Datums.To_Datum (Y));

      return PGXS.Composites.Return_Value (Args, A);
   end Composite;

   -----------------
   -- Simple_Sets --
   -----------------

   package Simple_Sets is

      type Simple_Set_Context is record
         Amount  : PGXS.Types.Int_32;
         Counter : PGXS.Types.Int_32 := 0;
      end record;

      type Simple_Set_Context_Access is access all Simple_Set_Context
        with Storage_Pool => PGXS.Pools.Defaults.Default_Pool;

      procedure First_Initialize
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : out Simple_Set_Context_Access);

      function Step
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : Simple_Set_Context_Access) return PGXS.Datum;

      function Set_Simple is
        new PGXS.Return_Sets.Generic_Set_Return_Function
          (User_Data        => Simple_Set_Context,
           User_Data_Access => Simple_Set_Context_Access,
           First_Initialize => First_Initialize,
           Step             => Step);

   end Simple_Sets;

   -----------------
   -- Simple_Sets --
   -----------------

   package body Simple_Sets is

      ----------------------
      -- First_Initialize --
      ----------------------

      procedure First_Initialize
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : out Simple_Set_Context_Access) is
      begin
         Data := new Simple_Set_Context;
         Data.Amount := PGXS.Call_Info.Get_Arg (Args, 0);
      end First_Initialize;

      ----------
      -- Step --
      ----------

      function Step
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : Simple_Set_Context_Access) return PGXS.Datum
      is
         use type PGXS.Types.Int_32;

      begin
         if Data.Counter < Data.Amount then
            Data.Counter := Data.Counter + 1;

            return
              PGXS.Return_Sets.Return_Next_Value
                (Args, Context, PGXS.Datums.To_Datum (Data.Counter));

         else
            return PGXS.Return_Sets.Return_Done (Args, Context);
         end if;
      end Step;

   end Simple_Sets;

   function Set_Simple
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
      renames Simple_Sets.Set_Simple;

end Test_PGXS;
