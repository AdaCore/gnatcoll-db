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

with System.Address_To_Access_Conversions;

package body PGXS.Return_Sets is

   ---------------------------------
   -- Generic_Set_Return_Function --
   ---------------------------------

   function Generic_Set_Return_Function
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is

      use type System.Address;

      package Conversions is
        new System.Address_To_Access_Conversions (User_Data);

      function Is_First_Call
        (Args : PGXS.Function_Call_Info) return PGXS.Types.Bool
        with Import, Convention => C, Link_Name => "__ada_SRF_IS_FIRSTCALL";

      function First_Call_Init
        (Args : PGXS.Function_Call_Info) return PGXS.Func_Call_Context
        with Import, Convention => C, Link_Name => "__ada_SRF_FIRSTCALL_INIT";

      function Per_Call_Setup
        (Args : PGXS.Function_Call_Info) return PGXS.Func_Call_Context
        with Import, Convention => C, Link_Name => "__ada_SRF_PERCALL_SETUP";

      function Get_Memory_Context
        (Context : PGXS.Func_Call_Context) return PGXS.Memory_Context
        with Import,
             Convention => C,
             Link_Name => "__ada_PG_FuncCallContext_get_multi_call_memory_ctx";

      function Get_User_Data
        (Context : PGXS.Func_Call_Context) return System.Address
        with Import,
             Convention => C,
             Link_Name => "__ada_PG_FuncCallContext_get_user_fctx";

      procedure Set_User_Data
        (Context : PGXS.Func_Call_Context;
         To      : System.Address)
        with Import,
             Convention => C,
             Link_Name => "__ada_PG_FuncCallContext_set_user_fctx";

      function Memory_Context_Switch_To
        (Context : PGXS.Memory_Context) return PGXS.Memory_Context
        with Import,
             Convention => C,
             Link_Name => "__ada_MemoryContextSwitchTo";

      procedure Memory_Context_Switch_To (Context : PGXS.Memory_Context)
        with Import,
             Convention => C,
             Link_Name => "__ada_MemoryContextSwitchTo";

      Func_Context : PGXS.Func_Call_Context;
      Data         : User_Data_Access;
      Old          : PGXS.Memory_Context;

   begin
      if Is_First_Call (Args) then
         Func_Context := First_Call_Init (Args);
         Old := Memory_Context_Switch_To (Get_Memory_Context (Func_Context));
         First_Initialize (Args, Func_Context, Data);
         Set_User_Data
           (Func_Context,
            Conversions.To_Address (Conversions.Object_Pointer (Data)));
         Memory_Context_Switch_To (Old);
      end if;

      Func_Context := Per_Call_Setup (Args);

      if Get_User_Data (Func_Context) /= System.Null_Address then
         Data :=
           Conversions.To_Pointer
             (Get_User_Data (Func_Context)).all'Unchecked_Access;

      else
         Data := null;
      end if;

      Step_Initialize (Args, Func_Context, Data);

      return Step (Args, Func_Context, Data);
   end Generic_Set_Return_Function;

end PGXS.Return_Sets;
