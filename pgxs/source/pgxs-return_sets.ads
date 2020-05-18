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
--  Support to return sets (multiple rows)

with PGXS.Types;

package PGXS.Return_Sets is

   function Return_Next_Value
     (Args    : PGXS.Function_Call_Info;
      Context : PGXS.Func_Call_Context;
      Value   : PGXS.Datum) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_SRF_RETURN_NEXT";
   --  Return given value and continue processing

   function Return_Next_Null
     (Args : PGXS.Function_Call_Info;
      Context : PGXS.Func_Call_Context) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_SRF_RETURN_NEXT_NULL";
   --  Return null value and continue processing

   function Return_Done
     (Args : PGXS.Function_Call_Info;
      Context : PGXS.Func_Call_Context) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_SRF_RETURN_DONE";
   --  Return without any value and end processing

   function Get_Call_Counter
     (Context : PGXS.Func_Call_Context) return PGXS.Types.UInt_64
     with Import,
          Convention => C,
          Link_Name => "__ada_PG_FuncCallContext_call_cntr";
   --  Call counter of the function in current context

   function Get_Tuple_Descriptor
     (Context : PGXS.Func_Call_Context) return PGXS.Tuple_Desc
     with Import,
          Convention => C,
          Link_Name => "__ada_PG_FuncCallContext_get_tuple_desc";

   procedure Set_Tuple_Descriptor
     (Context : PGXS.Func_Call_Context;
      To      : PGXS.Tuple_Desc)
     with Import,
          Convention => C,
          Link_Name => "__ada_PG_FuncCallContext_set_tuple_desc";

   generic
      type User_Data (<>) is limited private;
      type User_Data_Access is access all User_Data;

      with procedure First_Initialize
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : out User_Data_Access);
      --  Do first time initialization. User data must be allocated with
      --  memory pool defined in PGXS.Pools.

      with procedure Step_Initialize
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : User_Data_Access) is null;
      --  Do each step initialization.

      with function Step
        (Args    : PGXS.Function_Call_Info;
         Context : PGXS.Func_Call_Context;
         Data    : User_Data_Access) return PGXS.Datum;
      --  Return result of current iteration. Implementation should use
      --  Return_Nex_Value or Return_Next_Null function to construct next
      --  return value or Return_Done to end processing.

   function Generic_Set_Return_Function
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Convention => C;

end PGXS.Return_Sets;
