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

with PGXS.Call_Info;
with PGXS.Datums;

package body PGXS.Generic_Bytea is

   -------------
   -- Get_Arg --
   -------------

   function Get_Arg
     (Args : Function_Call_Info; Index : PGXS.Types.Int_32) return Data
   is
      Aux    : constant PGXS.Types.Byte_A :=
        PGXS.Call_Info.Get_Arg (Args, Index);
      Result : Data
        with Import, Address => PGXS.Types.Data (Aux);

   begin
      return Result;
   end Get_Arg;

   ------------------
   -- Return_Value --
   ------------------

   function Return_Value
     (Args : Function_Call_Info; Item : Data) return PGXS.Datum
   is
      pragma Unreferenced (Args);

      Result  : PGXS.Types.Byte_A :=
        PGXS.Types.Allocate (Data'Max_Size_In_Storage_Elements);
      Storage : Data
        with Import, Address => PGXS.Types.Data (Result);

   begin
      Storage := Item;

      return PGXS.Datums.To_Datum (Result);
   end Return_Value;

end PGXS.Generic_Bytea;
