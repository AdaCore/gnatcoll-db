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

with PGXS.ABI;

package body Sample is

   -----------
   -- Get_X --
   -----------

   function Get_X (Args : in out PGXS.Function_Call_Info) return PGXS.Datum is
      V : constant Information := Information_Bytea.Get_Arg (Args, 0);

   begin
      return PGXS.Call_Info.Return_Value (Args, PGXS.Types.Int_32 (V.X));
   end Get_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Args : in out PGXS.Function_Call_Info) return PGXS.Datum is
      V : constant Information := Information_Bytea.Get_Arg (Args, 0);

   begin
      return PGXS.Call_Info.Return_Value (Args, PGXS.Types.Int_32 (V.Y));
   end Get_Y;

   --------------
   -- To_Bytea --
   --------------

   function To_Bytea
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
   is
      X : constant PGXS.Types.Int_32 := PGXS.Call_Info.Get_Arg (Args, 0);
      Y : constant PGXS.Types.Int_32 := PGXS.Call_Info.Get_Arg (Args, 1);

   begin
      return
        Information_Bytea.Return_Value
          (Args,
           (X => Integer (X),
            Y => Integer (Y)));

   exception
      when others =>
         return PGXS.Call_Info.Return_Null (Args);
   end To_Bytea;

end Sample;
