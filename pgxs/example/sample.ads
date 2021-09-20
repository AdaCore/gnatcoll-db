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

with PGXS;
with PGXS.ABI;
with PGXS.Generic_Bytea;

package Sample is

   type Information is record
      X : Integer;
      Y : Integer;
   end record;

   package Information_Bytea is new PGXS.Generic_Bytea (Information);

   function To_Bytea (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name  => "to_bytea";

   function To_Bytea_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name  => "pg_finfo_to_bytea";

   function Get_X (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name  => "get_x";

   function Get_X_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name  => "pg_finfo_get_x";

   function Get_Y (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name  => "get_y";

   function Get_Y_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name  => "pg_finfo_get_y";

end Sample;
