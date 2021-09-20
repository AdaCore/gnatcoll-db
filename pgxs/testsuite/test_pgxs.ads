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

with PGXS.ABI;
with PGXS.Generic_Bytea;
with PGXS.Types;

package Test_PGXS is

   function Num_Args (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_num_args";
   function Num_Args_Info return not null access PGXS.ABI.Function_Info_Record
     is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_num_args";

   function Is_Null (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_arg_is_null";
   function Is_Null_Info return not null access PGXS.ABI.Function_Info_Record
     is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_arg_is_null";

   function Inverse (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_inverse_bool";
   function Inverse_Info return not null access PGXS.ABI.Function_Info_Record
     is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_inverse_bool";

   function Add_One_Int16
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_add_one_int16";
   function Add_One_Int16_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_add_one_int16";

   function Add_One_Int32
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_add_one_int32";
   function Add_One_Int32_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_add_one_int32";

   function Add_One_Float4
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_add_one_float4";
   function Add_One_Float4_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export,
          Convention => C,
          Link_Name => "pg_finfo_apgxs_add_one_float4";

   function Add_One_Float8
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_add_one_float8";
   function Add_One_Float8_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export,
          Convention => C,
          Link_Name => "pg_finfo_apgxs_add_one_float8";

   function Overpaid
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_overpaid";
   function Overpaid_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_overpaid";

   function Composite
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_composite";
   function Composite_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_composite";

   function Set_Simple
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
     with Export, Convention => C, Link_Name => "apgxs_set_simple";
   function Set_Simple_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name => "pg_finfo_apgxs_set_simple";

   --  U917-009

   type Coord is record
      X, Y : PGXS.Types.Float_4;
   end record;

   type Pos is record
      C : Coord;
      H : PGXS.Types.Int_32;
   end record;

   package Pos_Bytea is new PGXS.Generic_Bytea (Pos);

   function Pos_From_Bin
     (Args : in out PGXS.Function_Call_Info) return PGXS.Datum
         with Export, Convention => C, Link_Name => "pos_from_bin";

   function Pos_From_Bin_Info
     return not null access PGXS.ABI.Function_Info_Record
   is (PGXS.ABI.Ada_Function_Info_1)
     with Export, Convention => C, Link_Name  => "pg_finfo_pos_from_bin";

end Test_PGXS;
