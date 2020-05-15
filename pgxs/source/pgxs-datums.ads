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
--  Functions to obtain value from the Datum and to convert value to the Datum.

with PGXS.Types;

package PGXS.Datums is

   --  Bool

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Bool
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetBool";
   --  Returns boolean value of a datum.

   function To_Datum (Item : PGXS.Types.Bool) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_BoolGetDatum";
   --  Returns datum representation for a boolean.

   --  Int_16

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Int_16
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetInt16";
   --  Returns 16-bit integer value of a datum

   function To_Datum (Item : PGXS.Types.Int_16) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_Int16GetDatum";
   --  Returns datum representation for a 16-bit integer

   --  UInt_16

   function To_Value (Item : PGXS.Datum) return PGXS.Types.UInt_16
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetUInt16";
   --  Returns 16-bit unsigned integer value of a datum

   function To_Datum (Item : PGXS.Types.UInt_16) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_UInt16GetDatum";
   --  Returns datum representation for a 16-bit unsigned integer

   --  Int_32

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Int_32
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetInt32";
   --  Returns 32-bit integer value of a datum

   function To_Datum (Item : PGXS.Types.Int_32) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_Int32GetDatum";
   --  Returns datum representation for a 32-bit integer

   --  UInt_32

   function To_Value (Item : PGXS.Datum) return PGXS.Types.UInt_32
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetUInt32";
   --  Returns 32-bit unsigned integer value of a datum

   function To_Datum (Item : PGXS.Types.UInt_32) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_UInt32GetDatum";
   --  Returns datum representation for a 32-bit unsigned integer

   --  Int_64

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Int_64
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetInt64";
   --  Returns 64-bit integer value of a datum

   function To_Datum (Item : PGXS.Types.Int_64) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_Int64GetDatum";
   --  Returns datum representation for a 64-bit integer

   --  Float_4

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Float_4
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetFloat4";
   --  Returns 4-byte floating point value of a datum

   function To_Datum (Item : PGXS.Types.Float_4) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_Float4GetDatum";
   --  Returns datum representation for a 4-byte floating point number

   --  Float_8

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Float_8
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetFloat8";
   --  Returns 8-byte floating point value of a datum

   function To_Datum (Item : PGXS.Types.Float_8) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_Float8GetDatum";
   --  Returns datum representation for a 8-byte floating point number

   --  Byte_A

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Byte_A
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetByteaP";
   --  Returns bytea value of a datum

   function To_Datum (Item : PGXS.Types.Byte_A) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_ByteaPGetDatum";
   --  Returns datum representation for a bytea

   --  Text

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Text
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetTextP";
   --  Returns text value of a datum

   function To_Datum (Item : PGXS.Types.Text) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_TextPGetDatum";
   --  Returns datum representation for a text

   --  Bp_Char

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Bp_Char
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetBpCharP";
   --  Returns text value of a datum

   function To_Datum (Item : PGXS.Types.Bp_Char) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_BpCharPGetDatum";
   --  Returns datum representation for a text

   --  Var_Char

   function To_Value (Item : PGXS.Datum) return PGXS.Types.Var_Char
     with Import, Convention => C, Link_Name => "__ada_PG_DatumGetVarCharP";
   --  Returns text value of a datum

   function To_Datum (Item : PGXS.Types.Var_Char) return PGXS.Datum
     with Import, Convention => C, Link_Name => "__ada_PG_VarCharPGetDatum";
   --  Returns datum representation for a text

end PGXS.Datums;
