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
--  Mapping of SQL types to C and Ada
--
--  oid                        Oid       postgres.h  Oid
--  boolean                    bool      postgres.h  Bool
--  smallint (int2)            int16     postgres.h  Int_16
--  integer (int4)             int32     postgres.h  Int_32
--  real (float4)              float4*   postgres.h  Float_4
--  double precision (float8)  float8*   postgres.h  Float_8
--  bytea                      bytea*    postgres.h  Byte_A
--  character                  BpChar*   postgres.h  Bp_Char
--  text                       text*     postgres.h  Text
--  varchar                    VarChar*  postgres.h  Var_Char

with Interfaces.C;

with PGXS.Varlen;

package PGXS.Types is

   type Oid is private;

   subtype Bool is Interfaces.C.C_bool;

   subtype Int_16 is Interfaces.Integer_16;

   subtype UInt_16 is Interfaces.Unsigned_16;

   subtype Int_32 is Interfaces.Integer_32;

   subtype UInt_32 is Interfaces.Unsigned_32;

   subtype Int_64 is Interfaces.Integer_64;

   subtype UInt_64 is Interfaces.Unsigned_64;

   subtype Float_4 is Interfaces.IEEE_Float_32;

   subtype Float_8 is Interfaces.IEEE_Float_64;

   type Byte_A is new PGXS.Varlen.Varlen_A;

   type Bp_Char is new PGXS.Varlen.Varlen_A;

   type Text is new PGXS.Varlen.Varlen_A;

   type Var_Char is new PGXS.Varlen.Varlen_A;

private

   type Oid is new Interfaces.C.unsigned;

end PGXS.Types;
