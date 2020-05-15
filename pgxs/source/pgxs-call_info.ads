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

with PGXS.Types;

package PGXS.Call_Info is

   subtype Argument_Count is PGXS.Types.Int_32
     range 0 .. PGXS.Types.Int_32'Last;

   subtype Argument_Index is PGXS.Types.Int_32
     range 0 .. PGXS.Types.Int_32'Last;

   type Func_Type_Class is
     (Typefunc_Scalar,     --  scalar result type
      Typefunc_Composite,  --  determinable rowtype result
      Typefunc_Record,     --  indeterminate rowtype result
      Typefunc_Other)      --  bogus type, e.g. pseudotype
     with Convention => C;

   function Arguments (Args : Function_Call_Info) return Argument_Count
     with Import, Convention => C, Link_Name => "__ada_PG_NARGS";

   function Get_Arg_Is_Null
     (Args : Function_Call_Info; Arg : Argument_Index) return PGXS.Types.Bool
     with Import, Convention => C, Link_Name => "__ada_PG_ARGISNULL";

   function Get_Arg_Type
     (Args : Function_Call_Info; Arg : Argument_Index) return PGXS.Types.Oid
     with Import, Convention => C, Link_Name => "__ada_PG_get_fn_expr_argtype";

   function Get_Return_Type (Args : Function_Call_Info) return PGXS.Types.Oid
     with Import, Convention => C, Link_Name => "__ada_PG_get_fn_expr_rettype";

   function Get_Call_Result_Type
     (Args   : Function_Call_Info;
      Oid    : out PGXS.Types.Oid;
      Result : out Tuple_Desc) return Func_Type_Class
     with Import, Convention => C, Link_Name => "get_call_result_type";

   function Get_Expr_Result_Type
     (Args   : Function_Call_Info;
      Oid    : out PGXS.Types.Oid;
      Result : out Tuple_Desc) return Func_Type_Class
     with Import, Convention => C, Link_Name => "get_expr_result_type";

   function Get_Func_Result_Type
     (Args   : Function_Call_Info;
      Oid    : out PGXS.Types.Oid;
      Result : out Tuple_Desc) return Func_Type_Class
     with Import, Convention => C, Link_Name => "get_func_result_type";

   function Get_Arg
     (Args : Function_Call_Info; Arg : Argument_Index) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_DATUM";

   function Return_Null (Args : Function_Call_Info) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_NULL";

   function Return_Void (Args : Function_Call_Info) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_VOID";

   --  Boolean

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Bool
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_BOOL";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Bool) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_BOOL";

   --  Int_16

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Int_16
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_INT16";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Int_16) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_INT16";

   --  UInt_16

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.UInt_16
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_UINT16";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.UInt_16) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_UINT16";

   --  Int_32

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Int_32
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_INT32";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Int_32) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_INT32";

   --  UInt_32

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.UInt_32
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_UINT32";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.UInt_32) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_UINT32";

   --  Int_64

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Int_64
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_INT64";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Int_64) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_INT64";

   --  Float_4

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Float_4
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_FLOAT4";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Float_4) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_FLOAT4";

   --  Float_8

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Float_8
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_FLOAT8";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Float_8) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_FLOAT8";

   --  Byte_A

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Byte_A
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_BYTEA_P";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Byte_A) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_BYTEA_P";

   --  Text

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Text
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_TEXT_P";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Text) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_TEXT_P";

   --  Bp_Char

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Bp_Char
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_BPCHAR_P";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Bp_Char) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_BPCHAR_P";

   --  Var_Char

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Types.Var_Char
     with Import, Convention => C, Link_Name => "__ada_PG_GETARG_VARCHAR_P";

   function Return_Value
     (Args : Function_Call_Info; Item : PGXS.Types.Var_Char) return Datum
     with Import, Convention => C, Link_Name => "__ada_PG_RETURN_VARCHAR_P";

   --  Heap_Tuple_Header

   function Get_Arg
     (Args : Function_Call_Info;
      Arg  : Argument_Index) return PGXS.Heap_Tuple_Header
     with Import,
          Convention => C,
          Link_Name => "__ada_PG_GETARG_HEAPTUPLEHEADER";

end PGXS.Call_Info;
