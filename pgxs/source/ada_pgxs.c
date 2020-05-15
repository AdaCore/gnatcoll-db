/*----------------------------------------------------------------------------
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
----------------------------------------------------------------------------*/

#include <postgres.h>
#include <fmgr.h>
#include <funcapi.h>
#include <executor/executor.h>

/* Expansion of the macro to link necessary global declarations into module */

PG_MODULE_MAGIC;

/* Expansion of the macro to export function version function to Ada code */

PG_FUNCTION_INFO_V1(_ada_function);

/* Subprogram to get and return special kinds of values */

int32 __ada_PG_NARGS(PG_FUNCTION_ARGS) { return PG_NARGS(); }
bool __ada_PG_ARGISNULL(PG_FUNCTION_ARGS, int32 n) { return PG_ARGISNULL(n); }
Datum __ada_PG_GETARG_DATUM(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_DATUM(n); }
Datum __ada_PG_RETURN_NULL(PG_FUNCTION_ARGS) { PG_RETURN_NULL(); }
Datum __ada_PG_RETURN_VOID(PG_FUNCTION_ARGS) { PG_RETURN_VOID(); }
Oid __ada_PG_get_fn_expr_argtype(PG_FUNCTION_ARGS, int32 n) { return get_fn_expr_argtype(fcinfo->flinfo, n); }
Oid __ada_PG_get_fn_expr_rettype(PG_FUNCTION_ARGS) { return get_fn_expr_rettype(fcinfo->flinfo); }

/* Data types with fixed length */

bool __ada_PG_GETARG_BOOL(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BOOL(n); }
int16 __ada_PG_GETARG_INT16(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_INT16(n); }
uint16 __ada_PG_GETARG_UINT16(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_UINT16(n); }
int32 __ada_PG_GETARG_INT32(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_INT32(n); }
uint32 __ada_PG_GETARG_UINT32(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_UINT32(n); }
int64 __ada_PG_GETARG_INT64(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_INT64(n); }
float4 __ada_PG_GETARG_FLOAT4(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_FLOAT4(n); }
float8 __ada_PG_GETARG_FLOAT8(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_FLOAT8(n); }

Datum __ada_PG_RETURN_BOOL(PG_FUNCTION_ARGS, bool x) { PG_RETURN_BOOL(x); }
Datum __ada_PG_RETURN_INT16(PG_FUNCTION_ARGS, int16 x) { PG_RETURN_INT16(x); }
Datum __ada_PG_RETURN_UINT16(PG_FUNCTION_ARGS, uint16 x) { PG_RETURN_UINT16(x); }
Datum __ada_PG_RETURN_INT32(PG_FUNCTION_ARGS, int32 x) { PG_RETURN_INT32(x); }
Datum __ada_PG_RETURN_UINT32(PG_FUNCTION_ARGS, uint32 x) { PG_RETURN_UINT32(x); }
Datum __ada_PG_RETURN_INT64(PG_FUNCTION_ARGS, int64 x) { PG_RETURN_INT64(x); }
Datum __ada_PG_RETURN_FLOAT4(PG_FUNCTION_ARGS, float4 x) { PG_RETURN_FLOAT4(x); }
Datum __ada_PG_RETURN_FLOAT8(PG_FUNCTION_ARGS, float8 x) { PG_RETURN_FLOAT8(x); }

/* Data types with variable length */

bytea *__ada_PG_GETARG_BYTEA_P(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BYTEA_P(n); }
bytea *__ada_PG_GETARG_BYTEA_PP(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BYTEA_PP(n); }
bytea *__ada_PG_GETARG_BYTEA_P_COPY(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BYTEA_P_COPY(n); }
bytea *__ada_PG_GETARG_BYTEA_P_SLICE(PG_FUNCTION_ARGS, int32 n, int32 a, int32 b) { return PG_GETARG_BYTEA_P_SLICE(n, a, b); }
text *__ada_PG_GETARG_TEXT_P(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_TEXT_P(n); }
text *__ada_PG_GETARG_TEXT_PP(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_TEXT_PP(n); }
text *__ada_PG_GETARG_TEXT_P_COPY(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_TEXT_P_COPY(n); }
text *__ada_PG_GETARG_TEXT_P_SLICE(PG_FUNCTION_ARGS, int32 n, int32 a, int32 b) { return PG_GETARG_TEXT_P_SLICE(n, a, b); }
BpChar *__ada_PG_GETARG_BPCHAR_P(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BPCHAR_P(n); }
BpChar *__ada_PG_GETARG_BPCHAR_PP(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BPCHAR_PP(n); }
BpChar *__ada_PG_GETARG_BPCHAR_P_COPY(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_BPCHAR_P_COPY(n); }
BpChar *__ada_PG_GETARG_BPCHAR_P_SLICE(PG_FUNCTION_ARGS, int32 n, int32 a, int32 b) { return PG_GETARG_BPCHAR_P_SLICE(n, a, b); }
VarChar *__ada_PG_GETARG_VARCHAR_P(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_VARCHAR_P(n); }
VarChar *__ada_PG_GETARG_VARCHAR_PP(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_VARCHAR_PP(n); }
VarChar *__ada_PG_GETARG_VARCHAR_P_COPY(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_VARCHAR_P_COPY(n); }
VarChar *__ada_PG_GETARG_VARCHAR_P_SLICE(PG_FUNCTION_ARGS, int32 n, int32 a, int32 b) { return PG_GETARG_VARCHAR_P_SLICE(n, a, b); }

Datum __ada_PG_RETURN_BYTEA_P(PG_FUNCTION_ARGS, bytea *x) { PG_RETURN_BYTEA_P(x); }
Datum __ada_PG_RETURN_TEXT_P(PG_FUNCTION_ARGS, text *x) { PG_RETURN_TEXT_P(x); }
Datum __ada_PG_RETURN_BPCHAR_P(PG_FUNCTION_ARGS, BpChar *x) { PG_RETURN_BPCHAR_P(x); }
Datum __ada_PG_RETURN_VARCHAR_P(PG_FUNCTION_ARGS, VarChar *x) { PG_RETURN_VARCHAR_P(x); }

/* Varsize types utilities */

void __ada_SET_VARSIZE(struct varlena *item, int32 size) { SET_VARSIZE(item, VARHDRSZ + size); }
int32 __ada_VARSIZE_ANY_EXHDR(struct varlena *item) { return VARSIZE_ANY_EXHDR(item); }
void *__ada_VARDATA_ANY(struct varlena *item) { return VARDATA_ANY(item); }

/* Composite-Type Arguments */

HeapTupleHeader __ada_PG_GETARG_HEAPTUPLEHEADER(PG_FUNCTION_ARGS, int32 n) { return PG_GETARG_HEAPTUPLEHEADER(n); }

/* Memory management */

void *__ada_palloc(int32 size) { return palloc(size); }
void *__ada_palloc_varlena(int32 size) { return palloc(VARHDRSZ + size); }

/* Datum Utilities */

bool __ada_PG_DatumGetBool(Datum x) { return DatumGetBool(x); }
Datum __ada_PG_BoolGetDatum(bool x) { return BoolGetDatum(x); }

int16 __ada_PG_DatumGetInt16(Datum x) { return DatumGetInt16(x); }
Datum __ada_PG_Int16GetDatum(int16 x) { return Int16GetDatum(x); }

uint16 __ada_PG_DatumGetUInt16(Datum x) { return DatumGetUInt16(x); }
Datum __ada_PG_UInt16GetDatum(uint16 x) { return UInt16GetDatum(x); }

int32 __ada_PG_DatumGetInt32(Datum x) { return DatumGetInt32(x); }
Datum __ada_PG_Int32GetDatum(int32 x) { return Int32GetDatum(x); }

uint32 __ada_PG_DatumGetUInt32(Datum x) { return DatumGetUInt32(x); }
Datum __ada_PG_UInt32GetDatum(uint32 x) { return UInt32GetDatum(x); }

int64 __ada_PG_DatumGetInt64(Datum x) { return DatumGetInt64(x); }
Datum __ada_PG_Int64GetDatum(int64 x) { return Int64GetDatum(x); }

float4 __ada_PG_DatumGetFloat4(Datum x) { return DatumGetFloat4(x); }
Datum __ada_PG_Float4GetDatum(float4 x) { return Float4GetDatum(x); }

float8 __ada_PG_DatumGetFloat8(Datum x) { return DatumGetFloat8(x); }
Datum __ada_PG_Float8GetDatum(float8 x) { return Float8GetDatum(x); }

bytea* __ada_PG_DatumGetByteaP(Datum x) { return DatumGetByteaP(x); }
Datum __ada_PG_ByteaPGetDatum(bytea* x) { return PointerGetDatum(x); }

text* __ada_PG_DatumGetTextP(Datum x) { return DatumGetTextP(x); }
Datum __ada_PG_TextPGetDatum(text* x) { return PointerGetDatum(x); }

BpChar* __ada_PG_DatumGetBpCharP(Datum x) { return DatumGetBpCharP(x); }
Datum __ada_PG_BpCharPGetDatum(BpChar* x) { return PointerGetDatum(x); }

VarChar* __ada_PG_DatumGetVarCharP(Datum x) { return DatumGetVarCharP(x); }
Datum __ada_PG_VarCharPGetDatum(VarChar* x) { return PointerGetDatum(x); }


