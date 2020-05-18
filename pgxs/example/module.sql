
CREATE OR REPLACE FUNCTION to_bytea(integer, integer) RETURNS bytea
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'to_bytea'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION get_x(bytea) RETURNS integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'get_x'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION get_y(bytea) RETURNS integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'get_y'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_num_args() RETURNS integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_num_args'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_num_args(integer) RETURNS integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_num_args'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_num_args(integer, bytea) RETURNS integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_num_args'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_arg_is_null(integer) RETURNS bool
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_arg_is_null'
    LANGUAGE C;

CREATE OR REPLACE FUNCTION apgxs_inverse_bool(bool) RETURNS bool
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_inverse_bool'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_add_one_smallint(smallint) RETURNS smallint
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_add_one_int16'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_add_one_integer(integer) RETURNS integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_add_one_int32'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_add_one_float4(float4) RETURNS float4
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_add_one_float4'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_add_one_float8(float8) RETURNS float8
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_add_one_float8'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_overpaid(emp, integer) RETURNS boolean
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_overpaid'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_composite(integer, integer) RETURNS apgxs_composite_type
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_composite'
    LANGUAGE C STRICT;

CREATE OR REPLACE FUNCTION apgxs_set_simple(integer) RETURNS SETOF integer
    AS '/home/godunko/AdaCore/SC06-078.pgsql/gnatcoll-db/pgxs/.libs/libadamodule', 'apgxs_set_simple'
    LANGUAGE C STRICT;
