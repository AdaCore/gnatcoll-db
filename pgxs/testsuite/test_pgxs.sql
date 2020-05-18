
SELECT apgxs_num_args();
SELECT apgxs_num_args(100500);
SELECT apgxs_num_args(100500, 'x');
SELECT apgxs_arg_is_null(100);
SELECT apgxs_arg_is_null(null);
SELECT apgxs_inverse_bool(true);
SELECT apgxs_inverse_bool(false);
SELECT apgxs_add_one_smallint(CAST (1 as smallint));
SELECT apgxs_add_one_integer(CAST (2 as integer));
SELECT apgxs_add_one_float4(CAST (3 as float4));
SELECT apgxs_add_one_float8(CAST (4 as float8));

SELECT name, apgxs_overpaid(emp, 10) FROM emp WHERE name = 'Bill' OR name = 'Sam' Or name = 'Null' ORDER BY name;

SELECT to_bytea(2, 5);
SELECT get_x(to_bytea(3, 7));
SELECT get_y(to_bytea(4, 8));

SELECT apgxs_composite(5, 10);
