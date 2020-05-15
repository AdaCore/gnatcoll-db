How to test the binding
-----------------------

1. Build an example

2. Create database as ordinary user

    createdb test

3. Create database schema as ordinary user

    psql test -f example/schema.sql

4. Create functions declarations as PostgreSQL superuser

    psql test -f example/module.sql

5. Run the test script

    psql test -f testsuite/test_pgxs.sql

6. Compare the output with the expected result
