
all: setup
	gprbuild -p -P example/module.gpr
	gprbuild -p -P testsuite/module.gpr

setup:
	sed "s/@MODULEDIR@/`pwd | sed 's/\\//\\\\\\//g'`\/.libs/" example/module.sql.in > example/module.sql
	sed "s/@MODULEDIR@/`pwd | sed 's/\\//\\\\\\//g'`\/.libs/" testsuite/module.sql.in > testsuite/module.sql

clean:
	rm -rf .objs .libs
	rm -f example/module.sql testsuite/module.sql
	rm -f test_pgxs.output

initdb_ordinary:
	dropdb test || true
	createdb test
	psql test -f testsuite/schema.sql

initdb_super:
	psql test -f example/module.sql
	psql test -f testsuite/module.sql

check:
	psql -q -t test -f testsuite/test_pgxs.sql > test_pgxs.output
	diff -u testsuite/test_pgxs.expected test_pgxs.output
	rm test_pgxs.output
