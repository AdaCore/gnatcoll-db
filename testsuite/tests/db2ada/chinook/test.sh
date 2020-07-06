# It is not part of the test, it is script to debug/develop test
rm db.db database*.ad?
set -e
sqlite3 -cmd ".read chinook.sql" db.db
gnatcoll_sqlite2ada -dbname db.db
gprbuild -p
obj/test

gnatcoll_sqlite2ada -dbname db.db -text > schema.txt
gnatcoll_sqlite2ada -dbmodel schema.txt -orm orm
