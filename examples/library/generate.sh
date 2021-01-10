cd generated

# Generate the Ada API
gnatcoll_db2ada -api=Database -orm=ORM -dbmodel=../dbschema.txt
