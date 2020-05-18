
CREATE TABLE emp
 (name   CHARACTER VARYING NOT NULL UNIQUE,
  salary INTEGER);

INSERT INTO emp VALUES ('Bill',  900);
INSERT INTO emp VALUES ('Bob',    90);
INSERT INTO emp VALUES ('Sam',     9);
INSERT INTO emp VALUES ('Null', null);

CREATE TYPE apgxs_composite_type AS (x integer, y integer);
