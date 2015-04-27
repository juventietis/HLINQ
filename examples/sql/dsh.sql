DROP TABLE IF EXISTS facilities;
DROP TABLE IF EXISTS features;
DROP TABLE IF EXISTS meanings;

CREATE TABLE facilities (
	fac VARCHAR(255),
	cat VARCHAR(255)
);

CREATE TABLE features (
	ffac VARCHAR(255),
	feature VARCHAR(255)
);
CREATE TABLE meanings (
	mfeature VARCHAR(255),
	meaning TEXT
);