DROP TABLE IF EXISTS departments;
DROP TABLE IF EXISTS employees;
DROP TABLE IF EXISTS tasks;
CREATE TABLE departments (
	ddpt VARCHAR(255)
);

CREATE TABLE employees (
	edpt VARCHAR(255),
	emp VARCHAR(255)
);

CREATE TABLE tasks (
	temp VARCHAR(255),
	tsk VARCHAR(255)
);

INSERT INTO departments VALUES ('Product');
INSERT INTO departments VALUES ('Quality');
INSERT INTO departments VALUES ('Research');
INSERT INTO departments VALUES ('Sales');

INSERT INTO employees VALUES ('Product', 'Alex');
INSERT INTO employees VALUES ('Product', 'Bert');
INSERT INTO employees VALUES ('Research', 'Cora');
INSERT INTO employees VALUES ('Research', 'Drew');
INSERT INTO employees VALUES ('Research', 'Edna');
INSERT INTO employees VALUES ('Sales', 'Fred');

INSERT INTO tasks VALUES ('Alex', 'build');
INSERT INTO tasks VALUES ('Bert', 'build');
INSERT INTO tasks VALUES ('Cora', 'abstract');
INSERT INTO tasks VALUES ('Cora', 'build');
INSERT INTO tasks VALUES ('Cora', 'design');
INSERT INTO tasks VALUES ('Drew', 'abstract');
INSERT INTO tasks VALUES ('Drew', 'design');
INSERT INTO tasks VALUES ('Edna', 'abstract');
INSERT INTO tasks VALUES ('Edna', 'call');
INSERT INTO tasks VALUES ('Edna', 'design');
INSERT INTO tasks VALUES ('Fred', 'call');



