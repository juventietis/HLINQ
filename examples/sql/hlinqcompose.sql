EXPLAIN EXTENDED SELECT person_76.name
FROM people as person_71, people as person_73, people as person_76
WHERE person_71.name = 'Edna' AND person_73.name = 'Drew' AND person_71.age <= person_76.age AND person_76.age < person_73.age;

