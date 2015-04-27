EXPLAIN ANALYZE SELECT name3 as name
FROM (SELECT name as name1,
             age as age1
      FROM people as T1) as T1,
     (SELECT age as age2
      FROM people as T1
      WHERE ((name) = 'Drew')) as T2,
     (SELECT name as name3,
             age as age3
      FROM people as T1) as T3
WHERE (((age3) >= (age1)) AND ((age3) < (age2))) AND ((name1) = 'Edna');

