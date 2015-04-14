EXPLAIN EXTENDED SELECT name5 as name
FROM (SELECT name as name5,
             age as age5
      FROM people as T1) as T1,
     (SELECT age3 as age4,
             age2
      FROM (SELECT age as age3
            FROM people as T1
            WHERE ((name) = 'Drew')) as T1,
           (SELECT age1 as age2
            FROM (SELECT name as name1,
                         age as age1
                  FROM people as T1) as T1
            WHERE ((name1) = 'Edna')) as T2) as T2
WHERE (((age5) >= (age2)) AND ((age5) < (age4)));

