SELECT name3 as name,
       (age3) - (age2) as age
FROM (SELECT name as name2,
             age as age2
      FROM people as T1) as T1,
     (SELECT him as him1,
             her as her1
      FROM couples as T1) as T2,
     (SELECT name as name3,
             age as age3
      FROM people as T1) as T3
WHERE (((her1) = (name3)) AND (((him1) = (name2)) AND ((age3) > (age2))))

