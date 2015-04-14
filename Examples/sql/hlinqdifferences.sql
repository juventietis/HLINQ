EXPLAIN EXTENDED SELECT (w_8.name) as name , (w_8.age - m_7.age) as diff
FROM couples as c_6, people as m_7, people as w_8
WHERE c_6.her = w_8.name AND c_6.him = m_7.name AND w_8.age > m_7.age
