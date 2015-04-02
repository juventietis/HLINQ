select w.name as name, w.age - m.age as diff
from couples as c, people as w, people as m
where c.her = w.name and c.him = m.name and w.age > m.age;