select count (w.name)
from couples as c, people as w, people as m
where c.her = w.name and c.him = m.name and w.age > m.age;
