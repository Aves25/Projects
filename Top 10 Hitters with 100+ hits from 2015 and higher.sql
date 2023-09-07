SELECT
p.nameFirst,
p.nameLast,
b.playerID,
b.yearID,
b.AB,
b.H
FROM
Batting AS b
INNER JOIN
People AS p
ON
b.playerID = p.playerID
WHERE
H >= 100 AND yearID >= 2015
Order By
H DESC
LIMIT
10
