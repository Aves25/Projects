SELECT
p.nameFirst,
p.nameLast,
f.playerID,
f.yearID,
f.PO,
CASE
WHEN f.PO < 500 THEN "Bad"
WHEN f.PO BETWEEN 500 AND 1000 THEN "Below Average"
WHEN f.PO BETWEEN 1000 AND 1499 THEN "Average"
WHEN f.PO >=1500 THEN "Good"
END AS Productivity
FROM
Fielding AS f
INNER JOIN
People AS p
ON
f.playerID = p.playerID
WHERE
Productivity IN ("Below Average", "Average") AND yearID>= 2015
ORDER BY
f.PO DESC