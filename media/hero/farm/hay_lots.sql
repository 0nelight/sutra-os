SELECT trim(cut)                                         AS name,
       CAST(bales AS INTEGER)                            AS baled,
       lower(replace(split_part(cut, ',', 1), ' ', '-')) AS field
FROM log
