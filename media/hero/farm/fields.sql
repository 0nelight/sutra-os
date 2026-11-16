SELECT field_id                          AS "field-id",
       trim(name)                        AS name,
       trim(crop)                        AS crop,
       CAST(hectares AS DECIMAL)         AS hectares,
       CAST(cost_per_hectare AS DECIMAL) AS "cost-per-hectare"
FROM register
