CREATE TABLE pokemon(
    number INTEGER PRIMARY KEY,
    pokemon_name TEXT,
    url TEXT,
    type1 TEXT,
    type2 TEXT,
    species TEXT,
    height NUMERIC,
    weight NUMERIC,
    hp INTEGER,
    attack INTEGER,
    defense INTEGER,
    sp_attack INTEGER,
    sp_def INTEGER,
    speed INTEGER,
    total INTEGER,
    type3 TEXT,
    has_evo BOOLEAN,
    evo_id INTEGER,
    max_evo INTEGER,
    evo_ratio NUMERIC
);

COPY pokemon FROM '/tmp/pokedex.csv' WITH (FORMAT CSV, DELIMITER ',', NULL 'NA', HEADER);