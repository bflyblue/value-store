CREATE TABLE value (
    hashstr varchar PRIMARY KEY,
    value   bytea
);

CREATE TABLE variable (
    label   varchar PRIMARY KEY,
    value   bytea
);
