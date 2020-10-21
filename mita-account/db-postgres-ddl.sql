DROP TABLE IF EXISTS accounts;
CREATE TABLE accounts (
  account_id char(36) NOT NULL PRIMARY KEY,
  username varchar(32) NOT NULL UNIQUE,
  password_hashed varchar(256) NOT NULL
);
