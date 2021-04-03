CREATE TABLE accounts (
  account_id char(36) NOT NULL PRIMARY KEY,
  username varchar(32) NOT NULL UNIQUE,
  password_hashed varchar(256) NOT NULL
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
