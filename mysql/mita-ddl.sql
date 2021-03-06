CREATE TABLE images (
  image_id char(36) NOT NULL PRIMARY KEY,
  source varchar(64) NOT NULL,
  path varchar(256) NOT NULL
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE albums (
  album_id char(36) NOT NULL PRIMARY KEY,
  name varchar(256) NOT NULL,
  created_on datetime NOT NULL
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE album_thumbnail_image (
  album_id char(36) NOT NULL PRIMARY KEY,
  image_id char(36)  NOT NULL,
  FOREIGN KEY (album_id) REFERENCES albums(album_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE album_image (
  album_id char(36) NOT NULL,
  image_id char(36) NOT NULL,
  FOREIGN KEY (album_id) REFERENCES albums(album_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE tags (
  tag_id char(36) NOT NULL PRIMARY KEY,
  name varchar(256) NOT NULL,
  added_on datetime NOT NULL
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE tag_content (
  tag_id char(36) NOT NULL,
  content_type varchar(64) NOT NULL,
  content_id char(36) NOT NULL,
  added_on datetime NOT NULL,
  FOREIGN KEY (tag_id) REFERENCES tags(tag_id)
) engine=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
