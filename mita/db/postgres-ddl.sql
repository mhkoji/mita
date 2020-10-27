CREATE TABLE pages (
  page_id char(36) NOT NULL PRIMARY KEY,
  created_on timestamp with time zone NOT NULL
);

CREATE TABLE page_text (
  page_id char(36) NOT NULL PRIMARY KEY,
  string text NOT NULL,
  FOREIGN KEY (page_id) REFERENCES pages(page_id)
);

CREATE TABLE images (
  image_id char(36) NOT NULL PRIMARY KEY,
  path varchar(256) NOT NULL
);

CREATE TABLE page_image (
  page_id char(36) NOT NULL,
  image_id char(36) NOT NULL,
  FOREIGN KEY (page_id) REFERENCES pages(page_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
);

CREATE TABLE albums (
  album_id char(36) NOT NULL PRIMARY KEY,
  name varchar(256) NOT NULL,
  created_on timestamp with time zone NOT NULL
);

CREATE TABLE album_thumbnail_image (
  album_id char(36) NOT NULL PRIMARY KEY,
  image_id char(36)  NOT NULL,
  FOREIGN KEY (album_id) REFERENCES albums(album_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
);

CREATE TABLE album_image (
  album_id char(36) NOT NULL,
  image_id char(36) NOT NULL,
  FOREIGN KEY (album_id) REFERENCES albums(album_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
);

CREATE TABLE tags (
  tag_id char(36) NOT NULL PRIMARY KEY,
  name varchar(256) NOT NULL
);

CREATE TABLE tag_content (
  tag_id char(36) NOT NULL,
  content_type varchar(64) NOT NULL,
  content_id char(36) NOT NULL,
  FOREIGN KEY (tag_id) REFERENCES tags(tag_id)
);
