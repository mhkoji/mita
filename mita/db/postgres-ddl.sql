DROP TABLE IF EXISTS pages;
CREATE TABLE pages (
  page_id char(36) NOT NULL PRIMARY KEY,
  created_on timestamp with time zone NOT NULL
);

DROP TABLE IF EXISTS page_text;
CREATE TABLE page_text (
  page_id char(36) NOT NULL PRIMARY KEY,
  string text NOT NULL,
  FOREIGN KEY (page_id) REFERENCES pages(page_id)
);

DROP TABLE IF EXISTS images;
CREATE TABLE images (
  image_id char(36) NOT NULL PRIMARY KEY,
  path varchar(256) NOT NULL
);

DROP TABLE IF EXISTS page_image;
CREATE TABLE page_image (
  page_id char(36) NOT NULL,
  image_id char(36) NOT NULL,
  FOREIGN KEY (page_id) REFERENCES pages(page_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
);

DROP TABLE IF EXISTS albums;
CREATE TABLE albums (
  album_id char(36) NOT NULL PRIMARY KEY,
  name varchar(256) NOT NULL,
  created_on timestamp with time zone NOT NULL
);

DROP TABLE IF EXISTS album_thumbnail_image;
CREATE TABLE album_thumbnail_image (
  album_id char(36) NOT NULL PRIMARY KEY,
  image_id char(36)  NOT NULL,
  FOREIGN KEY (album_id) REFERENCES albums(album_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
);

DROP TABLE IF EXISTS album_image;
CREATE TABLE album_image (
  album_id char(36) NOT NULL,
  image_id char(36) NOT NULL,
  FOREIGN KEY (album_id) REFERENCES albums(album_id),
  FOREIGN KEY (image_id) REFERENCES images(image_id)
);
