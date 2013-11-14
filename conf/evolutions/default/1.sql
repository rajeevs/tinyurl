# Tasks schema
 
# --- !Ups

CREATE SEQUENCE shorturl_id_seq;

CREATE TABLE urlmappings (
    shorturlid integer NOT NULL DEFAULT nextval('shorturl_id_seq') PRIMARY KEY,
    longurl varchar(512) NOT NULL,
    mappingalgoversion text
);
 
CREATE TABLE urlstats (
    shorturlid integer NOT NULL PRIMARY KEY,
    numclicks integer DEFAULT 0
);

# --- !Downs
 
DROP TABLE urlmappings;
DROP TABLE urlstats;
DROP SEQUENCE shorturl_id_seq;
