/* SH: DDL Extractor started at 08.01.2006 23:36:57.218                       */

SET SQL DIALECT 3;

SET NAMES NONE;

/* < == Extracting database header... ===================================== > */
/* < ====================================================================== > */

CREATE DATABASE 'localhost:D:\Developpement\berlios\sofia\bin\sofia.fdb'
USER 'SYSDBA' PASSWORD 'masterkey'
PAGE_SIZE 4096
DEFAULT CHARACTER SET NONE;

/* < == Extracting domains... ============================================= > */
/* < ====================================================================== > */

CREATE DOMAIN VARCHAR100 AS
VARCHAR(100);

CREATE DOMAIN VARCHAR250 AS
VARCHAR(250);

CREATE DOMAIN VARCHAR32 AS
VARCHAR(32);

/* < == Extracting tables... ============================================== > */
/* < ====================================================================== > */

CREATE TABLE PERSONNES (
  PRS_ID        VARCHAR32 NOT NULL,
  PRS_CATEGORIE VARCHAR250,
  PRS_NOM       VARCHAR100,
  PRS_PRENOM    VARCHAR100
);

/* < == Extracting constraints (PRIMARY KEY)... =========================== > */
/* < ====================================================================== > */

ALTER TABLE PERSONNES ADD CONSTRAINT
PK_PERSONNES
PRIMARY KEY (PRS_ID);

/* < == Extracting indices... ============================================= > */
/* < ====================================================================== > */

CREATE INDEX IDX_PRS_CATEGORI
ON PERSONNES (PRS_CATEGORIE);

COMMIT WORK;

/* SH: DDL Extractor ended at 08.01.2006 23:36:57.593                         */
/* SH: Elapsed time 00:00:00.391                                              */

