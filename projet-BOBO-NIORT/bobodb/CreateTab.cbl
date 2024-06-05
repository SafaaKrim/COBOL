       IDENTIFICATION DIVISION.
       PROGRAM-ID. CreateTab.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       01 DB-CONN-INFO.
           05 DB-USERNAME PIC X(30) VALUE "cobol".
           05 DB-PASSWORD PIC X(10) VALUE SPACES.
           05 DB-NAME     PIC X(30) VALUE "bobodb".

       01 DUMMY-DATA.
           05 DUMMY-NAME-MUT      PIC X(30) VALUE 'Mutual1'.
           05 DUMMY-ADR1-MUT      PIC X(50) VALUE 'Adresse1'.
           05 DUMMY-ADR2-MUT      PIC X(50) VALUE 'Adresse2'.
           05 DUMMY-PC-MUT        PIC X(10) VALUE '75001'.
           05 DUMMY-TOWN-MUT      PIC X(50) VALUE 'Paris'.
           05 DUMMY-COUNTRY-MUT   PIC X(20) VALUE 'France'.

       PROCEDURE DIVISION.

      * Connect to the database
        EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'Connection failed with SQLCODE ' SQLCODE
               STOP RUN
           END-IF.

      * Create the MUTUAL table
       EXEC SQL
           CREATE TABLE MUTUAL (
               NAME_MUT VARCHAR(30),
               ADR1_MUT VARCHAR(50),
               ADR2_MUT VARCHAR(50),
               PC_MUT VARCHAR(10),
               TOWN_MUT VARCHAR(50),
               COUNTRY_MUT VARCHAR(20)
           )
       END-EXEC.

           IF SQLCODE = 0
               DISPLAY 'Table MUTUAL created successfully.'
           ELSE
               DISPLAY 'Failed to create table MUTUAL with SQLCODE ' 
               SQLCODE
           END-IF.

      * Insert dummy data into MUTUAL table
       EXEC SQL
           INSERT INTO MUTUAL (NAME_MUT, ADR1_MUT, ADR2_MUT, PC_MUT, 
           TOWN_MUT, COUNTRY_MUT)
           VALUES (:DUMMY-NAME-MUT, 
                  :DUMMY-ADR1-MUT, 
                  :DUMMY-ADR2-MUT, 
                  :DUMMY-PC-MUT, 
                  :DUMMY-TOWN-MUT, 
                  :DUMMY-COUNTRY-MUT)
       END-EXEC.

           IF SQLCODE = 0
               DISPLAY 'Data inserted into MUTUAL successfully.'
           ELSE
               DISPLAY 'Failed to insert data into MUTUAL with SQLCODE ' 
               SQLCODE
           END-IF.

      * Commit the transaction
       EXEC SQL COMMIT END-EXEC.

      * Create the USER table
       EXEC SQL
           CREATE TABLE USER (
               CODE_USER VARCHAR(10),
               PW_USER VARCHAR(30),
               CODE_PRF VARCHAR(10),
               LNAME_USER VARCHAR(30),
               FNAME_USER VARCHAR(30),
               DAT_CREA_USER DATE,
               DAT_UPDT_USER DATE,
               DAT_CLOSE_USER DATE
           )
       END-EXEC.

           IF SQLCODE = 0
               DISPLAY 'Table USER created successfully.'
           ELSE
               DISPLAY 'Failed to create table USER with SQLCODE ' 
               SQLCODE
           END-IF.

      * Insert dummy data into USER table
       EXEC SQL
           INSERT INTO USER (CODE_USER, PW_USER, CODE_PRF, LNAME_USER, 
           FNAME_USER, DAT_CREA_USER, DAT_UPDT_USER, DAT_CLOSE_USER)
           VALUES ('U001', 'password', 'P001', 'Doe', 'John', 
           '2022-01-01', '2022-01-02', NULL)
       END-EXEC.

           IF SQLCODE = 0
               DISPLAY 'Data inserted into USER successfully.'
           ELSE
               DISPLAY 'Failed to insert data into USER with SQLCODE ' 
               SQLCODE
           END-IF.

      * Commit the transaction
       EXEC SQL COMMIT END-EXEC.

      * Create the rest of the tables
       EXEC SQL
           CREATE TABLE PROFIL (
               CODE_PRF VARCHAR(10),
               NAME_PRF VARCHAR(30),
               DAT_CREA_PRF DATE,
               DAT_UPDT_PRF DATE,
               DAT_CLOSE_PRF DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE FUNCTION_TAB (
               CODE_FCT SERIAL PRIMARY KEY,
               LABS_FCT VARCHAR(15),
               LABL_FCT VARCHAR(80),
               CODE_FCT_MOTHER INTEGER,
               DAT_CREA_FCT DATE,
               DAT_UPDT_FCT DATE,
               DAT_CLOSE_FCT DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE FCT_PRF (
               CODE_PRF VARCHAR(10),
               CODE_FCT INTEGER,
               DAT_CREA_FCTPRF DATE,
               DAT_UPDT_FCTPRF DATE,
               DAT_CLOSE_FCTPRF DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE MEMBER (
               CODE_MBR VARCHAR(10),
               LNAME_MBR VARCHAR(30),
               FNAME_MBR VARCHAR(30),
               ADR1_MBR VARCHAR(50),
               ADR2_MBR VARCHAR(50),
               PC_MBR VARCHAR(10),
               TOWN_MBR VARCHAR(50),
               COUNTRY_MBR VARCHAR(20),
               BIRTH_DAT DATE,
               DOCTOR_MBR VARCHAR(50),
               TEL_MBR INTEGER,
               MAIL_MBR VARCHAR(50),
               DAT_CREA_MBR DATE,
               DAT_UPDT_MBR DATE,
               DAT_CLOSE_MBR DATE,
               ACTIVE_MBR VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE BANK (
               CODE_IBAN_BK INTEGER,
               NAME_BK VARCHAR(30),
               DAT_CREA_BK DATE,
               DAT_UPDT_BK DATE,
               DAT_CLOSE_BK DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE MBR_BK (
               CODE_IBAN_BK INTEGER,
               CODE_MBR VARCHAR(10),
               CODE_IBAN_MBR VARCHAR(34),
               DAT_CREA_MBRBK DATE,
               DAT_CLOSE_MBRBK DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE MBR_FAMILY (
               CODE_MBR VARCHAR(10),
               CODE_FAM SERIAL PRIMARY KEY,
               LNAME_MBRFAM VARCHAR(30),
               FNAME_MBRFAM VARCHAR(30),
               CODE_SECU VARCHAR(34),
               DAT_CREA_MBRFAM DATE,
               DAT_UPDT_MBRFAM DATE,
               ROOT_MBRFAM VARCHAR(1),
               ACTIVE_MBRFAM VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE CONTRACT (
               CODE_CNT VARCHAR(10),
               LABS_CNT VARCHAR(10),
               LABL_CNT VARCHAR(80),
               DAT_CREA_CNT DATE,
               DAT_UPDT_CNT DATE,
               ACTIVE_CNT VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE CNT_GAR (
               CODE_CNT VARCHAR(10),
               CODE_GAR INTEGER,
               CODE_MNT INTEGER,
               DAT_CREA_CNTGAR DATE,
               DAT_MAJ_CNTGAR DATE,
               ACTIF_CNTGAR VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE GUARANTEE (
               CODE_GAR SERIAL PRIMARY KEY,
               FLAG_GAR VARCHAR(1),
               LABS_GAR VARCHAR(10),
               LABL_GAR VARCHAR(80),
               DAT_ACT_GAR DATE,
               TYP_MNT VARCHAR(1),
               CODE_MNT INTEGER,
               PERIODICITY INTEGER,
               FLAG_MNT_GAR BOOLEAN,
               ROOT_GAR INTEGER,
               DAT_CREA_GAR DATE,
               DAT_UPDT_GAR DATE,
               ACTIVE_GAR VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE MNT_GAR (
               CODE_MNT SERIAL PRIMARY KEY,
               CODE_GAR INTEGER,
               DAT_CREA_MNTGAR DATE,
               ACTIVE_MNTGAR VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE AMOUNT (
               CODE_MNT SERIAL PRIMARY KEY,
               TYP_MNT VARCHAR(1),
               LABS_MNT VARCHAR(10),
               AMOUNT_MNT INTEGER,
               COST_MNT DECIMAL,
               DAT_CREA_MNT DATE,
               BASE_MNT VARCHAR(1),
               ACTIVE_MNT VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE MBR_CNT (
               CODE_MBR VARCHAR(10),
               CODE_CNT VARCHAR(10),
               COST_CNT INTEGER,
               DAT_DUEDATE DATE,
               PAYMENT_TYP VARCHAR(1),
               DAT_CREA_MBRCNT DATE,
               DAT_UPDT_MBRCNT DATE,
               ACTIVE_MBRCNT VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE MBR_CNT_GAR (
               CODE_MBR VARCHAR(10),
               CODE_CNT VARCHAR(10),
               CODE_GAR INTEGER,
               AMOUNT_CNTGAR INTEGER,
               COST_CNTGAR DECIMAL,
               DAT_CREA_CNTGAR DATE,
               DAT_MAJ_CNTGAR DATE,
               ACTIF_CNTGAR VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE INVOICE (
               NUM_INV VARCHAR(10),
               CODE_MBR VARCHAR(10),
               DAT_CREA_INV DATE,
               DAT_CLOSE_INV DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE INV_LINE (
               NUM_INV VARCHAR(10),
               CODE_CNT VARCHAR(10)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE INV_DUEDATE (
               NUM_INV VARCHAR(10),
               NUM_MOUNTH INTEGER,
               DUE_AMOUNT DECIMAL,
               DUE_PAID BOOLEAN,
               DUE_RELOUNCH INTEGER,
               DAT_LST_RELAUNCH DATE,
               DAT_CREA_INV DATE
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE REIMBURSEMENT (
               CODE_REIMB INTEGER,
               CODE_CNT VARCHAR(10),
               CODE_MBR VARCHAR(10),
               CODE_GAR INTEGER,
               DAT_CREA_REIMB DATE,
               REIMB_AMOUNT DECIMAL,
               REIMB_PAID BOOLEAN,
               REIMB_DATE DATE,
               REIMB_TYPE VARCHAR(1)
           )
       END-EXEC.

       EXEC SQL
           CREATE TABLE ASKED_REIMB (
               CODE_REIMB SERIAL PRIMARY KEY,
               CODE_CNT VARCHAR(10),
               CODE_MBR VARCHAR(10),
               CODE_GAR INTEGER,
               DAT_CREA_REIMB DATE,
               REIMB_AMOUNT DECIMAL,
               SECU_AMOUNT DECIMAL,
               SECU_BASE INTEGER,
               REIMB_TOPAY BOOLEAN,
               REIMB_REASON VARCHAR(50),
               REIMB_TYPE VARCHAR(1)
           )
       END-EXEC.

           IF SQLCODE = 0
               DISPLAY 'All tables created successfully.'
           ELSE
            DISPLAY 'Failed to create one or more tables with SQLCODE ' 
            SQLCODE
           END-IF.

      * Insert more dummy data into other tables as necessary
      * Example: Insert dummy data into MEMBER table
       EXEC SQL
           INSERT INTO MEMBER (CODE_MBR, LNAME_MBR, FNAME_MBR, 
           ADR1_MBR, ADR2_MBR, PC_MBR, TOWN_MBR, COUNTRY_MBR, BIRTH_DAT,
            DOCTOR_MBR, TEL_MBR, MAIL_MBR, DAT_CREA_MBR, DAT_UPDT_MBR,
             DAT_CLOSE_MBR, ACTIVE_MBR)
           VALUES ('M001', 'Doe', 'Jane', '123 Main St', '', '75000',
            'Paris', 'France', '1990-01-01', 'Dr. Smith', 1234567890,
             'jane.doe@example.com', '2022-01-01',
              '2022-01-02', NULL, 'Y')
       END-EXEC.

           IF SQLCODE = 0
               DISPLAY 'Data inserted into MEMBER successfully.'
           ELSE
               DISPLAY 'Failed to insert data into MEMBER with SQLCODE '
                SQLCODE
           END-IF.

      * Commit the transaction
       EXEC SQL COMMIT END-EXEC.

      * Disconnect from the database
       EXEC SQL DISCONNECT END-EXEC.

           STOP RUN.
