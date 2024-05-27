       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgm3.
       AUTHOR. safaa.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ENTREE ASSIGN TO "fichier.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                ACCESS MODE IS SEQUENTIAL
                FILE STATUS IS STATUT-F-ENTREE.

           SELECT F-SORTIE ASSIGN TO "fichier-sortie.cpy"
                ORGANIZATION IS LINE SEQUENTIAL
                ACCESS MODE IS SEQUENTIAL
                FILE STATUS IS STATUT-F-SORTIE.

       DATA DIVISION.
       FILE SECTION.
       
       FD  F-ENTREE
           RECORD CONTAINS 1 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  ENREGISTREMENT-ENTREE PIC X(1000).

       FD  F-SORTIE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  ENREGISTREMENT-SORTIE PIC X(80).

       WORKING-STORAGE SECTION.
       
      * Statut du fichier d'entrée
       01  STATUT-F-ENTREE        PIC X(02).
           88 ENTREE-STATUT-OK    VALUE "00".
           88 ENTREE-STATUT-EOF   VALUE "10".

      * Statut du fichier de sortie
       01  STATUT-F-SORTIE        PIC X(02).
           88 SORTIE-STATUT-OK    VALUE "00".

      * Variables pour le traitement des lignes d'entrée
       01  LIGNE-ENTREE.
           03 COMPTEUR-MOTS      PIC 9(04) VALUE 1.
           03 TABLEAU-MOTS OCCURS 1 TO 1000 TIMES
                     DEPENDING ON COMPTEUR-MOTS
                     INDEXED BY INDEX-MOT.
               05 MOT             PIC X(50).
               05 LONGUEUR-MOT    PIC 9(02).
               05 LONGUEUR-ESPACE PIC 9(02).

      * Index pour le traitement des caractères
       01  INDEX-CARACTERE       PIC 9(04) VALUE 1.

      * Format de sortie pour le copybook
       01  FORMAT-COPYBOOK.
           03 GROUPE-CB          PIC X(16) 
                                 VALUE "       01 .".
           03 FILLER-DEBUT-CB    PIC X(27) 
                                 VALUE "           05 FILLER PIC X(".
           03 FILLER-VALUE-CB    PIC X(08) 
                                 VALUE ") VALUE ".
           03 FILLER-ESPACES-CB  PIC X(07) 
                                 VALUE "SPACES.".

       PROCEDURE DIVISION.
       
      * Point d'entrée principal
       DEBUT-PRINCIPAL.
           PERFORM LIRE-FICHIER-ENTREE  
           THRU FIN-LIRE-FICHIER-ENTREE.
           PERFORM ECRIRE-FICHIER-SORTIE 
           THRU FIN-ECRIRE-FICHIER-SORTIE.
           STOP RUN.

      * Lecture du fichier d'entrée
       LIRE-FICHIER-ENTREE.
           OPEN INPUT F-ENTREE.
           IF STATUT-F-ENTREE = "00"
              SET ENTREE-STATUT-OK TO TRUE
              PERFORM UNTIL ENTREE-STATUT-EOF
                 READ F-ENTREE 
                 AT END 
                    SET ENTREE-STATUT-EOF TO TRUE
                 NOT AT END 
                    PERFORM DIVISER-LIGNE-ENTREE 
                       THRU FIN-DIVISER-LIGNE-ENTREE
                 END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE STATUT-F-ENTREE
           END-IF.    
           CLOSE F-ENTREE.
       FIN-LIRE-FICHIER-ENTREE.
           EXIT.

      * Division de la ligne d'entrée en mots
       DIVISER-LIGNE-ENTREE.
           PERFORM VARYING INDEX-CARACTERE FROM 1 BY 1 
           UNTIL 
           INDEX-CARACTERE > FUNCTION LENGTH(ENREGISTREMENT-ENTREE)
              IF ENREGISTREMENT-ENTREE(INDEX-CARACTERE:1) = SPACE 
                 ADD 1 TO LONGUEUR-ESPACE(COMPTEUR-MOTS)
                 IF ENREGISTREMENT-ENTREE(INDEX-CARACTERE + 1:1) 
                 NOT = SPACE
                    ADD 1 TO COMPTEUR-MOTS
                 END-IF
              ELSE 
                 ADD 1 TO LONGUEUR-MOT(COMPTEUR-MOTS)
                 MOVE ENREGISTREMENT-ENTREE(INDEX-CARACTERE:1) 
                    TO MOT(COMPTEUR-MOTS)(LONGUEUR-MOT(COMPTEUR-MOTS):1)
              END-IF
           END-PERFORM.
       FIN-DIVISER-LIGNE-ENTREE.
           EXIT.

      * Écriture des mots dans le fichier de sortie au format copybook
       ECRIRE-FICHIER-SORTIE.
           OPEN OUTPUT F-SORTIE.
           WRITE ENREGISTREMENT-SORTIE FROM GROUPE-CB.
           PERFORM VARYING INDEX-MOT FROM 1 BY 1
                   UNTIL INDEX-MOT >= COMPTEUR-MOTS
              IF MOT(INDEX-MOT) NOT = SPACE
                 INITIALIZE ENREGISTREMENT-SORTIE
                 STRING FILLER-DEBUT-CB 
                        LONGUEUR-MOT(INDEX-MOT) 
                        FILLER-VALUE-CB 
                        "'" FUNCTION TRIM(MOT(INDEX-MOT)) "'."
                        DELIMITED BY SIZE
                        INTO ENREGISTREMENT-SORTIE
                 WRITE ENREGISTREMENT-SORTIE
                 INITIALIZE ENREGISTREMENT-SORTIE
                 STRING FILLER-DEBUT-CB 
                        LONGUEUR-ESPACE(INDEX-MOT) 
                        FILLER-VALUE-CB
                        FILLER-ESPACES-CB
                        DELIMITED BY SIZE
                        INTO ENREGISTREMENT-SORTIE
                 WRITE ENREGISTREMENT-SORTIE
              END-IF
           END-PERFORM.
           CLOSE F-SORTIE.
       FIN-ECRIRE-FICHIER-SORTIE.
           EXIT.
