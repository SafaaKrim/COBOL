      ****************************************************************** 
      * Auteur       : K.SAFAA                                         *
      * Date         : 24/04/2024                                      *      
      * Description  : Ce programme COBOL gère les données             *
      * des Train.                                                     *
      ******************************************************************   
       IDENTIFICATION DIVISION.
       PROGRAM-ID. train3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT F-INPUT 
           ASSIGN TO 'train.dat' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICIN.

           SELECT F-OUTPUT 
           ASSIGN TO 'train-planning.dat' 
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STAT-FICOUT.
       DATA DIVISION.
       FILE SECTION.
       COPY train1.
       COPY train2.
       WORKING-STORAGE SECTION.
      * gestion des status des fichiers  
       01  WS-STAT-FICIN     PIC XX                     .
           88  WS-STAT-FICIOK                     VALUE '00'.
           88  WS-STAT-FICIFIN                    VALUE '10'.
       01  WS-STAT-FICOUT    PIC XX                     .
           88  WS-STAT-OFICOK                     VALUE '00'.
           88  WS-STAT-OFICDBL                    VALUE '06'. 
       01  WS-NB-LUS         PIC 999.
       01  WS-NB-ECRIT       PIC 999.
       01  WS-NB-ARRET       PIC 99.
       01  WS-IND-MAX        PIC 99.
       01  WS-FS-ENREG       PIC 99.
       01  WS-FS-FIX         PIC 99   VALUE 27.
       01  WS-FIX-ENREG      PIC 99   VALUE 37.
       01  WS-FSO-ENREG      PIC 99.
       01  WS-IND-TAB        PIC 99.
       01  WS-LIG-RAP        PIC X(255).
       01  WS-HHMN           PIC 9(4).
       01  WS-HHMN-DET       REDEFINES WS-HHMN .
           05 WS-HH          PIC 99.
           05 WS-MN          PIC 99.  
       PROCEDURE DIVISION .

      * Ouverture des fichiers
           OPEN INPUT F-INPUT.
           IF (NOT WS-STAT-FICIOK) THEN 
              PERFORM TEST-STATUT
           END-IF.
           OPEN OUTPUT F-OUTPUT .
           IF (NOT WS-STAT-OFICOK) THEN 
              PERFORM TEST-STATUT
           END-IF.
           INITIALIZE WS-NB-LUS WS-NB-ECRIT .
      *  Lecture du fichier     
           READ F-INPUT.
           ADD 1 TO WS-NB-LUS.

           PERFORM  UNTIL (WS-STAT-FICIFIN) 
              PERFORM CALC-ARRIV
              PERFORM CALC-ARRET
              PERFORM ECRIT-FO
              READ F-INPUT
              ADD 1 TO WS-NB-LUS 
           END-PERFORM.        
      * Test du status 
       TEST-STATUT.
           IF (NOT WS-STAT-FICIOK) AND (NOT WS-STAT-FICIFIN) THEN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
              DISPLAY 'CODE RETOUR D''ERREUR' SPACE WS-STAT-FICIN 
              MOVE ALL  '/' TO WS-LIG-RAP
              DISPLAY WS-LIG-RAP 
           END-IF.  
           CLOSE F-INPUT.
           CLOSE F-OUTPUT.
           SUBTRACT 1 FROM WS-NB-LUS.
           DISPLAY 'Nombre d''enregistrements lus: ' WS-NB-LUS.
           DISPLAY 'Nombre d''enregistrements écrits: ' WS-NB-ECRIT.
           STOP RUN.
       CALC-ARRIV.
           INITIALIZE WS-HHMN.
           MOVE ALL SPACE TO RECO-TRAIN.
           MOVE FS-HHMN OF FS-TRAIN-DET  TO WS-HHMN .
           COMPUTE WS-HH = WS-HH + FS-NBH OF FS-TRAIN-DET.
           PERFORM UNTIL (WS-HH < 24) 
              COMPUTE WS-HH = WS-HH - 24
           END-PERFORM.
       CALC-ARRET.   
           INITIALIZE WS-IND-TAB.
           INITIALIZE WS-IND-MAX.
           INITIALIZE WS-NB-ARRET.
           MOVE 0 TO FS-NB-ARRET.
           PERFORM VARYING WS-IND-TAB FROM 1 BY 1 
                 UNTIL (WS-IND-TAB > (WS-FS-ENREG - WS-FS-FIX))
                    IF TRAIN-STOP(WS-IND-TAB) THEN 
                       ADD 1 TO WS-NB-ARRET
                    END-IF   
           END-PERFORM.      
           MOVE WS-IND-TAB TO WS-IND-MAX.

       ECRIT-FO.
           MOVE CORR FS-TRAIN-DET TO FSO-TRAIN-DET .
           MOVE WS-HHMN TO FS-HHMN-DEST.
           MOVE WS-NB-ARRET TO FS-NB-ARRET .
           PERFORM VARYING WS-IND-TAB FROM 1 BY 1 
                 UNTIL (WS-IND-TAB > WS-IND-MAX)
                    MOVE FS-ARR-TAB(WS-IND-TAB)
                    TO FSO-ARR-TAB(WS-IND-TAB) 
           END-PERFORM.      
           WRITE RECO-TRAIN.
           ADD 1 TO WS-NB-ECRIT .
           DISPLAY RECO-TRAIN. 
           INITIALIZE WS-FSO-ENREG .
           COMPUTE WS-FSO-ENREG = WS-FIX-ENREG + WS-IND-MAX.


      
