    
       03 REC-F-INPUT-100        PIC X(100).
            05  R-LIG1           PIC X() VALUE SPACE.
            05 R-TITRE-ENTETE PIC X(17) 
            VALUE 'Ce tableux gère les notes des étudiants.'
            05 R-IG2- PIC X() VALUE SPACES.
       
       03 R-TABLAUX 

       03 R-ENPIED.
           05 R-LIG1-ETOILE    PIC X(24) VALUE SPACES.
           05 R-NBRE-STUD.
             10 PIC X(20) VALUE 'NBRE ETUDANTS : '.
             10 R-LIG3-ETOILE PIC X(5) VALUE SPACES.
             10 R-MOYAN-STUD  VALUE ' moyen d''élèves par classe : '.
             10 R-LIG3-ETOILE PIC X(5) VALUE SPACES.
             10 R--LIBELE-MY PIC X(22) VALUE 'MOIYAN PAR MATIANR : '.
             10 R-NBRE-MY PIC 9(3).
             10 R--LIG2-ETOILE     PIC X(24) VALUE SPACES.

