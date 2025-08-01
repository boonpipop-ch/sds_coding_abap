*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSSDC032......................................*
DATA:  BEGIN OF STATUS_ZSDSSDC032                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSSDC032                    .
CONTROLS: TCTRL_ZSDSSDC032
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSSDC032                    .
TABLES: ZSDSSDC032                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
