*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC006......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC006                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC006                    .
CONTROLS: TCTRL_ZSDSCAC006
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC006                    .
TABLES: ZSDSCAC006                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
