*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC006......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC006                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC006                    .
CONTROLS: TCTRL_ZSDSMMC006
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC006                    .
TABLES: ZSDSMMC006                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
