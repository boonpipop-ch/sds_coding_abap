*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC008......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC008                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC008                    .
CONTROLS: TCTRL_ZSDSMMC008
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC008                    .
TABLES: ZSDSMMC008                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
