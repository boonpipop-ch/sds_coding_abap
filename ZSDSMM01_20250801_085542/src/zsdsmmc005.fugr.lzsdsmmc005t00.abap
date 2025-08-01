*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC005......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC005                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC005                    .
CONTROLS: TCTRL_ZSDSMMC005
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC005                    .
TABLES: ZSDSMMC005                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
