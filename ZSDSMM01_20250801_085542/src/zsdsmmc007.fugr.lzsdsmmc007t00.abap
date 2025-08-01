*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC007......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC007                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC007                    .
CONTROLS: TCTRL_ZSDSMMC007
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC007                    .
TABLES: ZSDSMMC007                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
