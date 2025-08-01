*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC004......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC004                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC004                    .
CONTROLS: TCTRL_ZSDSMMC004
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC004                    .
TABLES: ZSDSMMC004                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
