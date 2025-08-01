*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMC003......................................*
DATA:  BEGIN OF STATUS_ZSDSMMC003                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMC003                    .
CONTROLS: TCTRL_ZSDSMMC003
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMC003                    .
TABLES: ZSDSMMC003                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
