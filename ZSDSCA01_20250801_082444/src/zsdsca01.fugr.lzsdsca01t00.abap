*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC003......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC003                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC003                    .
CONTROLS: TCTRL_ZSDSCAC003
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC003                    .
TABLES: ZSDSCAC003                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
