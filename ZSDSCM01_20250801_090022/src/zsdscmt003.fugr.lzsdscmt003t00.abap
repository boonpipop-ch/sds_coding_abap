*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCMT003......................................*
DATA:  BEGIN OF STATUS_ZSDSCMT003                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCMT003                    .
CONTROLS: TCTRL_ZSDSCMT003
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDSCMT003                    .
TABLES: ZSDSCMT003                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
