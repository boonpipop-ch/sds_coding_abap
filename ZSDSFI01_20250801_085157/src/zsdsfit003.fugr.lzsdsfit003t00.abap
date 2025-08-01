*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT003......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT003                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT003                    .
CONTROLS: TCTRL_ZSDSFIT003
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT003                    .
TABLES: ZSDSFIT003                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
