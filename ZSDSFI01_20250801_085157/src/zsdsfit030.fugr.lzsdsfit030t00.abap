*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT030......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT030                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT030                    .
CONTROLS: TCTRL_ZSDSFIT030
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT030                    .
TABLES: ZSDSFIT030                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
