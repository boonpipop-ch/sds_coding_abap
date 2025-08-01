*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT022......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT022                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT022                    .
CONTROLS: TCTRL_ZSDSFIT022
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT022                    .
TABLES: ZSDSFIT022                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
