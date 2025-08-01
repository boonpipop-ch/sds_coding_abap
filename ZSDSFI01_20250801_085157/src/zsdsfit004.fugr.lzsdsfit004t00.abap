*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT004......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT004                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT004                    .
CONTROLS: TCTRL_ZSDSFIT004
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT004                    .
TABLES: ZSDSFIT004                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
