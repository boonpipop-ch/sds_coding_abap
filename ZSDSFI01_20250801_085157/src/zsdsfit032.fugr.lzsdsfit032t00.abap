*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT032......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT032                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT032                    .
CONTROLS: TCTRL_ZSDSFIT032
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT032                    .
TABLES: ZSDSFIT032                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
