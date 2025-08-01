*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT005......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT005                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT005                    .
CONTROLS: TCTRL_ZSDSFIT005
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT005                    .
TABLES: ZSDSFIT005                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
