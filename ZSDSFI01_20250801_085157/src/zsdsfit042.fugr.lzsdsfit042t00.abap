*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT042......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT042                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT042                    .
CONTROLS: TCTRL_ZSDSFIT042
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT042                    .
TABLES: ZSDSFIT042                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
