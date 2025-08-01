*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT056......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT056                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT056                    .
CONTROLS: TCTRL_ZSDSFIT056
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT056                    .
TABLES: ZSDSFIT056                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
