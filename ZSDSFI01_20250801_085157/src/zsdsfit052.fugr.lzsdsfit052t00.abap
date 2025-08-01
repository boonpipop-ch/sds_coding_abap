*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT052......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT052                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT052                    .
CONTROLS: TCTRL_ZSDSFIT052
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT052                    .
TABLES: ZSDSFIT052                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
