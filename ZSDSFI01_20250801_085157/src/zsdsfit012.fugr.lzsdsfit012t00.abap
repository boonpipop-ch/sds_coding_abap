*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT012......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT012                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT012                    .
CONTROLS: TCTRL_ZSDSFIT012
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT012                    .
TABLES: ZSDSFIT012                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
