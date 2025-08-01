*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT044......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT044                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT044                    .
CONTROLS: TCTRL_ZSDSFIT044
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT044                    .
TABLES: ZSDSFIT044                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
