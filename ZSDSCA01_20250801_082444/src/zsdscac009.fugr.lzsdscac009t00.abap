*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC009......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC009                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC009                    .
CONTROLS: TCTRL_ZSDSCAC009
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC009                    .
TABLES: ZSDSCAC009                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
