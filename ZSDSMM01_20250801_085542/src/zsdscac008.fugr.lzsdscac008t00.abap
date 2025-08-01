*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC008......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC008                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC008                    .
CONTROLS: TCTRL_ZSDSCAC008
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC008                    .
TABLES: ZSDSCAC008                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
