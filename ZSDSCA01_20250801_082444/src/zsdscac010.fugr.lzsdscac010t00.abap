*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC010......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC010                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC010                    .
CONTROLS: TCTRL_ZSDSCAC010
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC010                    .
TABLES: ZSDSCAC010                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
