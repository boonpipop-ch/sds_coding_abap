*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC002......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC002                    .
CONTROLS: TCTRL_ZSDSCAC002
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC002                    .
TABLES: ZSDSCAC002                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
