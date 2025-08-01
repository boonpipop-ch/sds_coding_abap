*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCAC007......................................*
DATA:  BEGIN OF STATUS_ZSDSCAC007                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCAC007                    .
CONTROLS: TCTRL_ZSDSCAC007
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCAC007                    .
TABLES: ZSDSCAC007                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
