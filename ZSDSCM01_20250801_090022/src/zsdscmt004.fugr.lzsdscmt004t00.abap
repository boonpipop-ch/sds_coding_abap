*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCMT004......................................*
DATA:  BEGIN OF STATUS_ZSDSCMT004                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCMT004                    .
CONTROLS: TCTRL_ZSDSCMT004
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSCMT004                    .
TABLES: ZSDSCMT004                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
