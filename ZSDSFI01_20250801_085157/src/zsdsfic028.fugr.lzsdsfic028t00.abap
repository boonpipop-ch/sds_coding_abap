*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC028......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC028                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC028                    .
CONTROLS: TCTRL_ZSDSFIC028
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC028                    .
TABLES: ZSDSFIC028                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
