*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC023......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC023                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC023                    .
CONTROLS: TCTRL_ZSDSFIC023
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC023                    .
TABLES: ZSDSFIC023                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
