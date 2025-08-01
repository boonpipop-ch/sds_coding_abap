*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC026......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC026                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC026                    .
CONTROLS: TCTRL_ZSDSFIC026
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC026                    .
TABLES: ZSDSFIC026                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
