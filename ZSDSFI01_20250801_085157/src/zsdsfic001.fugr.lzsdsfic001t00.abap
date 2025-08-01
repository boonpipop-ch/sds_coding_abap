*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC001......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC001                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC001                    .
CONTROLS: TCTRL_ZSDSFIC001
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC001                    .
TABLES: ZSDSFIC001                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
