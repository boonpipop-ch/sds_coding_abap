*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC020......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC020                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC020                    .
CONTROLS: TCTRL_ZSDSFIC020
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC020                    .
TABLES: ZSDSFIC020                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
