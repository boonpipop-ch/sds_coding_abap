*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC030......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC030                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC030                    .
CONTROLS: TCTRL_ZSDSFIC030
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC030                    .
TABLES: ZSDSFIC030                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
