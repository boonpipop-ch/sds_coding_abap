*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC002......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC002                    .
CONTROLS: TCTRL_ZSDSFIC002
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC002                    .
TABLES: ZSDSFIC002                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
