*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC017......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC017                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC017                    .
CONTROLS: TCTRL_ZSDSFIC017
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC017                    .
TABLES: ZSDSFIC017                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
