*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIC025......................................*
DATA:  BEGIN OF STATUS_ZSDSFIC025                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIC025                    .
CONTROLS: TCTRL_ZSDSFIC025
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSFIC025                    .
TABLES: ZSDSFIC025                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
