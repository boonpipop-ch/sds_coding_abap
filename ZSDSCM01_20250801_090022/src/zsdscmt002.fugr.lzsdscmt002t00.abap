*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSCMT002......................................*
DATA:  BEGIN OF STATUS_ZSDSCMT002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSCMT002                    .
CONTROLS: TCTRL_ZSDSCMT002
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSCMT002                    .
TABLES: ZSDSCMT002                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
