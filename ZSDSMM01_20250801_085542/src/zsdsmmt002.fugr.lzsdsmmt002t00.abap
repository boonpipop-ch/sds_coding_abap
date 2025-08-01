*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT002......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT002                    .
CONTROLS: TCTRL_ZSDSMMT002
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT002                    .
TABLES: ZSDSMMT002                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
