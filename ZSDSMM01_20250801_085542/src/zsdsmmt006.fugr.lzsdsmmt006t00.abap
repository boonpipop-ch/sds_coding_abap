*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT006......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT006                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT006                    .
CONTROLS: TCTRL_ZSDSMMT006
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT006                    .
TABLES: ZSDSMMT006                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
