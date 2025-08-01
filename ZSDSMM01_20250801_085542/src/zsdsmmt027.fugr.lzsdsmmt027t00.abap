*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT027......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT027                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT027                    .
CONTROLS: TCTRL_ZSDSMMT027
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT027                    .
TABLES: ZSDSMMT027                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
