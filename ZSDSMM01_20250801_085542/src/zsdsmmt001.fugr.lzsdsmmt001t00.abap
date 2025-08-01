*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT001......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT001                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT001                    .
CONTROLS: TCTRL_ZSDSMMT001
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT001                    .
TABLES: ZSDSMMT001                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
