*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT005......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT005                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT005                    .
CONTROLS: TCTRL_ZSDSMMT005
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT005                    .
TABLES: ZSDSMMT005                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
