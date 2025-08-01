*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT019......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT019                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT019                    .
CONTROLS: TCTRL_ZSDSMMT019
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT019                    .
TABLES: ZSDSMMT019                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
