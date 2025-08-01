*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSMMT020......................................*
DATA:  BEGIN OF STATUS_ZSDSMMT020                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSMMT020                    .
CONTROLS: TCTRL_ZSDSMMT020
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDSMMT020                    .
TABLES: ZSDSMMT020                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
