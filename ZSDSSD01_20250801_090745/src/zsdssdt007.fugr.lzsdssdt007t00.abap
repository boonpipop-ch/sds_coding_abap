*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSSDT007......................................*
DATA:  BEGIN OF STATUS_ZSDSSDT007                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSSDT007                    .
CONTROLS: TCTRL_ZSDSSDT007
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSSDT007                    .
TABLES: ZSDSSDT007                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
