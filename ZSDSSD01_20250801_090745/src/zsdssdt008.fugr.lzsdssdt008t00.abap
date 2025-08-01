*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSSDT008......................................*
DATA:  BEGIN OF STATUS_ZSDSSDT008                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSSDT008                    .
CONTROLS: TCTRL_ZSDSSDT008
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSSDT008                    .
TABLES: ZSDSSDT008                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
