*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSSDT014......................................*
DATA:  BEGIN OF STATUS_ZSDSSDT014                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSSDT014                    .
CONTROLS: TCTRL_ZSDSSDT014
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDSSDT014                    .
TABLES: ZSDSSDT014                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
