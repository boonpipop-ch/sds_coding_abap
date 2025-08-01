*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSSDC027......................................*
DATA:  BEGIN OF STATUS_ZSDSSDC027                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSSDC027                    .
CONTROLS: TCTRL_ZSDSSDC027
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSSDC027                    .
TABLES: ZSDSSDC027                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
