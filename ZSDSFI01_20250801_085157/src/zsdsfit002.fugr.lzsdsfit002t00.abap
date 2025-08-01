*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDSFIT002......................................*
DATA:  BEGIN OF STATUS_ZSDSFIT002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDSFIT002                    .
CONTROLS: TCTRL_ZSDSFIT002
            TYPE TABLEVIEW USING SCREEN '0101'.
*.........table declarations:.................................*
TABLES: *ZSDSFIT002                    .
TABLES: ZSDSFIT002                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
