FUNCTION-POOL zsdsmm08.                     "MESSAGE-ID ..

* persistent data
DATA: gt_persistent_data TYPE SORTED TABLE OF zsdsmmt013
                         WITH UNIQUE KEY mandt ebeln ebelp,

* actual data
      gt_data            TYPE SORTED TABLE OF zsdsmmt013
                         WITH UNIQUE KEY mandt ebeln ebelp.

* dynpro output structure
TABLES: zsdsmms014,
        zsdsmms015,
        zsdsmms020.

DATA:    ok-code             TYPE sy-ucomm.
