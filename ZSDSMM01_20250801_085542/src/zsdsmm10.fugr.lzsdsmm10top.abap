FUNCTION-POOL zsdsmm10.                   "MESSAGE-ID ..

* persistent data
DATA: gt_persistent_data TYPE SORTED TABLE OF zsdsmmt013
                         WITH UNIQUE KEY mandt ebeln ebelp,

* actual data
      gt_data            TYPE SORTED TABLE OF zsdsmmt013
                         WITH UNIQUE KEY mandt ebeln ebelp.

* dynpro output structure
TABLES: zsdsmms020.

* definitions required for dynpro/framework integration
DATA: ok-code TYPE sy-ucomm.
INCLUDE lmeviewsf01.
