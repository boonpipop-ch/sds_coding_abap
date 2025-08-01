FUNCTION-POOL zsdsfi18.                     "MESSAGE-ID ..

INCLUDE lzsdsfi18d01.                      " Local class definition


DATA: go_alv      TYPE REF TO cl_salv_table,
      go_events   TYPE REF TO lcl_handle_events,

      gt_coll_log TYPE zsdsfis144_tt,
      gv_cancel   TYPE char01.
