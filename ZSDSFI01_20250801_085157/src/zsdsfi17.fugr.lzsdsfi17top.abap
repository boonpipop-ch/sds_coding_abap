FUNCTION-POOL zsdsfi17.                     "MESSAGE-ID ..

* INCLUDE LZSDSFI17D...                      " Local class definition

TYPES:
  gty_data TYPE x255,
  tt_data  TYPE STANDARD TABLE OF gty_data.

DATA:
  gt_data             TYPE tt_data          ##NEEDED,

  go_custom_container TYPE REF TO cl_gui_custom_container ##NEEDED,
  go_html             TYPE REF TO cl_gui_html_viewer  ##NEEDED,

  gv_container        TYPE scrfname ##NEEDED,

  gv_alignment        TYPE i        ##NEEDED,
  gv_url              TYPE char255  ##NEEDED,
  gv_ok_code          TYPE sy-ucomm ##NEEDED,
  gv_filename         TYPE string   ##NEEDED,
  gv_ext              TYPE zsdsde_attach_ext   ##NEEDED.
