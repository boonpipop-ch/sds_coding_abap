FUNCTION-POOL ZSDSMM09 MESSAGE-ID me..                     "MESSAGE-ID ..
DEFINE mv.
  if not &1 = space.
    &2 = &1.
  else.
    clear &2.
  endif.
END-OF-DEFINITION.

TABLES: ekes.
* INCLUDE LZSDSMM09D...                      " Local class definition
