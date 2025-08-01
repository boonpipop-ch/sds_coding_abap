*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE MC_CONCAT.

  LF_LEN = STRLEN( &1 ) + STRLEN( &2 ).
  IF LF_LEN GT LC_MAX.
    ASSIGN LS_TRADEPARTY-POSTALTRADEADDRESS-LINETWO TO &1.
  ENDIF.

  IF &1 IS INITIAL.
    &1 = &2.
  ELSE.
    &1 = |{ &1 } { &2 }|.
  ENDIF.

END-OF-DEFINITION.

DEFINE MC_READ_TEXT.
  CLEAR &4.
* Read Text data - English Version
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                            = &2
      LANGUAGE                      = 'E'
      NAME                          = &3
      OBJECT                        = &1
    TABLES
      LINES                         = &4
   EXCEPTIONS
     ID                            = 1
     LANGUAGE                      = 2
     NAME                          = 3
     NOT_FOUND                     = 4
     OBJECT                        = 5
     REFERENCE_CHECK               = 6
     WRONG_ACCESS_TO_ARCHIVE       = 7
     OTHERS                        = 8.
  IF SY-SUBRC NE 0.
*   Read Text data -Thai Version
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                            = &2
        LANGUAGE                      = '2'
        NAME                          = &3
        OBJECT                        = &1
      TABLES
        LINES                         = &4
     EXCEPTIONS
       ID                            = 1
       LANGUAGE                      = 2
       NAME                          = 3
       NOT_FOUND                     = 4
       OBJECT                        = 5
       REFERENCE_CHECK               = 6
       WRONG_ACCESS_TO_ARCHIVE       = 7
       OTHERS                        = 8.
    IF sy-subrc NE 0.
      CLEAR &4.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.
