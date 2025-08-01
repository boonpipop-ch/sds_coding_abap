class ZCL_SDSMM_ENHANCEMENT definition
  public
  final
  create public .

public section.

  methods CHECK_USER_STOCK
    importing
      !I_STORAGE_LOCATION type DFPS_LGORT_T
    returning
      value(R_RESULT) type ABAP_BOOLEAN .
  methods SEND_DATA_MATDOC
    importing
      !IT_GOITEM type TY_T_MSEG
      !IS_GOHEADER type MKPF .
  methods VALIDATION_MIGO
    importing
      !IT_ITEM type GOITEM_T
      !I_HEADER type GOHEAD
    returning
      value(R_ERR_MESSAGE) type CHAR255 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSMM_ENHANCEMENT IMPLEMENTATION.


  METHOD CHECK_USER_STOCK.
    DATA:
      BEGIN OF LS_LGORT,
        LGORT TYPE ZSDSMMC008-LGORT,
      END OF LS_LGORT.
    DATA:
      LT_LGORT LIKE HASHED TABLE OF LS_LGORT
                    WITH UNIQUE KEY LGORT.

    DATA : BEGIN OF LS_SLOC,
             LGORT TYPE ZSDSMMC008-LGORT,
           END OF LS_SLOC.
    DATA LT_SLOC LIKE TABLE OF LS_SLOC.

    DATA : LR_SLOC TYPE RANGE OF ZSDSMMC008-LGORT.

    FIELD-SYMBOLS : <fs_GOACTION> TYPE ANY.

    DATA : LV_GOACTION TYPE GOACTION.

    ASSIGN ('(SAPLMIGO)GODYNPRO-ACTION') TO <FS_GOACTION>.

    IF <FS_GOACTION> IS ASSIGNED.
      LV_GOACTION = <FS_GOACTION>.
    ENDIF.

*    IF LV_GOACTION NE 'A03'. " CANCEL DOC NOT CHECK
      SELECT COUNT( * )
        FROM ZSDSMMC008
        WHERE BNAME = @SY-UNAME
          AND LGORT = '*'.
      IF SY-SUBRC NE 0.
        LOOP AT I_STORAGE_LOCATION INTO DATA(LS_DATA).
          IF LS_DATA IS NOT INITIAL.
            LS_LGORT-LGORT = LS_DATA.
            INSERT LS_LGORT INTO TABLE LT_LGORT.
          ENDIF.
        ENDLOOP.

        IF LT_LGORT IS NOT INITIAL.
*          SELECT COUNT( * )
*            FROM @LT_LGORT AS A
*            INNER JOIN ZSDSMMC008 AS B ON A~LGORT EQ B~LGORT
*            WHERE B~BNAME = @SY-UNAME.
*          IF SY-SUBRC NE 0.
*            R_RESULT = ABAP_TRUE.
*          ELSE.
*            CLEAR : R_RESULT.
*          ENDIF.
          SELECT LGORT
            FROM ZSDSMMC008
            INTO TABLE @LT_SLOC
            WHERE BNAME = @SY-UNAME.
          IF SY-SUBRC NE 0.
            R_RESULT = ABAP_TRUE.
          ELSE.
            LR_SLOC =  VALUE #( FOR LS_TMP IN LT_SLOC INDEX INTO LV_INDEX
                                  (
                                    SIGN  = 'E' OPTION = 'CP' LOW = LS_TMP-LGORT )
                                   ).

            CLEAR : R_RESULT.
            LOOP AT LT_LGORT INTO LS_LGORT WHERE LGORT IN LR_SLOC[].
              R_RESULT = ABAP_TRUE.
              EXIT.
            ENDLOOP.
          ENDIF.

        ENDIF.
      ENDIF.
*    ENDIF.

  ENDMETHOD.


  METHOD SEND_DATA_MATDOC.

    LCL_DATA=>SAVE( IT_GOITEM ).

  ENDMETHOD.


  METHOD VALIDATION_MIGO.
    R_ERR_MESSAGE = LCL_DATA=>CHECK_RESERVATION( IT_ITEM ).
    IF R_ERR_MESSAGE IS NOT INITIAL.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
