*&---------------------------------------------------------------------*
*& Include          ZSDSFIR0350_CLS
*&---------------------------------------------------------------------*
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR.

    CLASS-METHODS :
      COPY_DATA,
      SKIP.

ENDCLASS.
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.
  METHOD COPY_DATA.

    LOOP AT GT_MAINITEM ASSIGNING FIELD-SYMBOL(<LFS_DATA>).
      READ TABLE GT_ACCA INTO DATA(LS_ACCA) INDEX 1.
      IF SY-SUBRC EQ 0.
        <LFS_DATA>-AUFNR    = LS_ACCA-AUFNR.
        <LFS_DATA>-PS_POSID = LS_ACCA-PS_POSID.
        <LFS_DATA>-PRCTR    = LS_ACCA-PRCTR.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD SKIP.

    DATA:
      LR_SELECTIONS  TYPE REF TO CL_SALV_SELECTIONS,
      LT_ROWS        TYPE SALV_T_ROW,
      LF_NO_SELECTED LIKE SY-TFILL.

    DATA : LS_ZSDSFIT063 TYPE ZSDSFIT063.

    DATA : LS_STBL     TYPE LVC_S_STBL.

    LS_STBL-COL = ABAP_TRUE.
    LS_STBL-ROW = ABAP_TRUE.

    LR_SELECTIONS = GR_TABLE->GET_SELECTIONS( ).
    LT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).
    DESCRIBE TABLE LT_ROWS LINES LF_NO_SELECTED.
*
    IF LF_NO_SELECTED IS INITIAL.
*         no selection made
      MESSAGE I142(AM).
    ELSEIF LF_NO_SELECTED GT 1.
      MESSAGE 'select 1 item'(005) TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      DATA(LF_ROWS) = LT_ROWS[ 1 ].
      READ TABLE GT_MAIN INTO DATA(LS_MAIN) INDEX LF_ROWS.
      IF SY-SUBRC EQ 0.
        LS_ZSDSFIT063-BUKRS = LS_MAIN-BUKRS.
        LS_ZSDSFIT063-BELNR = LS_MAIN-BELNR.
        LS_ZSDSFIT063-GJAHR = LS_MAIN-GJAHR.
        LS_ZSDSFIT063-ERNAM = SY-UNAME.
        LS_ZSDSFIT063-ERDAT = SY-DATUM.
        LS_ZSDSFIT063-ERZET = SY-UZEIT.
        LS_ZSDSFIT063-AENAM = SY-UNAME.
        LS_ZSDSFIT063-AEDAT = SY-DATUM.
        LS_ZSDSFIT063-AEZET = SY-UZEIT.
        MODIFY ZSDSFIT063 FROM LS_ZSDSFIT063.
        COMMIT WORK AND WAIT.
        DELETE GT_MAIN INDEX LF_ROWS.
      ENDIF.
    ENDIF.

    GR_TABLE->REFRESH( S_STABLE     = LS_STBL
                       REFRESH_MODE = IF_SALV_C_REFRESH=>SOFT ).

  ENDMETHOD.
ENDCLASS.
CLASS LCL_HANDLE_EVENTS DEFINITION DEFERRED.

DATA: GR_EVENTS TYPE REF TO LCL_HANDLE_EVENTS ##NEEDED.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS
        IMPORTING E_SALV_FUNCTION.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS IMPLEMENTATION.

  METHOD ON_USER_COMMAND.
    DATA:
      LR_SELECTIONS  TYPE REF TO CL_SALV_SELECTIONS,
      LT_ROWS        TYPE SALV_T_ROW,
      LF_NO_SELECTED LIKE SY-TFILL.

    FREE: GT_MAINITEM, GT_ACCA, GT_POSTITEM.

    CLEAR ZSDSFIS074.
*
    CASE E_SALV_FUNCTION.
      WHEN 'UPD'.
        LR_SELECTIONS = GR_TABLE->GET_SELECTIONS( ).
        LT_ROWS = LR_SELECTIONS->GET_SELECTED_ROWS( ).
        DESCRIBE TABLE LT_ROWS LINES LF_NO_SELECTED.
*
        IF LF_NO_SELECTED IS INITIAL.
*         no selection made
          MESSAGE I142(AM).
        ELSEIF LF_NO_SELECTED GT 1.
          MESSAGE 'select 1 item'(005) TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          DATA(LF_ROWS) = LT_ROWS[ 1 ].
          READ TABLE GT_MAIN INTO DATA(LS_MAIN) INDEX LF_ROWS.
          IF SY-SUBRC EQ 0.
*...enqueue document
            PERFORM F_FIDOCUMENT_ENQUEUE USING LS_MAIN-BUKRS
                                               LS_MAIN-BELNR
                                               LS_MAIN-GJAHR.
*
            PERFORM F_FIDOCUMENT_PROCEEDED USING LS_MAIN-BUKRS
                                                 LS_MAIN-BELNR
                                                 LS_MAIN-GJAHR
                                                 LS_MAIN-BUZEI
                                                 ABAP_TRUE   "msg alert
                                           CHANGING LS_MAIN-GUID LS_MAIN-STATUS_ICON.

            IF GF_MODE EQ GC_DISP OR GF_MODE EQ GC_EDIT.
              PERFORM F_GET_DOCUMENT_DATA USING LS_MAIN-GUID.
            ELSE.
              MOVE-CORRESPONDING  LS_MAIN TO ZSDSFIS074.

              IF ZSDSFIS074-METHOD IS INITIAL.
                ZSDSFIS074-METHOD = '1'.
              ENDIF.
            ENDIF.

            CALL SCREEN 9000.

          ENDIF.
        ENDIF.
      WHEN 'SKIP'.
        LCL_DATA=>SKIP( ).
    ENDCASE.

  ENDMETHOD.                    "on_double_click
  "on_added_function
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
