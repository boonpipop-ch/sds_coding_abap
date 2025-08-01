*&--------------------------------------------------------------------*
*& Include          ZRFPPD0020_F01
*&--------------------------------------------------------------------*
*& Version/TP No: 2.00( F36K904021 )                                  *
*& Changed On/By: 21.08.2024 Warat (EY TH)                            *
*& Description : CR#018 - Dynamically Calling Program                 *
*&--------------------------------------------------------------------*

START-OF-SELECTION.

*<<<BEG LOCAL ADDONS CR018: Call Dynamic Program by Plant on 21.08.2024
  CLEAR GF_SUBMIT_PROGRAM.

  DATA(LF_CURRENT_PROGRAM) = SY-REPID.
  DATA(LF_PLANT) = SO_PLANT-LOW.

  SELECT * FROM ZRFPPC000 INTO TABLE @DATA(LT_ZRFPPC000)
    WHERE WERKS = @LF_PLANT
    AND PROG = @LF_CURRENT_PROGRAM.

  LOOP AT LT_ZRFPPC000 ASSIGNING FIELD-SYMBOL(<FS_MAINTAIN>).

    CLEAR LS_SELTAB.

    IF <FS_MAINTAIN>-FIELD = 'CALL_PROGRAM_ID'.
      GF_SUBMIT_PROGRAM = <FS_MAINTAIN>-LOW.
      CONTINUE.
    ENDIF.

    LS_SELTAB-SELNAME = <FS_MAINTAIN>-FIELD.
    LS_SELTAB-KIND    = <FS_MAINTAIN>-KIND.
    LS_SELTAB-SIGN    = <FS_MAINTAIN>-SIGN.
    LS_SELTAB-OPTION  = <FS_MAINTAIN>-SELOPTION.
    LS_SELTAB-LOW     = <FS_MAINTAIN>-LOW.
    APPEND LS_SELTAB TO LT_SELTAB.
    CLEAR LS_SELTAB.

  ENDLOOP.

  IF GF_SUBMIT_PROGRAM IS NOT INITIAL.

    SUBMIT (GF_SUBMIT_PROGRAM) WITH SELECTION-TABLE LT_SELTAB AND RETURN.

  ELSE.

    "ERROR"

  ENDIF.

*  SUBMIT ZRFPPD0010
*          WITH P_PLANT = SO_PLANT-LOW
*          WITH P_MATNR = SPACE
*          WITH CB_PE = ABAP_TRUE
*          WITH CB_PU = ABAP_TRUE
*          WITH CB_CMD = ABAP_TRUE
*          WITH CB_SPS = ABAP_TRUE
*          WITH CB_GA = ABAP_TRUE
*          WITH CB_LGT = ABAP_TRUE
*          WITH CB_SAG = ABAP_TRUE
*          WITH CB_ACG = ABAP_TRUE
*          WITH CB_QC = ABAP_TRUE
*          WITH CB_PC = ABAP_TRUE
*          WITH CB_BCG = ABAP_TRUE
*          WITH CB_RD = ABAP_TRUE
*          WITH CB_IT = ABAP_TRUE
*          WITH CB_INCPR = ABAP_TRUE
*          WITH CB_INCCR = ABAP_TRUE
*          WITH CB_INCGR = ABAP_TRUE
*        AND RETURN.

*>>>END LOCAL ADDONS CR018: Call Dynamic Program by Plant on 21.08.2024


                                                         "#EC CI_SUBMIT
  TRY.
      NEW CL_SALV_BS_RUNTIME_INFO( )->GET_DATA_REF(
        IMPORTING R_DATA = LT_DATA_MNU ).
      ASSIGN LT_DATA_MNU->* TO <LT_TEST_DATA>.
    CATCH CX_SALV_BS_SC_RUNTIME_INFO.
      MESSAGE S023(ZRFPP01).
      EXIT.
  ENDTRY.

*  BREAK-POINT.


  IF <LT_TEST_DATA> IS NOT ASSIGNED.
    MESSAGE S023(ZRFPP01).
    EXIT.
  ENDIF.

  NEW CL_SALV_BS_RUNTIME_INFO( )->CLEAR_ALL( ).

*<<<BEG LOCAL ADDONS CR018: Call Dynamic Program by Plant on 21.08.2024

  IF <LT_TEST_DATA> IS ASSIGNED.

    LOOP AT <LT_TEST_DATA> ASSIGNING FIELD-SYMBOL(<FS_TEST_MAP>).

      MOVE-CORRESPONDING <FS_TEST_MAP> TO LS_TEST_MNU.
      APPEND LS_TEST_MNU TO LT_TEST_MNU_STAT.

    ENDLOOP.

    IF LT_TEST_MNU_STAT IS NOT INITIAL.
      DELETE LT_TEST_MNU_STAT WHERE FIELD03 EQ ICON_LED_GREEN.

      IF LT_TEST_MNU_STAT IS INITIAL.
        MESSAGE S023(ZRFPP01).
        EXIT.
      ELSE.
        LOOP AT LT_TEST_MNU_STAT ASSIGNING FIELD-SYMBOL(<LFS_MATNR_MNU>).
          LS_MATNR-MATNR = <LFS_MATNR_MNU>-FIELD05.
          APPEND LS_MATNR TO LT_MATNR.
        ENDLOOP.

        SORT LT_MATNR.
        DELETE ADJACENT DUPLICATES FROM LT_MATNR.
        SELECT MATNR,
               MMSTA
          FROM MARC
          INTO TABLE @DATA(LT_MNU_A)
          FOR ALL ENTRIES IN @LT_MATNR
        WHERE MATNR EQ @LT_MATNR-MATNR.

        LOOP AT LT_TEST_MNU_STAT
          ASSIGNING FIELD-SYMBOL(<LT_MNU_DATA>).
          IF <LT_MNU_DATA>-FIELD01 EQ ICON_LED_GREEN
            AND <LT_MNU_DATA>-FIELD02 EQ ICON_LED_GREEN
            AND <LT_MNU_DATA>-FIELD03 EQ ICON_LED_RED.

            LT_HEADDATA-MATERIAL = <LT_MNU_DATA>-FIELD05.
            LT_PLANTDATA-PUR_STATUS = LC_Z3.
            LT_PLANTDATAX-PUR_STATUS = ABAP_TRUE.
            LT_PLANTDATA-PLANT = LF_PLANT.
            LT_PLANTDATAX-PLANT = LF_PLANT.

            READ TABLE LT_MNU_A ASSIGNING FIELD-SYMBOL(<LT_MNU_A_DATA>)
              WITH KEY MATNR = <LT_MNU_DATA>-FIELD05.

            IF SY-SUBRC EQ 0.
              IF <LT_MNU_A_DATA>-MMSTA EQ LC_Z1
                OR <LT_MNU_A_DATA>-MMSTA EQ LC_Z2.

*                BREAK-POINT.

                CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
                  EXPORTING
                    HEADDATA   = LT_HEADDATA
                    PLANTDATA  = LT_PLANTDATA
                    PLANTDATAX = LT_PLANTDATAX
                  IMPORTING
                    RETURN     = LT_RETURN.



*<<<BEG LOCAL ADDONS HC#107 - FIX MESSAGE ERROR from BAPI on 29.10.2024
                MESSAGE S216(ZRFPP01) WITH LT_HEADDATA-MATERIAL . " WITH <LT_MNU_DATA>-FIELD05.
*                MESSAGE |Material { LT_HEADDATA-MATERIAL } ; { LT_RETURN-MESSAGE }| TYPE LT_RETURN-TYPE.
**                LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
*                MESSAGE LT_RETURN-MESSAGE TYPE LT_RETURN-TYPE.
**                ENDLOOP.
*>>>END LOCAL ADDONS HC#107 - FIX MESSAGE ERROR from BAPI on 29.10.2024
*<<<BEG LOCAL ADDONS HC#107 - Remove Error Message for Mat Status other than Z1 on 29.10.2024
              ELSEIF <LT_MNU_A_DATA>-MMSTA EQ ''.
                MESSAGE S027(ZRFPP01) WITH <LT_MNU_DATA>-FIELD05.
              ENDIF.
*>>>END LOCAL ADDONS HC#107 - Remove Error Message for Mat Status other than Z1 on 29.10.2024

            ENDIF.
          ELSEIF <LT_MNU_DATA>-FIELD01 EQ ICON_LED_GREEN
            AND <LT_MNU_DATA>-FIELD02 NE ICON_LED_GREEN
            AND <LT_MNU_DATA>-FIELD03 EQ ICON_LED_RED.

            LT_HEADDATA-MATERIAL = <LT_MNU_DATA>-FIELD05.
            LT_PLANTDATA-PUR_STATUS = LC_Z2.
            LT_PLANTDATAX-PUR_STATUS = ABAP_TRUE.
            LT_PLANTDATA-PLANT = LF_PLANT.
            LT_PLANTDATAX-PLANT = LF_PLANT.

            READ TABLE LT_MNU_A ASSIGNING FIELD-SYMBOL(<LT_MNU_B_DATA>)
              WITH KEY MATNR = <LT_MNU_DATA>-FIELD05.

            IF SY-SUBRC EQ 0.
              IF <LT_MNU_B_DATA>-MMSTA EQ 'Z1'.

*                BREAK-POINT.

                CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
                  EXPORTING
                    HEADDATA   = LT_HEADDATA
                    PLANTDATA  = LT_PLANTDATA
                    PLANTDATAX = LT_PLANTDATAX
                  IMPORTING
                    RETURN     = LT_RETURN.

*<<<BEG LOCAL ADDONS HC#107 - FIX MESSAGE ERROR from BAPI on 29.10.2024\
                MESSAGE S216(ZRFPP01) WITH LT_HEADDATA-MATERIAL. " WITH <LT_MNU_DATA>-FIELD05.
*                MESSAGE |Material { LT_HEADDATA-MATERIAL } ; { LT_RETURN-MESSAGE }| TYPE LT_RETURN-TYPE.

**                LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*                MESSAGE LT_RETURN-MESSAGE TYPE LT_RETURN-TYPE.
**                ENDLOOP.

*>>>END LOCAL ADDONS HC#107 - FIX MESSAGE ERROR from BAPI on 29.10.2024

*<<<BEG LOCAL ADDONS HC#107 - Remove Error Message for Mat Status other than Z1 on 29.10.2024
              ELSEIF <LT_MNU_B_DATA>-MMSTA EQ ''.
                MESSAGE S027(ZRFPP01) WITH <LT_MNU_DATA>-FIELD05.
*>>>END LOCAL ADDONS HC#107 - Remove Error Message for Mat Status other than Z1 on 29.10.2024

              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      MESSAGE S023(ZRFPP01).
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE S023(ZRFPP01).
    EXIT.
   ENDIF.

*>>>END LOCAL ADDONS CR018: Call Dynamic Program by Plant on 21.08.2024

*" ------ Legacy Code ------
*
*  IF <LT_TEST_DATA> IS ASSIGNED.
*    LOOP AT <LT_TEST_DATA> ASSIGNING <LT_TEST_MAP>.
*      APPEND <LT_TEST_MAP> TO LT_TEST_MNU.
*    ENDLOOP.
*
*    IF LT_TEST_MNU IS NOT INITIAL.
*      DELETE LT_TEST_MNU WHERE FIELD03 EQ ICON_LED_GREEN.
*
*      IF LT_TEST_MNU IS INITIAL.
*        MESSAGE S023(ZRFPP01).
*        EXIT.
*      ELSE.
*        LOOP AT LT_TEST_MNU ASSIGNING FIELD-SYMBOL(<LFS_MATNR_MNU>).
*          LS_MATNR-MATNR = <LFS_MATNR_MNU>-FIELD05.
*          APPEND LS_MATNR TO LT_MATNR.
*        ENDLOOP.
*
*        SORT LT_MATNR.
*        DELETE ADJACENT DUPLICATES FROM LT_MATNR.
*        SELECT MATNR,
*               MMSTA
*          FROM MARC
*          INTO TABLE @DATA(LT_MNU_A)
*          FOR ALL ENTRIES IN @LT_MATNR
*        WHERE MATNR EQ @LT_MATNR-MATNR.
*
*        LOOP AT LT_TEST_MNU
*          ASSIGNING FIELD-SYMBOL(<LT_MNU_DATA>).
*          IF <LT_MNU_DATA>-FIELD01 EQ ICON_LED_GREEN
*            AND <LT_MNU_DATA>-FIELD02 EQ ICON_LED_GREEN
*            AND <LT_MNU_DATA>-FIELD03 EQ ICON_LED_RED.
*
*            LT_HEADDATA-MATERIAL = <LT_MNU_DATA>-FIELD05.
*            LT_PLANTDATA-PUR_STATUS = LC_Z3.
*            LT_PLANTDATAX-PUR_STATUS = ABAP_TRUE.
*            LT_PLANTDATA-PLANT = LF_PLANT.
*            LT_PLANTDATAX-PLANT = LF_PLANT.
*
*            READ TABLE LT_MNU_A ASSIGNING FIELD-SYMBOL(<LT_MNU_A_DATA>)
*              WITH KEY MATNR = <LT_MNU_DATA>-FIELD05.
*
*            IF SY-SUBRC EQ 0.
*              IF <LT_MNU_A_DATA>-MMSTA EQ LC_Z1
*                OR <LT_MNU_A_DATA>-MMSTA EQ LC_Z2.
*                CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
*                  EXPORTING
*                    HEADDATA   = LT_HEADDATA
*                    PLANTDATA  = LT_PLANTDATA
*                    PLANTDATAX = LT_PLANTDATAX
*                  IMPORTING
*                    RETURN     = LT_RETURN.
*
**                LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>).
*                MESSAGE LT_RETURN-MESSAGE TYPE LT_RETURN-TYPE.
**                ENDLOOP.
*              ELSE.
*                MESSAGE S027(ZRFPP01) WITH <LT_MNU_DATA>-FIELD05.
*              ENDIF.
*            ENDIF.
*          ELSEIF <LT_MNU_DATA>-FIELD01 EQ ICON_LED_GREEN
*            AND <LT_MNU_DATA>-FIELD02 NE ICON_LED_GREEN
*            AND <LT_MNU_DATA>-FIELD03 EQ ICON_LED_RED.
*
*            LT_HEADDATA-MATERIAL = <LT_MNU_DATA>-FIELD05.
*            LT_PLANTDATA-PUR_STATUS = LC_Z2.
*            LT_PLANTDATAX-PUR_STATUS = ABAP_TRUE.
*            LT_PLANTDATA-PLANT = LF_PLANT.
*            LT_PLANTDATAX-PLANT = LF_PLANT.
*
*            READ TABLE LT_MNU_A ASSIGNING FIELD-SYMBOL(<LT_MNU_B_DATA>)
*              WITH KEY MATNR = <LT_MNU_DATA>-FIELD05.
*
*            IF SY-SUBRC EQ 0.
*              IF <LT_MNU_B_DATA>-MMSTA EQ 'Z1'.
*                CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
*                  EXPORTING
*                    HEADDATA   = LT_HEADDATA
*                    PLANTDATA  = LT_PLANTDATA
*                    PLANTDATAX = LT_PLANTDATAX
*                  IMPORTING
*                    RETURN     = LT_RETURN.
*
**                LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*                MESSAGE LT_RETURN-MESSAGE TYPE LT_RETURN-TYPE.
**                ENDLOOP.
*              ELSE.
*                MESSAGE S027(ZRFPP01) WITH <LT_MNU_DATA>-FIELD05.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*    ELSE.
*      MESSAGE S023(ZRFPP01).
*      EXIT.
*    ENDIF.
*  ELSE.
*    MESSAGE S023(ZRFPP01).
*    EXIT.
*  ENDIF.
*
*" ------ Legacy Code ------
