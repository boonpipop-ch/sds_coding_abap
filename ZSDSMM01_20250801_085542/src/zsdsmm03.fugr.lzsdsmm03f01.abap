*----------------------------------------------------------------------*
***INCLUDE LZSDSMM03F01.
*----------------------------------------------------------------------*
CLASS LCL_DATA DEFINITION.
  PUBLIC SECTION.
    METHODS :
      CONSTRUCTOR,
      GET_DATA,
      GET_OBJK,
      GET_SER01,
      GET_VBFA,
      GET_VBAK,
      GET_VBRK,
      GET_KNA1,
      GET_RESULT CHANGING C_DATA TYPE ZSDSMMS001,
      READ_SER01,
      READ_VBFA,
      READ_VBAK,
      READ_VBRK,
      READ_KNA1.
ENDCLASS.                    "LCL_DATA DEFINITION
*----------------------------------------------------------------------*
*       CLASS LCL_DATA IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_DATA IMPLEMENTATION.
  METHOD CONSTRUCTOR.

  ENDMETHOD.                    "CONSTRUCTOR
  METHOD GET_DATA.
    GET_OBJK( ).
    GET_SER01( ).
    GET_VBFA( ).
    GET_VBRK( ).
    GET_VBAK( ).
    GET_KNA1( ).
  ENDMETHOD.                    "get_Data
  METHOD GET_OBJK.
    CONSTANTS LC_TASER TYPE C LENGTH 5 VALUE 'SER01'.
    SELECT OBKNR
           SERNR
           MATNR
      FROM OBJK
      INTO TABLE GT_OBJK
      WHERE SERNR EQ GV_SERNR
        AND MATNR EQ GV_MATNR
        AND TASER EQ LC_TASER.
    SORT GT_OBJK BY OBKNR ASCENDING.
  ENDMETHOD.                    "get_objk
  METHOD GET_SER01.
    IF GT_OBJK IS NOT INITIAL.
      SELECT OBKNR
             LIEF_NR
             POSNR
             VBTYP
        FROM SER01
        INTO TABLE GT_SER01
        FOR ALL ENTRIES IN GT_OBJK
        WHERE OBKNR EQ GT_OBJK-OBKNR .
    ENDIF.
  ENDMETHOD.                                                "GET_SER01
  METHOD GET_VBFA.
    CONSTANTS LC_TYPE TYPE C VALUE 'M'.
    IF GT_SER01 IS NOT INITIAL.
      SELECT VBELV
             POSNV
             VBELN
             POSNN
        FROM VBFA
        INTO TABLE GT_VBFA
        FOR ALL ENTRIES IN GT_SER01
        WHERE VBELV   EQ GT_SER01-LIEF_NR
          AND POSNV   EQ GT_SER01-POSNR
          AND VBTYP_N EQ LC_TYPE.
    ENDIF.
  ENDMETHOD.                    "GET_VBFA
  METHOD GET_VBRK.
    IF GT_VBFA IS NOT INITIAL.
      SELECT VBRK~VBELN
             VBRK~FKDAT
             VBRK~ZUONR
             VBRP~POSNR
             VBRP~PRCTR
        FROM VBRK
        INNER JOIN VBRP ON VBRK~VBELN EQ VBRP~VBELN
        INTO TABLE GT_VBRK
        FOR ALL ENTRIES IN GT_VBFA
        WHERE VBRP~VBELN EQ GT_VBFA-VBELN
          AND VBRP~POSNR EQ GT_VBFA-POSNN.
    ENDIF.
  ENDMETHOD.                    "get_vbrk
  METHOD GET_VBAK.
    IF GT_VBRK IS NOT INITIAL.
      SELECT VBELN
             KUNNR
             VKBUR
             VKGRP
        FROM VBAK
        INTO TABLE GT_VBAK
        FOR ALL ENTRIES IN GT_VBRK
        WHERE VBELN = GT_VBRK-ZUONR+0(10).
    ENDIF.
  ENDMETHOD.                    "GET_VBAK
  METHOD  GET_KNA1.
    IF GT_VBAK IS NOT INITIAL.
      SELECT KNA1~KUNNR
             ADRC~NAME1
             ADRC~NAME2
             ADRC~NAME3
             ADRC~NAME4
        FROM KNA1
        INNER JOIN ADRC ON KNA1~ADRNR  EQ ADRC~ADDRNUMBER AND
                           ADRC~NATION EQ 'I'
        INTO TABLE GT_KNA1
        FOR ALL ENTRIES IN GT_VBAK
        WHERE KNA1~KUNNR EQ GT_VBAK-KUNNR.
    ENDIF.
  ENDMETHOD.                                                "GET_KNA1
  METHOD GET_RESULT.

    LOOP AT GT_OBJK INTO GS_OBJK .
      READ_SER01( ).
      IF GS_SER01 IS INITIAL.
        CONTINUE.
      ENDIF.
      READ_VBFA( ).
      READ_VBRK( ).
      READ_VBAK( ).
      READ_KNA1( ).

      C_DATA-MATNR = GS_OBJK-MATNR.
      C_DATA-SERNR = GS_OBJK-SERNR.
      C_DATA-KUNNR = GS_KNA1-KUNNR.
      CONCATENATE GS_KNA1-NAME1 GS_KNA1-NAME2 GS_KNA1-NAME3 GS_KNA1-NAME4
             INTO C_DATA-NAME1 SEPARATED BY SPACE.
      C_DATA-VKBUR = GS_VBAK-VKBUR.
      C_DATA-VKGRP = GS_VBAK-VKGRP.
      C_DATA-FKDAT = GS_VBRK-FKDAT.
      C_DATA-VBELN = GS_VBRK-VBELN.

      CLEAR : GS_OBJK,GS_SER01,
              GS_VBFA,GS_VBRK,GS_VBAK,GS_KNA1.
    ENDLOOP.

  ENDMETHOD.                    "GET_RESULT
  METHOD READ_SER01.
    READ TABLE GT_SER01 INTO GS_SER01
    WITH KEY OBKNR = GS_OBJK-OBKNR
             VBTYP = 'J'.
  ENDMETHOD.                                                "READ_SER01
  METHOD READ_VBFA.
    READ TABLE GT_VBFA INTO GS_VBFA
    WITH KEY VBELV = GS_SER01-LIEF_NR
             POSNV = GS_SER01-POSNR .
  ENDMETHOD.                    "READ_VBFA
  METHOD READ_VBRK.
    READ TABLE GT_VBRK INTO GS_VBRK
    WITH KEY VBELN = GS_VBFA-VBELN
             POSNR = GS_VBFA-POSNN.
  ENDMETHOD.                    "READ_VBAK
  METHOD READ_VBAK.
    READ TABLE GT_VBAK INTO GS_VBAK
    WITH KEY VBELN = GS_VBRK-ZUONR.
  ENDMETHOD.                    "READ_VBRK
  METHOD READ_KNA1.
    READ TABLE GT_KNA1 INTO GS_KNA1
    WITH KEY KUNNR = GS_VBAK-KUNNR.
  ENDMETHOD.                                                "READ_KNA1
ENDCLASS.                    "LCL_DATA IMPLEMENTATION
