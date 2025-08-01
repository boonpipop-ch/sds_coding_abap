class ZCL_IM_SDS_ACC_DOCUMENT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ACC_DOCUMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_SDS_ACC_DOCUMENT IMPLEMENTATION.


  METHOD IF_EX_ACC_DOCUMENT~CHANGE.

    DATA : LV_CHECK_SDS LIKE ABAP_TRUE.
*--------------------------------------------------------------------*
* check Client SDS
*--------------------------------------------------------------------*
    LV_CHECK_SDS = LCL_DATA=>CHECK_SDS( SY-MANDT ).
*--------------------------------------------------------------------*
* Start Enhance SDS
*--------------------------------------------------------------------*
    DATA: WA_EXTENSION   TYPE BAPIPAREX,
          EXT_VALUE(960) TYPE C,
          WA_ACCIT       TYPE ACCIT,
          L_REF          TYPE REF TO DATA.

    FIELD-SYMBOLS: <L_STRUC> TYPE ANY,
                   <L_FIELD> TYPE ANY,
                   <L_BRNCH> TYPE ANY,
                   <L_BUPLA> TYPE ANY.

    SORT C_EXTENSION2 BY STRUCTURE.

    DATA LV_TABIX TYPE SY-TABIX.
    IF LV_CHECK_SDS EQ ABAP_TRUE.
      LOOP AT C_EXTENSION2 INTO WA_EXTENSION.
        AT NEW STRUCTURE.
          CREATE DATA L_REF TYPE (WA_EXTENSION-STRUCTURE).
          ASSIGN L_REF->* TO <L_STRUC>.
        ENDAT.
        CONCATENATE WA_EXTENSION-VALUEPART1 WA_EXTENSION-VALUEPART2
                    WA_EXTENSION-VALUEPART3 WA_EXTENSION-VALUEPART4
               INTO EXT_VALUE.
        MOVE EXT_VALUE TO <L_STRUC>.
        ASSIGN COMPONENT 'POSNR' OF STRUCTURE <L_STRUC> TO <L_FIELD>.
        READ TABLE C_ACCIT WITH KEY POSNR = <L_FIELD>
              INTO WA_ACCIT.
        IF SY-SUBRC IS INITIAL.
          LV_TABIX = SY-TABIX.
          MOVE-CORRESPONDING <L_STRUC> TO WA_ACCIT.
          IF WA_EXTENSION-STRUCTURE = 'ZSDSFIS007'.
            ASSIGN COMPONENT 'BRNCH' OF STRUCTURE <L_STRUC> TO <L_BRNCH>.
            WA_ACCIT-BRNCH = <L_BRNCH>.

            ASSIGN COMPONENT 'BUPLA' OF STRUCTURE <L_STRUC> TO <L_BUPLA>.
            WA_ACCIT-BUPLA = <L_BUPLA>.

          ENDIF.
          MODIFY C_ACCIT FROM WA_ACCIT INDEX LV_TABIX.
        ENDIF.
      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------*
* End Enhance SDS
*--------------------------------------------------------------------*

  ENDMETHOD.


  method IF_EX_ACC_DOCUMENT~FILL_ACCIT.
  endmethod.
ENDCLASS.
