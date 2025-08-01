*-----------------------------------------------------------------------
*  Program ID         : LZSDSV_REST_INTFF01
*  Creation Date      : 23.05.2024
*  Author             : Wuthichai L.(Eviden)
*  Add-on ID          : N/A
*  Description        : This is include program used in table maintenance
*                       of view ZSDSV_REST_INTF
*                       - Q: How to use?
*                       - A: Set Event 01 to perform F_PROCESS_BEFORE_SAVE
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  DD.MM.YYYY  TR no.      ABAP Name   Detail
*-----------------------------------------------------------------------
FORM F_PROCESS_BEFORE_SAVE.

* Encode Password
  PERFORM F_ENCODING_PASSWD.
* Set Time Stamp Fields
  PERFORM SET_TIMESTAMP.

ENDFORM.

FORM F_ENCODING_PASSWD.

  DATA:
    LS_DATA  TYPE ZSDSV_REST_INTF,
    LV_PASS  TYPE STRING,
    LV_ENCOD TYPE STRING.


  FIELD-SYMBOLS:
    <LFS_FIELD>  TYPE  ANY.


  LOOP AT TOTAL.

    CHECK <ACTION> EQ 'N' OR
          <ACTION> EQ 'U' .

    MOVE <VIM_TOTAL_STRUC> TO LS_DATA.

*   Only Outbound
    CHECK LS_DATA-INTTY EQ 'O'.

*   Only Password Entered
    CHECK LS_DATA-PASSWD IS NOT INITIAL.

*   For new or password changed entry only
    IF <ACTION> NE 'N'.
*     Read Existing Data
      SELECT SINGLE *
        INTO @DATA(LS_OLD)
        FROM ZSDSCAC005
       WHERE INTFNO EQ @LS_DATA-INTFNO.
      IF SY-SUBRC NE 0.
        CLEAR LS_OLD.
      ENDIF.
*     Check password changed?
      CHECK LS_OLD-PASSWD NE LS_DATA-PASSWD.
    ENDIF.

*   Encrypt Password
    LV_PASS = LS_DATA-PASSWD.
    CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
      EXPORTING
        UNENCODED = LV_PASS
      RECEIVING
        ENCODED   = LV_ENCOD.
    IF SY-SUBRC IS INITIAL.
*     Update Password Field
      ASSIGN COMPONENT 'PASSWD' OF STRUCTURE <VIM_TOTAL_STRUC> TO <LFS_FIELD>.
      IF SY-SUBRC NE 0.
        CONTINUE.
      ENDIF.
      CLEAR <LFS_FIELD>.
      MOVE  LV_ENCOD  TO <LFS_FIELD>.
    ENDIF.

*   Update Entry
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      EXTRACT = TOTAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.

    IF TOTAL IS NOT INITIAL.
      MODIFY TOTAL.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  Module ZCHANGE_FIELD_STATUS OUTPUT
*----------------------------------------------------------------------*
*  Change Field Status
*----------------------------------------------------------------------*
MODULE ZCHANGE_FIELD_STATUS OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSDSV_REST_INTF-PASSWD'.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.
