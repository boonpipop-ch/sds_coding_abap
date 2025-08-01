class ZCL_SDSSD_WL_STATUS_SRV definition
  public
  inheriting from ZCL_SDSCA_REST_SERVICE
  final
  create public .

public section.

  methods PROCESS_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SDSSD_WL_STATUS_SRV IMPLEMENTATION.


  METHOD PROCESS_DATA.
*CALL METHOD SUPER->PROCESS_DATA
*  EXPORTING
*    IREF_REQUEST_DATA  =
**  IMPORTING
**    EREF_RESPONSE_DATA =
**    EF_STATUS          =
**    EF_MESSAGE         =
**    EF_HTTP_ERROR      =
*    .

    DATA: LS_REQUEST    TYPE ZSDSCMS018,
          LV_CHANGE_FLG TYPE C,
          LV_WL_ID      TYPE ZSDSDE_WL_ID.

    FIELD-SYMBOLS: <L_RESPONSE> TYPE ZSDSCMS018.

* Initialize Output
    CLEAR: EF_STATUS,
           EF_MESSAGE,
           EF_HTTP_ERROR.

    LS_REQUEST = IREF_REQUEST_DATA->*.

    ASSIGN EREF_RESPONSE_DATA->* TO <L_RESPONSE>.
    IF SY-SUBRC NE 0.
*   Critical error
      RETURN.
    ENDIF.

    <L_RESPONSE> = LS_REQUEST.
*Update Warranty Letter Status
*1  Created
*2  Send to Approve
*3  Approved
*4  Reject
*5  Cancelled
    LV_WL_ID = <L_RESPONSE>-WARRANTYLETTERID.
    SELECT SINGLE  WL_ID,STATUS
      FROM ZSDSCMT005
     WHERE WL_ID = @LV_WL_ID
      INTO @DATA(LS_ZSDSCMT005).
    IF SY-SUBRC = 0.
*      R = Rejected, A = Approved, C = Canceled
      CASE <L_RESPONSE>-STATUS.
        WHEN 'A'.
          IF LS_ZSDSCMT005-STATUS <> '3'.
            LS_ZSDSCMT005-STATUS = '3'.
            LV_CHANGE_FLG = 'X'.
          ENDIF.
        WHEN 'R'.
          IF LS_ZSDSCMT005-STATUS <> '4'.
            LS_ZSDSCMT005-STATUS = '4'.
            LV_CHANGE_FLG = 'X'.
          ENDIF.
        WHEN 'C'.
          IF LS_ZSDSCMT005-STATUS <> '5'.
            LS_ZSDSCMT005-STATUS = '5'.
            LV_CHANGE_FLG = 'X'.
          ENDIF.
      ENDCASE.
    ENDIF.

    IF LV_CHANGE_FLG = 'X'.
      UPDATE ZSDSCMT005
        SET STATUS = @LS_ZSDSCMT005-STATUS,
            AEDAT  = @SY-DATUM,
            AETIM  = @SY-UZEIT,
            AENAM  = @SY-UNAME
      WHERE WL_ID = @LV_WL_ID.
      IF SY-SUBRC = 0.
        SELECT SINGLE *
          FROM ZSDSCMT008
         WHERE WL_ID = @LV_WL_ID
          INTO @DATA(LS_ZSDSCMT008).
        IF SY-SUBRC = 0.
          UPDATE ZSDSCMT008
             SET APPROVE_NAME  = @<L_RESPONSE>-APPROVENAME,
                 EMP_POSITION  = @<L_RESPONSE>-POSITION
           WHERE WL_ID = @LS_ZSDSCMT008-WL_ID
             AND SPRAS = @LS_ZSDSCMT008-SPRAS.

          "Status is updated successfully
          <L_RESPONSE>-MESSAGETYPE = 'S'.
          <L_RESPONSE>-MESSAGETEXT = TEXT-S01.
        ENDIF.
      ELSE.
        "Update error: status not saved
        <L_RESPONSE>-MESSAGETYPE = 'E'.
        <L_RESPONSE>-MESSAGETEXT = TEXT-E02.
      ENDIF.
    ELSE.
      "Existing and status from K2 have same status.No status changed.
      <L_RESPONSE>-MESSAGETYPE = 'E'.
      <L_RESPONSE>-MESSAGETEXT = TEXT-E01.
    ENDIF.

* Assign Log status from Response structure
    EF_STATUS  = <L_RESPONSE>-MESSAGETYPE.
    EF_MESSAGE = <L_RESPONSE>-MESSAGETEXT.

  ENDMETHOD.
ENDCLASS.
