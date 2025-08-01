*----------------------------------------------------------------------*
*   INCLUDE LFQSRTOOLF05                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SMARTFORM_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_GLOB  text
*      -->P_C_COMPCD  text
*      -->P_C_PARTNER  text
*      -->P_C_FIDOC  text
*----------------------------------------------------------------------*
CONSTANTS: C_MESSAGECLASS TYPE ARBGB VALUE 'ID_WT'.

*---------------------------------------------------------------------*
*       FORM SMARTFORM_CALL                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_INDICATOR                                                   *
*  -->  P_GLOB                                                        *
*  -->  P_COMPCD                                                      *
*  -->  P_PARTNER                                                     *
*  -->  P_FIDOC                                                       *
*  -->  P_ERROR                                                       *
*  -->  P_EXCEPTION                                                   *
*---------------------------------------------------------------------*
FORM SMARTFORM_CALL USING    P_INDICATOR TYPE C
                             VALUE(P_GLOB) TYPE IDWTGLOB
                             VALUE(P_COMPCD) TYPE TY_IDWTCOMPCD
                             VALUE(P_PARTNER) TYPE TY_IDWTPARTNER
                             VALUE(P_FIDOC) TYPE TY_IDWTFIDOC
                             VALUE(P_ERROR) TYPE TY_IDWTERROR
                    CHANGING P_EXCEPTION.

  DATA: H_FORMNAME TYPE TDSFNAME,
        H_FMNAME TYPE RS38L_FNAM,
        H_INTERFACEALL TYPE IDWTINTALL,
        H_INTERFACEFORM TYPE IDWTINTFORM,
        IT_ERRORLIST TYPE TSFERROR,
        H_OUT_PARAMETERS TYPE PRI_PARAMS,
        H_OUTPUT_OPTIONS TYPE SSFCOMPOP,
        H_COVER_TEXT TYPE SYPRTXT.
  DATA: WA_WT_NATXT TYPE NATXT.

*----> Begin Insert by Teerasithi.
  DATA: P_COMPCD_TMP  TYPE TY_IDWTCOMPCD,
        P_PARTNER_TMP TYPE TY_IDWTPARTNER,
        P_FIDOC_TMP   TYPE TY_IDWTFIDOC,
        P_FIDOC_ARC   TYPE TY_IDWTFIDOC.

  DATA: LWA_CONTROL TYPE SSFCTRLOP,
        LWA_OPTIONS TYPE ITCPO,
        LWA_OPT_TMP TYPE SSFCOMPOP,
        LWA_BKPF    TYPE BKPF,
        TOA_DARA    TYPE TOA_DARA,
        LWA_LAYOUT  TYPE ZSDSCAC007,
        LWA_ARC_P   TYPE ARC_PARAMS,
        LWA_FIDOC   TYPE IDWTFIDOC,
        LWA_OUTPUT  TYPE SSFCRESCL.
*<---- End Insert by Teerasithi.

* decide, which form is called.
  CASE P_INDICATOR.
    WHEN '1'.
      H_FORMNAME = P_GLOB-WT_OG_FORM_NM1.
      H_COVER_TEXT = P_GLOB-WT_OG_FORM_TI1.
* overwrite some print parameters with user entry
      CALL FUNCTION 'GET_PRINT_PARAMETERS'
        EXPORTING
          COPIES                       = P_GLOB-WT_SC_FORM_CP1
*         COVER_PAGE                   = C_CHAR_UNKNOWN
*         DATA_SET                     = C_CHAR_UNKNOWN
*         DEPARTMENT                   = C_CHAR_UNKNOWN
          DESTINATION                  = P_GLOB-WT_SC_FORM_PR1
*         EXPIRATION                   = C_NUM1_UNKNOWN
          IMMEDIATELY                  = P_GLOB-WT_SC_FORM_IM1
          LIST_TEXT                    = H_COVER_TEXT
          NO_DIALOG                    = 'X'
       IMPORTING
*         OUT_ARCHIVE_PARAMETERS       =
          OUT_PARAMETERS               = H_OUT_PARAMETERS
*         VALID                        =
       EXCEPTIONS
         ARCHIVE_INFO_NOT_FOUND       = 1
         INVALID_PRINT_PARAMS         = 2
         INVALID_ARCHIVE_PARAMS       = 3
         OTHERS                       = 4
                .
* fill the error table or raise error.
      IF SY-SUBRC <> 0.
        IF P_GLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE E035(ID_WT) WITH P_GLOB-WT_OG_FORM_TI1
          RAISING ERROR_FORM.
        ELSE.
          MESSAGE E035(ID_WT) WITH P_GLOB-WT_OG_FORM_TI1
          INTO WA_WT_NATXT.
          PERFORM FILL_IDWTERROR TABLES P_ERROR[]
                          USING  '035' WA_WT_NATXT.
        ENDIF.
      ENDIF.
      H_OUT_PARAMETERS-PRIMM = P_GLOB-WT_SC_FORM_IM1.

    WHEN '2'.
      H_FORMNAME = P_GLOB-WT_OG_FORM_NM2.
      H_COVER_TEXT = P_GLOB-WT_OG_FORM_TI2.
* overwrite some print parameters with user entry
      CALL FUNCTION 'GET_PRINT_PARAMETERS'
         EXPORTING
           COPIES                       = P_GLOB-WT_SC_FORM_CP2
*          COVER_PAGE                   = C_CHAR_UNKNOWN
*          DATA_SET                     = C_CHAR_UNKNOWN
*          DEPARTMENT                   = C_CHAR_UNKNOWN
           DESTINATION                  = P_GLOB-WT_SC_FORM_PR2
*          EXPIRATION                   = C_NUM1_UNKNOWN
           IMMEDIATELY                  = P_GLOB-WT_SC_FORM_IM2
           LIST_TEXT                    = H_COVER_TEXT
           NO_DIALOG                    = 'X'

        IMPORTING
*          OUT_ARCHIVE_PARAMETERS       =
           OUT_PARAMETERS               = H_OUT_PARAMETERS
*          VALID                        =
        EXCEPTIONS
          ARCHIVE_INFO_NOT_FOUND       = 1
          INVALID_PRINT_PARAMS         = 2
          INVALID_ARCHIVE_PARAMS       = 3
          OTHERS                       = 4
                 .
      IF SY-SUBRC <> 0.
        IF P_GLOB-WT_SC_LIST_ACT4 IS INITIAL.
          MESSAGE E035(ID_WT) WITH P_GLOB-WT_OG_FORM_TI2
          RAISING ERROR_FORM.
        ELSE.
          MESSAGE E035(ID_WT) WITH P_GLOB-WT_OG_FORM_TI2
          INTO WA_WT_NATXT.
          PERFORM FILL_IDWTERROR TABLES P_ERROR[]
                          USING  '035' WA_WT_NATXT.
        ENDIF.
      ENDIF.
      H_OUT_PARAMETERS-PRIMM = P_GLOB-WT_SC_FORM_IM2.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

* fill the interface structure for all tools
  MOVE-CORRESPONDING P_GLOB TO H_INTERFACEALL.
  MOVE-CORRESPONDING P_GLOB TO H_INTERFACEFORM.
* fill the output options with the print parameters
  H_OUTPUT_OPTIONS-TDCOPIES   = H_OUT_PARAMETERS-PRCOP.   "copies
  H_OUTPUT_OPTIONS-TDDEST     = H_OUT_PARAMETERS-PDEST.   "output device
  H_OUTPUT_OPTIONS-TDCOVTITLE = H_OUT_PARAMETERS-PRTXT.   "cover text
  H_OUTPUT_OPTIONS-TDAUTORITY = H_OUT_PARAMETERS-PRBER.   "authorization
  H_OUTPUT_OPTIONS-TDIMMED    = H_OUT_PARAMETERS-PRIMM.   "print imm.
  H_OUTPUT_OPTIONS-TDDELETE   = H_OUT_PARAMETERS-PRREL.   "delete spool
  H_OUTPUT_OPTIONS-TDNEWID    = H_OUT_PARAMETERS-PRNEW.   "new spool
  H_OUTPUT_OPTIONS-TDDIVISION = H_OUT_PARAMETERS-PRABT.   "department
  H_OUTPUT_OPTIONS-TDRECEIVER = H_OUT_PARAMETERS-PRREC.   "recipient
  H_OUTPUT_OPTIONS-TDCOVER    = H_OUT_PARAMETERS-PRSAP.   "cover page
  H_OUTPUT_OPTIONS-TDLIFETIME = H_OUT_PARAMETERS-PEXPI.   "sp.retention
  H_OUTPUT_OPTIONS-TDARMOD    = H_OUT_PARAMETERS-ARMOD.   "arch. mode

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            FORMNAME           = H_FORMNAME
*           VARIANT            = ' '
*           DIRECT_CALL        = ' '
       IMPORTING
            FM_NAME            = H_FMNAME
       EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3
            .
  IF SY-SUBRC <> 0.
    WA_FORMNAME = H_FORMNAME.
    PERFORM EXCEPTION_SET TABLES   P_ERROR[]
                          USING    '1'
                                   P_GLOB
                          CHANGING P_EXCEPTION.
    EXIT.
  ENDIF.

*----> Begin Insert by Teerasithi.
  P_COMPCD_TMP[]  = P_COMPCD[].
  P_PARTNER_TMP[] = P_PARTNER[].
  P_FIDOC_TMP[]   = P_FIDOC[].

  H_OUTPUT_OPTIONS-TDIMMED = 'X'.

  DO.
*<---- End Insert by Teerasithi.

    CALL FUNCTION H_FMNAME
      EXPORTING
*       archive_index        =
*       archive_parameters   =
*       control_parameters   =
*       mail_appl_obj        =
*       mail_recipient       =
*       mail_sender          =
        OUTPUT_OPTIONS       = H_OUTPUT_OPTIONS
        USER_SETTINGS        = ' '
        WTINTERFACEALL       = H_INTERFACEALL
        WTINTERFACEFORM      = H_INTERFACEFORM
      TABLES
        WTCOMPCD             = P_COMPCD
        WTPARTNER            = P_PARTNER
        WTFIDOC              = P_FIDOC
*     importing
*        document_output_info =
*        job_output_info      =
*        job_output_options   =
      EXCEPTIONS
        FORMATTING_ERROR     = 1
        INTERNAL_ERROR       = 2
        SEND_ERROR           = 3
        USER_CANCELED        = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0.
* get the detailed error list for smart forms
      WA_FORMNAME = H_FORMNAME.
      CALL FUNCTION 'SSF_READ_ERRORS'
        IMPORTING
          ERRORTAB = IT_ERRORLIST.

*   error handling
      PERFORM EXCEPTION_SET  TABLES P_ERROR[]
                             USING  '1'
                                    P_GLOB
                           CHANGING P_EXCEPTION.

      EXIT. "Insert by Teerasithi.

*----> Begin Insert by Teerasithi.
    ELSE.

      BREAK TEERASITHI.

      IF SY-UCOMM EQ 'PRNT' AND
         ( P_GLOB-WT_OUTPUT_CUST NE 'REP' AND  "T41K920280
           P_GLOB-WT_OUTPUT_CUST NE 'REC' ) . "T41K926696,T41K926698

        LWA_OPT_TMP = H_OUTPUT_OPTIONS.
        SORT P_FIDOC_TMP BY BUKRS ASCENDING
                            GJAHR ASCENDING
                            BELNR ASCENDING.
        DELETE ADJACENT DUPLICATES FROM P_FIDOC_TMP COMPARING BUKRS GJAHR BELNR.

        LOOP AT P_FIDOC_TMP INTO LWA_FIDOC.

          REFRESH: P_COMPCD, P_PARTNER, P_FIDOC_ARC.

          CLEAR: LWA_BKPF, TOA_DARA, LWA_LAYOUT,
                 LWA_OPTIONS, LWA_OUTPUT.

          MOVE-CORRESPONDING: LWA_FIDOC   TO LWA_BKPF,
                              LWA_OPT_TMP TO LWA_OPTIONS.


          "Add by Wantanee 20160428 T41K922610
           IF P_INDICATOR = '2'.
              LWA_BKPF-BLART = 'W2'.
           ELSE.
             CLEAR: LWA_BKPF-BLART.
           ENDIF.
          "End Add by Wantanee 20160428 T41K922610

*          PERFORM GET_OVERLAY_FORM USING    LWA_BKPF
*                                            SY-CPROG
*                                   CHANGING TOA_DARA
*                                            LWA_LAYOUT.

*          PERFORM SET_ARCHIV_PARAM USING  LWA_BKPF
*                                          LWA_LAYOUT
*                                 CHANGING TOA_DARA
*                                          LWA_OPTIONS
*                                          LWA_ARC_P.

          MOVE-CORRESPONDING LWA_OPTIONS TO H_OUTPUT_OPTIONS.
          LWA_CONTROL-GETOTF    = 'X'.
          LWA_CONTROL-NO_DIALOG = 'X'.
          P_COMPCD[]            = P_COMPCD_TMP[].
          P_PARTNER[]           = P_PARTNER_TMP[].
          P_FIDOC_ARC[]         = P_FIDOC[].

          DELETE P_FIDOC_ARC WHERE BUKRS NE LWA_FIDOC-BUKRS OR
                                   BELNR NE LWA_FIDOC-BELNR OR
                                   GJAHR NE LWA_FIDOC-GJAHR.

*          CALL FUNCTION H_FMNAME
*            EXPORTING
*              ARCHIVE_INDEX        = TOA_DARA
*              ARCHIVE_PARAMETERS   = LWA_ARC_P
*              CONTROL_PARAMETERS   = LWA_CONTROL
*              OUTPUT_OPTIONS       = H_OUTPUT_OPTIONS
*              USER_SETTINGS        = ' '
*              WTINTERFACEALL       = H_INTERFACEALL
*              WTINTERFACEFORM      = H_INTERFACEFORM
*            IMPORTING
*              JOB_OUTPUT_INFO      = LWA_OUTPUT
*            TABLES
*              WTCOMPCD             = P_COMPCD
*              WTPARTNER            = P_PARTNER
*              WTFIDOC              = P_FIDOC_ARC
*            EXCEPTIONS
*              FORMATTING_ERROR     = 1
*              INTERNAL_ERROR       = 2
*              SEND_ERROR           = 3
*              USER_CANCELED        = 4
*              OTHERS               = 5.
*          IF SY-SUBRC EQ 0 AND
*             LWA_OUTPUT-OTFDATA IS NOT INITIAL.
*
*            LWA_ARC_P-ARCUSER  = SY-UNAME.
*            LWA_ARC_P-PRINTER  = 'PDF'.
*
*            CALL FUNCTION 'CONVERT_OTF_AND_ARCHIVE'
*              EXPORTING
*                ARC_P                    = LWA_ARC_P
*                ARC_I                    = TOA_DARA
*                FORMAT                   = 'PDF'
*              TABLES
*                OTF                      = LWA_OUTPUT-OTFDATA
*              EXCEPTIONS
*                ERROR_ARCHIV             = 1
*                ERROR_COMMUNICATIONTABLE = 2
*                ERROR_CONNECTIONTABLE    = 3
*                ERROR_KERNEL             = 4
*                ERROR_PARAMETER          = 5
*                ERROR_FORMAT             = 6
*                OTHERS                   = 7.
*            IF SY-SUBRC <> 0.
*              MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
*                    DISPLAY LIKE 'E'.
*            ENDIF.
*
*          ENDIF.

        ENDLOOP.

        EXIT. "Exit Do loop

      ELSE.

        EXIT. "T41K920280

      ENDIF.

    ENDIF.

  ENDDO.
*<---- End Insert by Teerasithi
ENDFORM.                               " SMARTFORM_CALL



*&---------------------------------------------------------------------*
*&      Form  EXCEPTION_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_newexc   new code of exception parameter
*      <--P_H_EXCEPTION old code of exception
*----------------------------------------------------------------------*
FORM EXCEPTION_SET  TABLES    T_ERROR STRUCTURE IDWTERROR
                    USING     VALUE(P_NEWEXC)
                              I_GLOB LIKE IDWTGLOB
                    CHANGING  P_OLDEXC
                            .
  DATA: WA_ERROR LIKE IDWTERROR.
*single error was caused.
  IF P_OLDEXC IS INITIAL .
    P_OLDEXC = P_NEWEXC.
* multiple errors.
  ELSE.
* fill the error table
    IF I_GLOB-WT_SC_LIST_ACT4 IS INITIAL.
      CASE P_OLDEXC.
        WHEN '1'.
          MESSAGE I011(ID_WT) WITH I_GLOB-WT_OG_FORM_NM1
                  RAISING ERROR_FORM.
        WHEN '2'.
          MESSAGE I008(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME
                  RAISING ERROR_FILE.
      ENDCASE.

      CASE P_NEWEXC.
        WHEN '1'.
          MESSAGE I011(ID_WT) WITH WA_FORMNAME
                  RAISING ERROR_FORM.
        WHEN '2'.
          MESSAGE I008(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME
                  RAISING ERROR_FILE.
      ENDCASE.

    ELSE.
      CLEAR WA_ERROR.
      WA_ERROR-WT_MSGID = C_MESSAGECLASS.
      WA_ERROR-WT_MSGTY = 'E'.

      CASE P_OLDEXC .
        WHEN '1'.
          WA_ERROR-WT_MSGNR = 11.
          MESSAGE E011(ID_WT) WITH I_GLOB-WT_OG_FORM_NM1
                  INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR(RFIDYYWT) TABLES T_ERROR[]
                                               USING WA_ERROR.

        WHEN '2'.
          WA_ERROR-WT_MSGNR = 8.
          MESSAGE E008(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME
                  INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR(RFIDYYWT) TABLES T_ERROR[]
                                               USING WA_ERROR.
      ENDCASE.


      CASE P_NEWEXC .
        WHEN '1'.
          WA_ERROR-WT_MSGNR = 11.
          MESSAGE E011(ID_WT) WITH WA_FORMNAME
                  INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR(RFIDYYWT) TABLES T_ERROR[]
                                               USING WA_ERROR.
        WHEN '2'.
          WA_ERROR-WT_MSGNR = 8.
          MESSAGE E008(ID_WT) WITH I_GLOB-WT_SC_FILE_NAME
                  INTO WA_ERROR-WT_NATXT.
          PERFORM F01_FILL_IDWTERROR(RFIDYYWT) TABLES T_ERROR[]
                                               USING WA_ERROR.
      ENDCASE.


    ENDIF.

*set the error code multi
    P_OLDEXC = 4.


  ENDIF.


ENDFORM.                               " EXCEPTION_SET

*&---------------------------------------------------------------------*
*&      Form  LISTINFO_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_GLOB  text
*      -->P_H_HANDLE  text
*      <--P_T_INFO1  text
*----------------------------------------------------------------------*
FORM LISTINFO_FILL USING    P_GLOB TYPE IDWTGLOB
                            BADIOBJ TYPE REF TO
                                 IF_EX_IDWTREP_ADDFUNCINT
                   CHANGING P_INFO1 TYPE TAX_ALV_INFO
                            P_INFO2 TYPE TAX_ALV_INFO
                            P_INFO3 TYPE TAX_ALV_INFO
                            P_INFO4 TYPE TAX_ALV_INFO.
  CONSTANTS: H_HANDLE1 TYPE SLIS_HANDL VALUE 'HAN1',
             H_HANDLE2 TYPE SLIS_HANDL VALUE 'HAN2',
             H_HANDLE3 TYPE SLIS_HANDL VALUE 'HAN3',
             H_HANDLE4 TYPE SLIS_HANDL VALUE 'HAN4'.

*  data: h_pointer TYPE REF TO data.

  IF P_GLOB-WT_SC_LIST_ACT1 = 'X'.
    P_INFO1-PRINT_TABLE   = P_GLOB-WT_SC_LIST_ACT1.
    P_INFO1-LIST_TITLE    = P_GLOB-WT_OG_LIST_TI1.
    P_INFO1-LAYOUT_HANDLE = H_HANDLE1.
    P_INFO1-LAYOUT_NAME   = P_GLOB-WT_OG_LIST_VR1.
* modify fieldcatelog
    PERFORM ALV_FIEDCATELOG_MODIFY USING    P_GLOB
                                          H_HANDLE1
                                          BADIOBJ
                                 CHANGING P_INFO1.
  ENDIF.

  IF P_GLOB-WT_SC_LIST_ACT2 = 'X'.
    P_INFO2-PRINT_TABLE   = P_GLOB-WT_SC_LIST_ACT2.
    P_INFO2-LIST_TITLE    = P_GLOB-WT_OG_LIST_TI2.
    P_INFO2-LAYOUT_HANDLE = H_HANDLE2.
    P_INFO2-LAYOUT_NAME   = P_GLOB-WT_OG_LIST_VR2.
* modify the column header
    PERFORM ALV_FIEDCATELOG_MODIFY USING   P_GLOB
                                           H_HANDLE2
                                           BADIOBJ
                                  CHANGING P_INFO2.
  ENDIF.

  IF P_GLOB-WT_SC_LIST_ACT3 = 'X'.
    P_INFO3-PRINT_TABLE   = P_GLOB-WT_SC_LIST_ACT3.
    P_INFO3-LIST_TITLE    = P_GLOB-WT_OG_LIST_TI3.
    P_INFO3-LAYOUT_HANDLE = H_HANDLE3.
    P_INFO3-LAYOUT_NAME   = P_GLOB-WT_OG_LIST_VR3.
* modify the column header
    PERFORM ALV_FIEDCATELOG_MODIFY USING   P_GLOB
                                           H_HANDLE3
                                           BADIOBJ
                                  CHANGING P_INFO3.
* >>> START OF DELETION  <<<                             "P9CK115022
* to be deleted: begin
*  field-symbols:  <fieldcat> TYPE slis_t_fieldcat_alv.
*  assign p_info3-POINTER_FIELDCAT->* to  <fieldcat>.
* >>> END   OF DELETION  <<<                             "P9CK115022

* to be deleted: end
  ENDIF.

  IF P_GLOB-WT_SC_LIST_ACT4 = 'X'.
    P_INFO4-PRINT_TABLE   = P_GLOB-WT_SC_LIST_ACT4.
    P_INFO4-LIST_TITLE    = P_GLOB-WT_OG_LIST_TI4.
    P_INFO4-LAYOUT_HANDLE = H_HANDLE4.
    P_INFO4-LAYOUT_NAME   = P_GLOB-WT_OG_LIST_VR4.
* modify column header
    PERFORM ALV_FIEDCATELOG_MODIFY USING   P_GLOB
                                           H_HANDLE4
                                           BADIOBJ
                                  CHANGING P_INFO4.
*   clear h_pointer.
*   GET REFERENCE OF p_info4 INTO h_pointer.
*   p_info4-POINTER_FIELDCAT = h_pointer.

  ENDIF.

ENDFORM.                               " LISTINFO_FILL

*&---------------------------------------------------------------------*
*&      Form  fill_idwterror
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0473   text
*      -->P_WA_WT_NATXT  text
*      <--P_C_ERROR[]  text
*----------------------------------------------------------------------*
FORM FILL_IDWTERROR TABLES   T_ERROR STRUCTURE IDWTERROR
                    USING    P_MSGID TYPE MSGID
                             P_NATXT TYPE NATXT
                             .
  DATA: WA_ERROR LIKE IDWTERROR.

  CLEAR WA_ERROR.
  WA_ERROR-WT_MSGID       = C_MESSAGECLASS.
  WA_ERROR-WT_MSGTY       = 'E'.
  WA_ERROR-WT_MSGNR       = P_MSGID.
  WA_ERROR-WT_NATXT       = P_NATXT.
  PERFORM F01_FILL_IDWTERROR(RFIDYYWT) TABLES T_ERROR[]
                              USING WA_ERROR.

ENDFORM.                               " fill_idwterror
*&---------------------------------------------------------------------*
*&      Form  alv_fiedcatelog_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_H_HANDLE1  text
*      <--P_P_INFO1  text
*----------------------------------------------------------------------*
FORM ALV_FIEDCATELOG_MODIFY USING    P_GLOB TYPE IDWTGLOB
                                     P_HANDLE TYPE SLIS_HANDL
                                     BADIOBJ TYPE REF TO
                                             IF_EX_IDWTREP_ADDFUNCINT
                            CHANGING P_INFO TYPE TAX_ALV_INFO .

  DATA: H_STRUCTURE_NAME LIKE DD02L-TABNAME,
        P_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        H_POINTER TYPE REF TO DATA.

* >>> START OF DELETION  <<<                           "P9CK115022
* to be deleted
*field-symbols:  <fieldcat> TYPE slis_t_fieldcat_alv.  "Fieldcat for ALV
*
* check p_glob-intca = 'GB'.   "has to be deleted
* >>> END   OF DELETION  <<<                           "P9CK115022

  CASE P_HANDLE.
    WHEN 'HAN1'.
      H_STRUCTURE_NAME = 'IDWTCOMPCD' .
    WHEN 'HAN2'.
      H_STRUCTURE_NAME = 'IDWTPARTNER' .
    WHEN 'HAN3'.
      H_STRUCTURE_NAME = 'IDWTFIDOC' .
    WHEN 'HAN4'.
      H_STRUCTURE_NAME = 'IDWTERROR' .
  ENDCASE.

* get original field catelog.
* >>> START OF DELETION  <<<                     "P9CK115021
*CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*       EXPORTING
*            i_program_name     = 'RFIDYYWT'
*     "g_epid
**           i_internal_tabname = h_itabname
*            i_structure_name   = h_structure_name
*       CHANGING
*            ct_fieldcat        = p_fieldcat.
* >>> END   OF DELETION  <<<                     "P9CK115021

* >>> START OF INSERTION <<<                     "P9CK115021
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME               = 'RFIDYYWT'
*     I_INTERNAL_TABNAME           =
      I_STRUCTURE_NAME             = H_STRUCTURE_NAME
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_INCLNAME                   =
      I_BYPASSING_BUFFER           = 'X'
*     I_BUFFER_ACTIVE              =
    CHANGING
      CT_FIELDCAT                  = P_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE       = 1
      PROGRAM_ERROR                = 2
      OTHERS                       = 3 .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
* >>> END   OF INSERTION <<<                     "P9CK115021

* modify fieldcatelog.
  CALL METHOD BADIOBJ->MODIFY_FIELDCATALOG
    EXPORTING
      FLT_VAL        = P_GLOB-INTCA
      I_HANDLE       = P_HANDLE
    CHANGING
      C_TAB_FIELDCAT = P_FIELDCAT.
*                    C_ERROR    = c_error[].

*    IF SY-SUBRC <> 0.
*    if wa_idwtglob-wt_sc_list_act4 is initial.
*     MESSAGE E024(id_wt) with 'UPDATES_AFTER_OUTPUT' raising ERROR_BADI
*  .
*    else.
*      MESSAGE E024 WITH 'UPDATES_AFTER_OUTPUT'
*                   into wa_wt_natxt.
*      perform fill_idwterror tables c_error[]
*                             using  '24' wa_wt_natxt
*                             .
*
*    endif.

* >>> START OF DELETION  <<<                             "P9CK115022
* assign the fieldcat. to a pointer.
* clear h_pointer.
* get reference of p_fieldcat into h_pointer.
* p_info-pointer_fieldcat = h_pointer.
* to be deleted
* assign p_info-pointer_fieldcat->* to <fieldcat>.
* >>> END   OF DELETION  <<<                             "P9CK115022

* >>> START OF INSERTION <<<                             "P9CK115022
  FIELD-SYMBOLS: <FS> TYPE ANY.

  CASE P_HANDLE.
    WHEN 'HAN1'.
      T_FIELDCAT1[] = P_FIELDCAT[].
      GET REFERENCE OF T_FIELDCAT1 INTO P_INFO-POINTER_FIELDCAT.
    WHEN 'HAN2'.
      T_FIELDCAT2[] = P_FIELDCAT[].
      GET REFERENCE OF T_FIELDCAT2 INTO P_INFO-POINTER_FIELDCAT.
    WHEN 'HAN3'.
      T_FIELDCAT3[] = P_FIELDCAT[].
      GET REFERENCE OF T_FIELDCAT3 INTO P_INFO-POINTER_FIELDCAT.
    WHEN 'HAN4'.
      T_FIELDCAT4[] = P_FIELDCAT[].
      GET REFERENCE OF T_FIELDCAT4 INTO P_INFO-POINTER_FIELDCAT.
  ENDCASE.


* >>> END   OF INSERTION <<<                             "P9CK115022

ENDFORM.                               " alv_fiedcatelog_modify
