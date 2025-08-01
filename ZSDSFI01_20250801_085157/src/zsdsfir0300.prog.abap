*-----------------------------------------------------------------------
*  Program ID         : ZSDSFIR0300
*  Creation Date      : 10.06.2024
*  Author             : Waraporn S. (Eviden)
*  Add-on ID          : FIAPE001
*  Description        : This program is to generate to text file
*                       from payment information as Bank Template
*                       both automatic fpayment and manual payment
*  Purpose            : Create payment file to SMBC and SCB
*  Copied from        : N/A
*  Restriction        : N/A
*-----------------------------------------------------------------------
*  CHANGE HISTORY
*-----------------------------------------------------------------------
*  Date        Task #      Programmer  Description
*-----------------------------------------------------------------------
*  14.11.2024  F36K908804  Waraporn S. CH01
*                                      1. Modify logic split amount SMBC
*  14.01.2025  F36K909322  Waraporn S. CH02 (Ticke# 420000241)
*                                      1. Fix logic for fiscal year
*                                      2. Fix case SMBC Split 2M
*                                         WHT Amount incorrect
*  23.07.2025  F36K921705  Waraporn S. CH03 (Ticke# 420000698)
*                                      1. Fix case SMBS Split for EWHT
*                                         For Splitted record,
*                                         Set WHT_CERTIFICATE to 02
*                                         Except the last record of the
*                                         group set WHT_CERTIFICATE to 05
*-----------------------------------------------------------------------
REPORT ZSDSFIR0300.

*----------------------------------------------------------------------*
* INCLUDES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: SSCRFIELDS, REGUH, BKPF.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_RESULT.
         INCLUDE TYPE ZSDSFIS100.
TYPES: END OF TS_RESULT.
TYPES: TT_RESULT TYPE STANDARD TABLE OF TS_RESULT
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_RESULT_LOG.
         INCLUDE TYPE ZSDSFIT047.
TYPES: END OF TS_RESULT_LOG.
TYPES: TT_RESULT_LOG TYPE STANDARD TABLE OF TS_RESULT_LOG
                         WITH DEFAULT KEY.

TYPES: BEGIN OF TS_VENGRP_EMP_RANGE,
         SIGN   TYPE SIGN_RANGE,
         OPTION TYPE OPT_RANGE,
         LOW    TYPE KTOKK,
         HIGH   TYPE KTOKK,
       END OF TS_VENGRP_EMP_RANGE.
TYPES: TT_VENGRP_EMP_RANGE TYPE STANDARD TABLE OF TS_VENGRP_EMP_RANGE.

TYPES: BEGIN OF TS_REGUH,
         LAUFD TYPE REGUH-LAUFD,
         LAUFI TYPE REGUH-LAUFI,
         XVORL TYPE REGUH-XVORL,
         ZBUKR TYPE REGUH-ZBUKR,
         LIFNR TYPE REGUH-LIFNR,
         KUNNR TYPE REGUH-KUNNR,
         EMPFG TYPE REGUH-EMPFG,
         VBLNR TYPE REGUH-VBLNR,
         WAERS TYPE REGUH-WAERS,
         HBKID TYPE REGUH-HBKID,
         HKTID TYPE REGUH-HKTID,
         UBKNT TYPE REGUH-UBKNT,
         ZALDT TYPE REGUH-ZALDT,
         RBETR TYPE REGUH-RBETR,
         RWBTR TYPE REGUH-RWBTR,
         VALUT TYPE REGUH-VALUT,
         RZAWE TYPE REGUH-RZAWE,
         UBHKT TYPE REGUH-UBHKT,
         GJAHR TYPE BKPF-GJAHR,
       END OF TS_REGUH.
TYPES: TT_REGUH TYPE STANDARD TABLE OF TS_REGUH.

TYPES: BEGIN OF TS_REGUP,
         LAUFD TYPE REGUP-LAUFD,
         LAUFI TYPE REGUP-LAUFI,
         XVORL TYPE REGUP-XVORL,
         ZBUKR TYPE REGUP-ZBUKR,
         LIFNR TYPE REGUP-LIFNR,
         KUNNR TYPE REGUP-KUNNR,
         EMPFG TYPE REGUP-EMPFG,
         VBLNR TYPE REGUP-VBLNR,
         BUKRS TYPE REGUP-BUKRS,
         BELNR TYPE REGUP-BELNR,
         GJAHR TYPE REGUP-GJAHR,
         BUZEI TYPE REGUP-BUZEI,
         BLART TYPE REGUP-BLART,
         SHKZG TYPE REGUP-SHKZG,
         DMBTR TYPE REGUP-DMBTR,
         XCPDD TYPE REGUP-XCPDD,
       END OF TS_REGUP.
TYPES: TT_REGUP TYPE STANDARD TABLE OF TS_REGUP.

TYPES: BEGIN OF TS_LIFNR,
         LIFNR TYPE LFA1-LIFNR,
       END OF TS_LIFNR.
TYPES: TT_LIFNR TYPE STANDARD TABLE OF TS_LIFNR.

TYPES: BEGIN OF TS_ADRNR,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,
       END OF TS_ADRNR.
TYPES: TT_ADRNR TYPE STANDARD TABLE OF TS_ADRNR.

TYPES: BEGIN OF TS_FIDOC_KEY,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BUZEI TYPE BSEG-BUZEI,
       END OF TS_FIDOC_KEY.
TYPES: TT_FIDOC_KEY TYPE SORTED TABLE OF TS_FIDOC_KEY
                    WITH NON-UNIQUE KEY BUKRS BELNR GJAHR BUZEI.

TYPES: BEGIN OF TS_VENDOR_DETAIL,
         LIFNR      TYPE LFA1-LIFNR,
         BANKS      TYPE LFBK-BANKS,
         BANKL      TYPE LFBK-BANKL,
         BANKN      TYPE LFBK-BANKN,
         BVTYP      TYPE LFBK-BVTYP,
         BANKA      TYPE BNKA-BANKA,
         BRNCH      TYPE BNKA-BRNCH,
         NAME1      TYPE LFA1-NAME1,
         NAME2      TYPE LFA1-NAME2,
         NAME3      TYPE LFA1-NAME3,
         NAME4      TYPE LFA1-NAME4,
         STCD1      TYPE LFA1-STCD1,
         STCD3      TYPE LFA1-STCD3,
         ADRNR      TYPE LFA1-ADRNR,
         KTOKK      TYPE LFA1-KTOKK,
         XCPDK      TYPE LFA1-XCPDK,
         TITLE      TYPE TSAD3T-TITLE_MEDI,
         NAME_FIRST TYPE BUT000-NAME_FIRST,
         NAME_LAST  TYPE BUT000-NAME_LAST,
       END OF TS_VENDOR_DETAIL.
TYPES: TT_VENDOR_DETAIL TYPE SORTED TABLE OF TS_VENDOR_DETAIL
                        WITH UNIQUE KEY LIFNR BANKS BANKL BANKN.

TYPES: BEGIN OF TS_ONETIME_DETAIL,
         BUKRS TYPE BSEC-BUKRS,
         BELNR TYPE BSEC-BELNR,
         GJAHR TYPE BSEC-GJAHR,
         BUZEI TYPE BSEC-BUZEI,
         NAME1 TYPE BSEC-NAME1,
         NAME2 TYPE BSEC-NAME2,
         NAME3 TYPE BSEC-NAME3,
         NAME4 TYPE BSEC-NAME4,
         PSTLZ TYPE BSEC-PSTLZ,
         ORT01 TYPE BSEC-ORT01,
         STRAS TYPE BSEC-STRAS,
         BANKN TYPE BSEC-BANKN,
         BANKL TYPE BSEC-BANKL,
         BANKS TYPE BSEC-BANKS,
         BANKA TYPE BNKA-BANKA,
         BRNCH TYPE BNKA-BRNCH,
         STCD1 TYPE BSEC-STCD1,
         STCD3 TYPE BSEC-STCD3,
         ADRNR TYPE BSEC-ADRNR,
         INTAD TYPE BSEC-INTAD,
       END OF TS_ONETIME_DETAIL.
TYPES: TT_ONETIME_DETAIL TYPE STANDARD TABLE OF TS_ONETIME_DETAIL.

TYPES: BEGIN OF TS_WHT_DETAIL,
         BUKRS     TYPE WITH_ITEM-BUKRS,
         BELNR     TYPE WITH_ITEM-BELNR,
         GJAHR     TYPE WITH_ITEM-GJAHR,
         BUZEI     TYPE WITH_ITEM-BUZEI,
         WITHT     TYPE WITH_ITEM-WITHT,
         WT_WITHCD TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHH  TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHH  TYPE WITH_ITEM-WT_QBSHH,
         CTNUMBER  TYPE WITH_ITEM-CTNUMBER,
         QSREC     TYPE WITH_ITEM-QSREC,
         QSATZ     TYPE T059Z-QSATZ,
         QEKAR     TYPE T059Z-QEKAR,
         TEXT40    TYPE T059ZT-TEXT40,
       END OF TS_WHT_DETAIL.
TYPES: TT_WHT_DETAIL TYPE SORTED TABLE OF TS_WHT_DETAIL
                     WITH UNIQUE KEY BUKRS BELNR GJAHR BUZEI WITHT.

TYPES: BEGIN OF TS_INVDOC_DETAIL_AUTO,
         BUKRS      TYPE BKPF-BUKRS,
         BELNR      TYPE BKPF-BELNR,
         GJAHR      TYPE BKPF-GJAHR,
         BUZEI      TYPE BSEG-BUZEI,
         BLDAT      TYPE BKPF-BLDAT,
         XBLNR      TYPE BKPF-XBLNR,
         XREVERSED  TYPE BKPF-XREVERSED,
         XREVERSING TYPE BKPF-XREVERSING,
         STBLG      TYPE BKPF-STBLG,
         STJAH      TYPE BKPF-STJAH,
         SGTXT      TYPE BSEG-SGTXT,
         REBZG      TYPE BSEG-REBZG,
         REBZJ      TYPE BSEG-REBZJ,
         REBZZ      TYPE BSEG-REBZZ,
*         HWBAS      TYPE BSET-HWBAS,
*         HWSTE      TYPE BSET-HWSTE,
*         SHKZG      TYPE BSET-SHKZG,
         WT_QSSHH   TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHH   TYPE WITH_ITEM-WT_QBSHH,
       END OF TS_INVDOC_DETAIL_AUTO.
TYPES: TT_INVDOC_DETAIL_AUTO TYPE SORTED TABLE OF TS_INVDOC_DETAIL_AUTO
                             WITH UNIQUE KEY BUKRS BELNR GJAHR BUZEI.

TYPES: BEGIN OF TS_ADDR_DETAIL,
         ADDRNUMBER TYPE ADRC-ADDRNUMBER,
         DATE_FROM  TYPE ADRC-DATE_FROM,
         NATION     TYPE ADRC-NATION,
         NAME1      TYPE ADRC-NAME1,
         NAME2      TYPE ADRC-NAME2,
         NAME3      TYPE ADRC-NAME3,
         NAME4      TYPE ADRC-NAME4,
         TEL_NUMBER TYPE ADRC-TEL_NUMBER,
         HOUSE_NUM1 TYPE ADRC-HOUSE_NUM1,
         STREET     TYPE ADRC-STREET,
         STR_SUPPL1 TYPE ADRC-STR_SUPPL1,
         STR_SUPPL2 TYPE ADRC-STR_SUPPL2,
         STR_SUPPL3 TYPE ADRC-STR_SUPPL3,
         CITY2      TYPE ADRC-CITY2,
         CITY1      TYPE ADRC-CITY1,
         POST_CODE1 TYPE ADRC-POST_CODE1,
         PERS_ADDR  TYPE ADRC-PERS_ADDR,
         SMTP_ADDR  TYPE ADR6-SMTP_ADDR,
         CONSNUMBER TYPE ADR6-CONSNUMBER,
         FLGDEFAULT TYPE ADR6-FLGDEFAULT,
       END OF TS_ADDR_DETAIL.
TYPES: TT_ADDR_DETAIL TYPE SORTED TABLE OF TS_ADDR_DETAIL
                      WITH UNIQUE KEY ADDRNUMBER DATE_FROM NATION CONSNUMBER.

*----------------------------------------------------------------------*
TYPES: BEGIN OF TS_PAYMENT_DOCUMENT,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BLART TYPE BKPF-BLART,
         BUDAT TYPE BKPF-BUDAT,
         WAERS TYPE BKPF-WAERS,
         BUZEI TYPE BSEG-BUZEI,
         ZLSCH TYPE BSEG-ZLSCH,
         LIFNR TYPE BSEG-LIFNR,
         EMPFB TYPE BSEG-EMPFB,
         HBKID TYPE BSEG-HBKID,
         HKTID TYPE BSEG-HKTID,
         BVTYP TYPE BSEG-BVTYP,
         HKONT TYPE BSEG-HKONT,
         SHKZG TYPE BSEG-SHKZG,
         DMBTR TYPE DEC31_14, "BSEG-DMBTR,
         XCPDD TYPE BSEG-XCPDD,
         VALUT TYPE BSEG-VALUT,
         REBZG TYPE BSEG-REBZG,
         REBZJ TYPE BSEG-REBZJ,
         REBZZ TYPE BSEG-REBZZ,
         REBZT TYPE BSEG-REBZT,
         NEBTR TYPE BSEG-NEBTR,
       END OF TS_PAYMENT_DOCUMENT.
TYPES: TT_PAYMENT_DOCUMENT TYPE STANDARD TABLE OF TS_PAYMENT_DOCUMENT.

TYPES: BEGIN OF TS_PAYMENT_DOCUMENT_BANKGL,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BUZEI TYPE BSEG-BUZEI,
         HKONT TYPE BSEG-HKONT,
         SHKZG TYPE BSEG-SHKZG,
         DMBTR TYPE BSEG-DMBTR,
       END OF TS_PAYMENT_DOCUMENT_BANKGL.
TYPES: TT_PAYMENT_DOCUMENT_BANKGL TYPE SORTED TABLE OF TS_PAYMENT_DOCUMENT_BANKGL
                                  WITH UNIQUE KEY BUKRS BELNR GJAHR BUZEI.

TYPES: BEGIN OF TS_HOUSE_BANK_ACC,
         ZBUKR TYPE T042I-ZBUKR,
         HBKID TYPE T042I-HBKID,
         ZLSCH TYPE T042I-ZLSCH,
         WAERS TYPE T042I-WAERS,
         HKTID TYPE T042I-HKTID,
         UKONT TYPE T042I-UKONT,
         BANKN TYPE T012K-BANKN,
       END OF TS_HOUSE_BANK_ACC.
TYPES: TT_HOUSE_BANK_ACC TYPE SORTED TABLE OF TS_HOUSE_BANK_ACC
                         WITH UNIQUE KEY ZBUKR HBKID ZLSCH WAERS
                         WITH NON-UNIQUE SORTED KEY UKONT COMPONENTS UKONT.

TYPES: BEGIN OF TS_INVDOC_DETAIL_MANUAL,
         BUKRS_CLR  TYPE BSE_CLR-BUKRS_CLR,
         BELNR_CLR  TYPE BSE_CLR-BELNR_CLR,
         GJAHR_CLR  TYPE BSE_CLR-GJAHR_CLR,
         INDEX_CLR  TYPE BSE_CLR-INDEX_CLR,
         WAERS      TYPE BSE_CLR-WAERS,
         CLRIN      TYPE BSE_CLR-CLRIN,
         BUKRS      TYPE BSE_CLR-BUKRS,
         BELNR      TYPE BSE_CLR-BELNR,
         GJAHR      TYPE BSE_CLR-GJAHR,
         BUZEI      TYPE BSE_CLR-BUZEI,
         SHKZG      TYPE BSE_CLR-SHKZG,
         DMBTR      TYPE BSE_CLR-DMBTR,
         DIFHW      TYPE BSE_CLR-DIFHW,
         BLDAT      TYPE BKPF-BLDAT,
         XBLNR      TYPE BKPF-XBLNR,
         XREVERSED  TYPE BKPF-XREVERSED,
         XREVERSING TYPE BKPF-XREVERSING,
         STBLG      TYPE BKPF-STBLG,
         STJAH      TYPE BKPF-STJAH,
         REBZG      TYPE BSEG-REBZG,
         REBZJ      TYPE BSEG-REBZJ,
         REBZZ      TYPE BSEG-REBZZ,
         SGTXT      TYPE BSEG-SGTXT,
         XCPDD      TYPE BSEG-XCPDD,
         WT_QSSHH   TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHH   TYPE WITH_ITEM-WT_QBSHH,
       END OF TS_INVDOC_DETAIL_MANUAL.
TYPES: TT_INVDOC_DETAIL_MANUAL TYPE SORTED TABLE OF TS_INVDOC_DETAIL_MANUAL
                               WITH NON-UNIQUE KEY BUKRS_CLR BELNR_CLR
                                                   GJAHR_CLR INDEX_CLR
                               WITH NON-UNIQUE SORTED KEY INVDOC COMPONENTS BUKRS BELNR GJAHR BUZEI.

TYPES: BEGIN OF TS_GENC_SCB,
         SCB_COMPANY_ID          TYPE ZSDSDE_PARAM_VALUE,
         SCB_CHANNEL_ID          TYPE ZSDSDE_PARAM_VALUE,
         SCB_PRODUCT_CODE        TYPE ZSDSDE_PARAM_VALUE,
         SCB_INTERNAL_REF        TYPE ZSDSDE_PARAM_VALUE,
         SCB_PRODUCT_CODE_DIRECT TYPE ZSDSDE_PARAM_VALUE,
         SCB_WHT_PRESENT         TYPE ZSDSDE_PARAM_VALUE,
         SCB_INV_PRESENT         TYPE ZSDSDE_PARAM_VALUE,
         SCB_CREDIT_ADVICE_REQ   TYPE ZSDSDE_PARAM_VALUE,
         SCB_DELIVERY_MODE       TYPE ZSDSDE_PARAM_VALUE,
         SCB_RECV_BANK_NAME      TYPE ZSDSDE_PARAM_VALUE,
         SCB_BENEFICIARY_NOTI    TYPE ZSDSDE_PARAM_VALUE,
         SCB_SERVICE_TYPE        TYPE ZSDSDE_PARAM_VALUE,
       END OF TS_GENC_SCB.

TYPES: BEGIN OF TS_GENC_SMB,
         SMB_ENV_SM            TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_TRANSFERTYPE  TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_DRBRANCH      TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_DRGL          TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_DRGLSUB       TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_DRSSNO        TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_DRSUBNO       TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_DRMKT         TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_CHARGEBRNCH   TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_CHARGEGL      TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_CHARGEGLSUB   TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_CHARGESS      TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_CHARGESUB     TYPE ZSDSDE_PARAM_VALUE,
         SMB_ENV_CHARGEMKT     TYPE ZSDSDE_PARAM_VALUE,
         SMB_INS_EMAILNOTIF    TYPE ZSDSDE_PARAM_VALUE,
         SMB_INS_FAXNOTIF      TYPE ZSDSDE_PARAM_VALUE,
         SMB_INS_CALCMODEID    TYPE ZSDSDE_PARAM_VALUE,
         SMB_INS_TRANSACTIONCD TYPE ZSDSDE_PARAM_VALUE,
         SMB_INS_PAYMENTBYID   TYPE ZSDSDE_PARAM_VALUE,
         SMB_INV_CALCFOR       TYPE ZSDSDE_PARAM_VALUE,
       END OF TS_GENC_SMB.

TYPES: TS_FILE TYPE STRING.
TYPES: TT_FILE TYPE STANDARD TABLE OF TS_FILE.

TYPES: BEGIN OF TS_SUM_BY_VALUE_DATE,
         UBKNT        TYPE T012K-BANKN,
         VALUT        TYPE BSEG-VALUT,
         PAY_AMT      TYPE BSEG-DMBTR,
         NO_OF_CREDIT TYPE I,
       END OF TS_SUM_BY_VALUE_DATE.
TYPES: TT_SUM_BY_VALUE_DATE TYPE SORTED TABLE OF TS_SUM_BY_VALUE_DATE
                            WITH UNIQUE KEY UBKNT VALUT.

TYPES: BEGIN OF TS_SUM_CREDIT_BY_VENDOR,
         UBKNT         TYPE T012K-BANKN,
         VALUT         TYPE BSEG-VALUT,
         LIFNR         TYPE ZSDSFIS100-LIFNR,
         BANKN         TYPE ZSDSFIS100-BANKN,
         BANKA         TYPE ZSDSFIS100-BANKA,
         BANKL         TYPE ZSDSFIS100-BANKL,
         PAYEE_NAME_EN TYPE ZSDSFIS100-PAYEE_NAME_EN,
         EMAIL         TYPE ZSDSFIS100-EMAIL,
         PAYDOC_BELNR  TYPE ZSDSFIS100-PAYDOC_BELNR,
         PAYDOC_GJAHR  TYPE ZSDSFIS100-PAYDOC_GJAHR,
         PAY_AMT       TYPE BSEG-DMBTR,
       END OF TS_SUM_CREDIT_BY_VENDOR.
TYPES: TT_SUM_CREDIT_BY_VENDOR TYPE SORTED TABLE OF TS_SUM_CREDIT_BY_VENDOR
             WITH NON-UNIQUE KEY UBKNT VALUT LIFNR BANKN BANKA BANKL
                                 PAYEE_NAME_EN EMAIL PAYDOC_BELNR PAYDOC_GJAHR.

TYPES: BEGIN OF TS_SCB_HEADER,
         RECORDTYPE(3)   TYPE C,
         COMPANYID(12)   TYPE C,
         DESCRIPTION(32) TYPE C,
         FILEDATE(8)     TYPE C,
         FILETIME(6)     TYPE C,
         CHANNELID(3)    TYPE C,
       END OF TS_SCB_HEADER.

TYPES: BEGIN OF TS_SCB_HEADER_DIRECT,
         RECORDTYPE(3)   TYPE C,
         COMPANYID(12)   TYPE C,
         DESCRIPTION(32) TYPE C,
         FILEDATE(8)     TYPE C,
         FILETIME(6)     TYPE C,
         CHANNELID(3)    TYPE C,
         BATCHREF(32)    TYPE C,
       END OF TS_SCB_HEADER_DIRECT.

TYPES: BEGIN OF TS_SCB_DEBIT,
         RECORDTYPE(3)      TYPE C,
         PRODUCTCODE(3)     TYPE C,
         VALUEDATE(8)       TYPE C,
         TAXID(13)          TYPE C,
         DEBITACCOUNT(25)   TYPE C,
         ACCOUNTTYPE(2)     TYPE N,
         DEBITBRANCH(4)     TYPE N,
         DEBITCURR(3)       TYPE C,
         DEBITAMOUNT(16)    TYPE C,
         INTERNALREF(8)     TYPE C,
         NUMCREDIT(6)       TYPE N,
         FEEDEBITACC(15)    TYPE C,
         ACCOUNTTYPEFEE(2)  TYPE N,
         DEBITBRANCHFEE(4)  TYPE N,
         TRANSACTIONREF(32) TYPE C,
       END OF TS_SCB_DEBIT.

TYPES: BEGIN OF TS_SCB_DEBIT_DIRECT,
         RECORDTYPE(3)         TYPE C,
         PRODUCTCODE(3)        TYPE C,
         VALUEDATE(8)          TYPE C,
         DEBITACCOUNT(25)      TYPE C,
         ACCOUNTTYPE(2)        TYPE N,
         DEBITBRANCH(4)        TYPE N,
         DEBITCURR(3)          TYPE C,
         DEBITAMOUNT(16)       TYPE C,
         INTERNALREF(8)        TYPE C,
         NUMCREDIT(6)          TYPE N,     "10
         FEEDEBITACC(15)       TYPE C,
         FILLER(9)             TYPE C,
         MEDIACLEARINGCYCLE(1) TYPE C,
         ACCOUNTTYPEFEE(2)     TYPE N,
         DEBITBRANCHFEE(4)     TYPE N,
       END OF TS_SCB_DEBIT_DIRECT.

TYPES: BEGIN OF TS_SCB_CREDIT,
         RECORDTYPE(3)     TYPE C,
         CREDITSEQNO(6)    TYPE N,
         CREDITACCOUNT(25) TYPE C,
         BENEFICIARY(70)   TYPE C,
         CREDITAMOUNT(16)  TYPE C,
         CREDITCURR(3)     TYPE C,
         RECVBANKNAME(35)  TYPE C,
         RECVBANKCODE(4)   TYPE N,
         TAXID(13)         TYPE C,
         MOBILE(10)        TYPE C,  "10
         INTERNALREF(8)    TYPE C,
         REMARK(32)        TYPE C,
         PROXYTYPE(3)      TYPE C,
         FILLER(46)        TYPE C,
       END OF TS_SCB_CREDIT.

TYPES: BEGIN OF TS_SCB_CREDIT_DIRECT,
         RECORDTYPE(3)          TYPE C,
         CREDITSEQNO(6)         TYPE N,
         CREDITACCOUNT(25)      TYPE C,
         CREDITAMOUNT(16)       TYPE C,
         CREDITCURR(3)          TYPE C,
         INTERNALREF(8)         TYPE C,
         WHTPRESENT(1)          TYPE C,
         INVOICEPRESENT(1)      TYPE C,
         CREDITADVICEREQ(1)     TYPE C,
         DELIVERYMODE(1)        TYPE C,    "10
         PICKUPLOCATION(4)      TYPE C,
         WHTFORMTYPE(2)         TYPE C,
         WHTRUNNINGNO(14)       TYPE C,
         WHTATTACHNO(6)         TYPE C,
         NUMWHTDETAIL(2)        TYPE C,
         TOTALWHTAMOUNT(16)     TYPE C,
         NUMINVOICEDETAIL(6)    TYPE C,
         TOTALINVOICEAMOUNT(16) TYPE C,
         WHTPAYTYPE(1)          TYPE C,
         WHTREMARK(40)          TYPE C,    "20
         WHTDEDUCTDATE(8)       TYPE C,
         RECVBANKCODE(3)        TYPE C,
         RECVBANKNAME(35)       TYPE C,
         RECVBRANCHCODE(4)      TYPE C,
         RECVBRANCHNAME(35)     TYPE C,
         WHTSIGNATORY(1)        TYPE C,
         BENEFICIARY(1)         TYPE C,
         CUSTREF(20)            TYPE C,
         CHEQUEREFDOCTYPE(1)    TYPE C,
         PAYMENTTYPECODE(3)     TYPE C,   "30
         SERVICETYPE(2)         TYPE C,
         REMARK(50)             TYPE C,
         SCBREMARK(18)          TYPE C,
         BENEFICIARYCHARGE(2)   TYPE C,
       END OF TS_SCB_CREDIT_DIRECT.

TYPES: BEGIN OF TS_SCB_PAYEE_DETAIL,
         RECORDTYPE(3)     TYPE C,
         INTERNALREF(8)    TYPE C,
         CREDITSEQNO(6)    TYPE N,
         PAYEE1IDCARD(15)  TYPE C,
         PAYEE1NAMETH(100) TYPE C,
         PAYEE1ADDR1(70)   TYPE C,
         PAYEE1ADDR2(70)   TYPE C,
         PAYEE1ADDR3(70)   TYPE C,
         PAYEE1TAXID(10)   TYPE C,
         PAYEE1NAMEEN(70)  TYPE C,   "10
         PAYEE1FAX(10)     TYPE C,
         PAYEE1MOBILE(10)  TYPE C,
         PAYEE1EMAIL(64)   TYPE C,
         PAYEE2NAMETH(100) TYPE C,
         PAYEE2ADDR1(70)   TYPE C,
         PAYEE2ADDR2(70)   TYPE C,
         PAYEE2ADDR3(70)   TYPE C,
       END OF TS_SCB_PAYEE_DETAIL.

TYPES: BEGIN OF TS_SCB_TRAILER,
         RECORDTYPE(3)   TYPE C,
         COUNTDEBIT(6)   TYPE N,
         COUNTCREDIT(6)  TYPE N,
         TOTALAMOUNT(16) TYPE C,
       END OF TS_SCB_TRAILER.

TYPES: BEGIN OF TS_SMB_HEADER,           " Record type ENV
         ENV(3)              TYPE C,
         DEFAULTID(20)       TYPE C,
         SM(1)               TYPE C,
         TRANSFERTYPE(2)     TYPE C,
         DEBITTYPE(2)        TYPE C,
         CUSTREF(32)         TYPE C,
         VALUEDATE(8)        TYPE C,
         CCY(3)              TYPE C,
         DEBITBRANCHCD(4)    TYPE C,
         DEBITGLCD(3)        TYPE C,
         DEBITGLSUBCD(5)     TYPE C,
         DEBITSSNO(35)       TYPE C,
         DEBITACCOUNTNO(35)  TYPE C,
         DEBITSUBNO(2)       TYPE C,
         DEBITCCY(3)         TYPE C,
         DEBITMKTCD(2)       TYPE C,
         CHARGEBRANCHCD(4)   TYPE C,
         CHARGEGLCD(4)       TYPE C,
         CHARGEGLSUBCD(4)    TYPE C,
         CHARGESSNO(35)      TYPE C,
         CHARGEACCOUNTNO(35) TYPE C,
         CHARGESUBNO(2)      TYPE C,
         CHARGECCY(3)        TYPE C,
         CHARGEMKTCD(2)      TYPE C,
         REMITCCY(3)         TYPE C,
         VALUEDATETO(8)      TYPE C,
         FREQUENCY(2)        TYPE C,
         SUBSTITUTEDAYFLG(1) TYPE C,
       END OF TS_SMB_HEADER.

TYPES: BEGIN OF TS_SMB_PAY,               " Record type INS
         INS(3)                             TYPE C,  "1
         DEFAULTID(20)                      TYPE C,
         PAYEENO(32)                        TYPE C,
         PAYEENAME(240)                     TYPE C,
         ADDESS(270)                        TYPE C,
         PHONENO(32)                        TYPE C,
         BANKNAME(240)                      TYPE C,
         BANKCODE(10)                       TYPE C,
         BRANCHREGIONCD(10)                 TYPE C,
         BRANCHREGIONNAME(240)              TYPE C,  "10
         BRANCHNAME(240)                    TYPE C,
         ACCOUNTNO(32)                      TYPE C,
         PAYMENTAMOUNT(16)                  TYPE C,
         MESSAGE(240)                       TYPE C,
         OURCHARGE(1)                       TYPE C,
         INSTRUCTIONREF(40)                 TYPE C,
         EMAILADDRESS(70)                   TYPE C,
         EMAILNOTIFICATION(1)               TYPE C,
         ZIPCD(10)                          TYPE C,
         FAXNO(15)                          TYPE C,  "20
         ATTENTION(160)                     TYPE C,
         WHTID(13)                          TYPE C,
         CARDID(13)                         TYPE C,
         WHTFORMID(2)                       TYPE C,
         FAXNOTIFICATION(1)                 TYPE C,
         PICKUPID(3)                        TYPE C,
         CALCMODEID(2)                      TYPE C,
         INVOICEAMOUNT(16)                  TYPE C,
         VATRATE(6)                         TYPE C,
         WHTAMOUNT(16)                      TYPE C,  "30
         DISCOUNTAMOUNT(16)                 TYPE C,
         REGISTEREDMAILADDRESS(270)         TYPE C,
         WHTORDERINGNO(20)                  TYPE C,
         WHTREF(15)                         TYPE C,
         REMARKS(255)                       TYPE C,
         ONBEHALFOFNAME(240)                TYPE C,
         ONBEHALFOFADDRESS(270)             TYPE C,
         ONBEHALFOFWHTID(13)                TYPE C,
         INSTRUCTIONREF2(40)                TYPE C,
         TRANSACTIONCD(8)                   TYPE C,  "40
         INPUTKANA(1)                       TYPE C,
         BANKADDRESS(248)                   TYPE C,
         MEMO(1000)                         TYPE C,
         REMITTYPE(2)                       TYPE C,
         PRIORITY(2)                        TYPE C,
         PAYERNAME(70)                      TYPE C,
         PAYERADDRESS(140)                  TYPE C,
         PAYERTYPE(2)                       TYPE C,
         UNITCODE(9)                        TYPE C,
         INDIVIDUALNO(32)                   TYPE C,  "50
         INDIVIDUALTYPE(2)                  TYPE C,
         COUNTRYNAME(50)                    TYPE C,
         BANKCOUNTRYNAME(50)                TYPE C,
         INTERMEDBANKNAME(70)               TYPE C,
         INTERMEDBANKCODE(11)               TYPE C,
         INTERMEDBANKBRANCHNAME(70)         TYPE C,
         INTERMEDBANKCOUNTRYNAME(50)        TYPE C,
         INTERMEDBANKACCOUNTNO(34)          TYPE C,
         FXAMOUNT(15)                       TYPE C,
         FXDEBITBRANCHCD(4)                 TYPE C,  "60
         FXDEBITGLCD(4)                     TYPE C,
         FXDEBITGLSUBCD(3)                  TYPE C,
         FXDEBITSSNO(35)                    TYPE C,
         FXDEBITACCOUNTNO(35)               TYPE C,
         FXDEBITSUBNO(2)                    TYPE C,
         FXDEBITCCY(3)                      TYPE C,
         FXDEBITMKTCD(2)                    TYPE C,
         PURCHASEAMOUNT(15)                 TYPE C,
         PURCHASEDEBITBRANCHCD(4)           TYPE C,
         PURCHASEDEBITGLCD(4)               TYPE C,  "70
         PURCHASEDEBITGLSUBCD(3)            TYPE C,
         PURCHASEDEBITSSNO(35)              TYPE C,
         PURCHASEDEBITACCOUNTNO(35)         TYPE C,
         PURCHASEDEBITSUBNO(2)              TYPE C,
         PURCHASEDEBITCCY(3)                TYPE C,
         PURCHASEDEBITMKTCD(2)              TYPE C,
         OTHERAMOUNT(15)                    TYPE C,
         OTHERDEBITBRANCHCD(4)              TYPE C,
         OTHERDEBITGLCD(4)                  TYPE C,
         OTHERDEBITGLSUBCD(3)               TYPE C,  "80
         OTHERDEBITSSNO(35)                 TYPE C,
         OTHERDEBITACCOUNTNO(35)            TYPE C,
         OTHERDEBITSUBNO(2)                 TYPE C,
         OTHERDEBITCCY(3)                   TYPE C,
         OTHERDEBITMKTCD(2)                 TYPE C,
         LOCALCOUNTRYNAME(15)               TYPE C,
         PAYMENTTYPE(2)                     TYPE C,
         PAYMENTPROPERTY(2)                 TYPE C,
         BOPTRANSCODE1(6)                   TYPE C,
         BOPCCY1(3)                         TYPE C,  "90
         BOPAMOUNT1(15)                     TYPE C,
         BOPREMARKS1(35)                    TYPE C,
         VATAMOUNT(20)                      TYPE C,
         CALCTYPE(2)                        TYPE C,
         TAXCALCMETHODID(2)                 TYPE C,
         CHEQUENO(10)                       TYPE C,
         CLEARINGDATE(8)                    TYPE C,
         INVOICENO(10)                      TYPE C,
         DOCUMENTNO(10)                     TYPE C,
         REGIONOPTIONFLG(1)                 TYPE C,  "100
         COMPANYID(10)                      TYPE C,
         SERVICECD(2)                       TYPE C,
         ENVELOPECD(30)                     TYPE C,
         INSTRUCTIONCD(30)                  TYPE C,
         COUNT(10)                          TYPE C,
         STATUS(2)                          TYPE C,
         STATUSREMARKS(248)                 TYPE C,
         BKBRSELECTFLG(1)                   TYPE C,
         NEWPAYEEFLG(1)                     TYPE C,
         WARNING(10)                        TYPE C,  "110
         REGTIME(17)                        TYPE C,
         LASTUPDATETIME(17)                 TYPE C,
         BOPTRANSCODE2(6)                   TYPE C,
         BOPCCY2(3)                         TYPE C,
         BOPAMOUNT2(15)                     TYPE C,
         BOPREMARKS2(35)                    TYPE C,
         LASTSHIPMENTDATE(8)                TYPE C,
         IMPORTINSPECTION(2)                TYPE C,
         CONTRACTNO(50)                     TYPE C,
         IMPORTINSPECTIONINVOICENO(50)      TYPE C,  "120
         SAFEAPPROVALNO(50)                 TYPE C,
         CUSTOMSUNITCODE(10)                TYPE C,
         APPLICANTNAME(20)                  TYPE C,
         APPLICANTTELNO(20)                 TYPE C,
         MESSAGETOBANK(80)                  TYPE C,
         COUNTRYCD(10)                      TYPE C,
         BANKCOUNTRYCD(10)                  TYPE C,
         LOCALCOUNTRYCODE(3)                TYPE C,
         CUSTOMSINFONO(10)                  TYPE C,
         TAX_FILE_NUMBER1(9)                TYPE C,  "130
         TAX_FILE_NUMBER2(3)                TYPE C,
         TAX_FILE_NUMBER3(3)                TYPE C,
         TAX_HOLDER_NAME(100)               TYPE C,
         TAX_PAYMENT_CODE(10)               TYPE C,
         DETAILS_OF_PAYMENT(250)            TYPE C,
         TAX_PERIOD_FROM(2)                 TYPE C,
         TAX_PERIOD_TO(2)                   TYPE C,
         TAX_PAYMENT_YEAR(4)                TYPE C,
         REFERENCE_NUMBER(19)               TYPE C,
         TAX_PAYER_NAME(100)                TYPE C,  "140
         EXCISE_TAX_REFERENCE(10)           TYPE C,
         TAX_PAYER_ID(13)                   TYPE C,
         REVENUE_S_CONTROL_CODE(15)         TYPE C,
         DUE_DATE_MEA_S_INVOICE(8)          TYPE C,
         INVOICE_NO_MEA_S_INVOICE(11)       TYPE C,
         COMPANY_ID_SSO(10)                 TYPE C,
         COMPANY_S_BRANCH_ID(6)             TYPE C,
         TERM_OF_PAYMENT(6)                 TYPE C,
         NO_OF_COMPANY_S_BRANCH(6)          TYPE C,
         REGISTRATION_NO(2)                 TYPE C,  "150
         SUBMISSION_DATE(8)                 TYPE C,
         NO_OF_EMPLOYEE(6)                  TYPE C,
         CONTROL_CODE1(5)                   TYPE C,
         CONTROL_CODE2(27)                  TYPE C,
         TAX_PAYER_SIGNATURE_NAME(50)       TYPE C,
         WHT_CERTIFICATE(2)                 TYPE C,
         MESSENGER_NAME(250)                TYPE C,
         EMPLOYER(80)                       TYPE C,
         MESSENGER_ID(20)                   TYPE C,
         MESSENGER_TEL(30)                  TYPE C,  "160
         BILLER_CODE(10)                    TYPE C,
         BILLER_NAME(250)                   TYPE C,
         BILLER_CODE_NAME(10)               TYPE C,
         BILLER_REF1(50)                    TYPE C,
         BILLER_REF2(50)                    TYPE C,
         REF_INFORMATION(250)               TYPE C,
         BILLING_CODE(15)                   TYPE C,
         TAX_CODE(14)                       TYPE C,
         CDF_NO(15)                         TYPE C,
         CDF_DATE(8)                        TYPE C,  "170
         ON_BEHALF_OF(250)                  TYPE C,
         ON_BEHALF_OF_TAX_PAYER_NAME(255)   TYPE C,
         ON_BEHALF_OF_TAX_PAYER_ADDR(255)   TYPE C,
         ON_BEHALF_OF_TAX_PAYER_CODE(14)    TYPE C,
         EXPORT_DUTIES(14)                  TYPE C,
         IMPORT_DUTIES(14)                  TYPE C,
         VALUE_ADDED_TAX(14)                TYPE C,
         ENVIRONMENT_PROTECTION_TAX(14)     TYPE C,
         SPECIAL_SALES_TAX(14)              TYPE C,
         SAFE_GUARD_ANTIDUMPING_DUTIES(14)  TYPE C,  "180
         CUSTOMS_FEE(14)                    TYPE C,
         TAX_TOAL_AMOUNT(14)                TYPE C,
         TRESURYCD(250)                     TYPE C,
         TREASURYNAME(250)                  TYPE C,
         CHAPTER1(3)                        TYPE C,
         ECONOMICCD1(4)                     TYPE C,
         TERM1(8)                           TYPE C,
         DTAXAMOUNT1(14)                    TYPE C,
         CHAPTER2(3)                        TYPE C,
         ECONOMICCD2(4)                     TYPE C,  "190
         TERM2(8)                           TYPE C,
         DTAXAMOUNT2(14)                    TYPE C,
         DTAXTOTALAMOUNT(14)                TYPE C,
         PAYMENTBYID(2)                     TYPE C,
         PERSONALID_TAXID(13)               TYPE C,
         MOBILENO(10)                       TYPE C,
         BILLERSHORTNAME(20)                TYPE C,
         BILLERLONGNAME(50)                 TYPE C,
         CUSTOMERREF(20)                    TYPE C,
         TAXPAYERSID(16)                    TYPE C,  "200
         TAXOBJECTNUMBER(18)                TYPE C,
         TAXCONTENT1(100)                   TYPE C,
         TAXCONTENT2(100)                   TYPE C,
         TAXCONTENT3(100)                   TYPE C,
         CHAPTER3(3)                        TYPE C,
         ECONOMICCODE3(4)                   TYPE C,
         TERM3(8)                           TYPE C,
         DTAXAMOUNT3(14)                    TYPE C,
         TAXCONTENT4(100)                   TYPE C,
         CHAPTER4(3)                        TYPE C,  "210
         ECONOMICCODE4(4)                   TYPE C,
         TERM4(8)                           TYPE C,
         DTAXAMOUNT4(14)                    TYPE C,
         TAXCONTENT5(100)                   TYPE C,
         CHAPTER5(3)                        TYPE C,
         ECONOMICCODE5(4)                   TYPE C,
         TERM5(8)                           TYPE C,
         DTAXAMOUNT5(14)                    TYPE C,
         ACCOUNTNAME(160)                   TYPE C,
         TRANSFERTOID(2)                    TYPE C,   "220
         DUITNOW_MOBILENO(11)               TYPE C,
         DUITNOW_ICNO(12)                   TYPE C,
         DUITNOW_ISSUINGCOUNTRY(2)          TYPE C,
         DUITNOW_PASSPORTNO(20)             TYPE C,
         DUITNOW_ARMYPOLICEID(20)           TYPE C,
         DUITNOW_BUSINESSREGISTRATIONNO(20) TYPE C,
* Begin of Customer fields - Not write to SMB File
         ZNEW_ENV_FLG(1)                    TYPE C,
         ZSPLIT_ID(20)                      TYPE C,
       END OF TS_SMB_PAY.
TYPES: TT_SMB_PAY TYPE STANDARD TABLE OF TS_SMB_PAY.

TYPES: BEGIN OF TS_SMB_INV,      " Record type INV
         INV(3)            TYPE C,
         DEFAULTID(20)     TYPE C,
         REFERENCE(20)     TYPE C,
         CALCFOR(2)        TYPE C,
         INVOICEDATE(8)    TYPE C,
         INVOICEAMOUNT(21) TYPE C,
         VATAMOUNT(21)     TYPE C,
         VATRATE(6)        TYPE C,
         WHTAMOUNT(21)     TYPE C,
         WHTTYPE(8)        TYPE C,
         DESCRIPTION(248)  TYPE C,
         GROSS_AMOUNT(20)  TYPE C,
         INV_RESERVE2(1)   TYPE C,
* Begin of Customer fields - Not write to SMB File
         ZSPLIT_ID(20)     TYPE C,
         WHTAMOUNT_INV(20) TYPE C,
       END OF TS_SMB_INV.
TYPES: TT_SMB_INV TYPE STANDARD TABLE OF TS_SMB_INV.

TYPES: BEGIN OF TS_SUM_PAY,
         PAYDOC_BELNR  TYPE BKPF-BELNR,
         PAYDOC_GJAHR  TYPE BKPF-GJAHR,
         PAYMENTAMOUNT TYPE BSEG-DMBTR,
         INVOICEAMOUNT TYPE BSEG-DMBTR,
         WHTAMOUNT     TYPE BSEG-DMBTR,
         GROSS_AMOUNT  TYPE BSEG-DMBTR,
       END OF TS_SUM_PAY.
TYPES: TT_SUM_PAY TYPE STANDARD TABLE OF TS_SUM_PAY.

TYPES: BEGIN OF TS_SUM_PAY_WHT,
         PAYDOC_BELNR TYPE BKPF-BELNR,
         PAYDOC_GJAHR TYPE BKPF-GJAHR,
         WITHT        TYPE WITH_ITEM-WITHT,
         WT_WITHCD    TYPE WITH_ITEM-WT_WITHCD,
         WT_QSSHH     TYPE WITH_ITEM-WT_QSSHH,
         WT_QBSHH     TYPE WITH_ITEM-WT_QBSHH,
       END OF TS_SUM_PAY_WHT.
TYPES: TT_SUM_PAY_WHT TYPE STANDARD TABLE OF TS_SUM_PAY_WHT.

TYPES: BEGIN OF TS_HBKID_H2H,
         HOUSE_BANK TYPE T012-HBKID,
         INTFNO     TYPE ZSDSCAC004-INTFNO,
       END OF TS_HBKID_H2H.
TYPES: TT_HBKID_H2H TYPE STANDARD TABLE OF TS_HBKID_H2H.

TYPES: BEGIN OF TS_PROC_INFO,
         DATUM     TYPE SY-DATUM,
         UZEIT     TYPE SY-UZEIT,
         UNAME     TYPE SY-UNAME,
         FILENAME  TYPE STRING,
         TOTINVAMT TYPE DMBTR,
         TOTWHTAMT TYPE DMBTR,
         TOTPAYAMT TYPE DMBTR,
       END OF TS_PROC_INFO.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  GC_TRUE      TYPE CHAR1 VALUE 'X',
  GC_SEPARATOR TYPE CHAR1 VALUE ',',
  GC_QUOTE     TYPE CHAR1 VALUE '"',
  GC_FILE_TXT  TYPE CHAR4 VALUE '.txt' ##NO_TEXT,
  GC_THB       TYPE BKPF-WAERS VALUE 'THB',
  GC_WHT       TYPE CHAR3 VALUE 'WHT',
  GC_TCODE     TYPE SY-TCODE VALUE 'ZSDSFI031',
  GC_NATION_I  TYPE ADRC-NATION VALUE 'I'.     "Address Int. version

CONSTANTS: BEGIN OF GC_KOART,
             CUSTOMER TYPE SHKZG VALUE 'D',
             VENDOR   TYPE SHKZG VALUE 'K',
           END OF GC_KOART.

CONSTANTS: BEGIN OF GC_SHKZG,
             CREDIT TYPE SHKZG VALUE 'H',
             DEBIT  TYPE SHKZG VALUE 'S',
           END OF GC_SHKZG.

CONSTANTS: BEGIN OF GC_HBKID,
             SMB TYPE T012K-HBKID VALUE 'SMB',
             SCB TYPE T012K-HBKID VALUE 'SCB',
           END OF GC_HBKID.

CONSTANTS: BEGIN OF GC_SCB_RECTYPE,
             HEADER  TYPE CHAR3 VALUE '001',
             DEBIT   TYPE CHAR3 VALUE '002',
             CREDIT  TYPE CHAR3 VALUE '003',
             PAYEE   TYPE CHAR3 VALUE '004',
             TRAILER TYPE CHAR3 VALUE '999',
           END OF GC_SCB_RECTYPE.

CONSTANTS: BEGIN OF GC_SMB_RECTYPE,
             ENV TYPE CHAR3 VALUE 'ENV',
             INS TYPE CHAR3 VALUE 'INS',
             INV TYPE CHAR3 VALUE 'INV',
           END OF GC_SMB_RECTYPE.

CONSTANTS: BEGIN OF GC_SMB_BANKCHARGE,
             APPLI TYPE CHAR1 VALUE 'A',
             BENEF TYPE CHAR1 VALUE 'B',
           END OF GC_SMB_BANKCHARGE.

*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: GT_RESULT      TYPE TT_RESULT                             ##NEEDED,
      GT_RESULT_LOG  TYPE TT_RESULT_LOG                         ##NEEDED,
      GT_SUM_PAY     TYPE TT_SUM_PAY                            ##NEEDED,
      GT_SUM_PAY_WHT TYPE TT_SUM_PAY_WHT                        ##NEEDED.

*----------------------------------------------------------------------*
* WORK AREAS
*----------------------------------------------------------------------*
DATA:
  GS_PROC TYPE TS_PROC_INFO                                  ##NEEDED.

*----------------------------------------------------------------------*
* DATA DECLARATION
*----------------------------------------------------------------------*
DATA: GF_FILENAME       TYPE STRING                          ##NEEDED,
      GF_HOUSE_BANK_KEY TYPE T012-BANKL                      ##NEEDED.

*----------------------------------------------------------------------*
* RANGE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* FIELD-SYMBOLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GENC VARIABLES
*----------------------------------------------------------------------*
DATA: GS_GENC_SCB         TYPE TS_GENC_SCB                           ##NEEDED,
      GS_GENC_SMB         TYPE TS_GENC_SMB                           ##NEEDED,
      GRT_HBKID           TYPE TABLE OF FCLM_S_RANGE_HBKID           ##NEEDED,
      GRT_BANK_GL_ACCOUNT TYPE TRGR_GL_ACCOUNT                       ##NEEDED,
      GRT_VENGRP_EMP      TYPE TT_VENGRP_EMP_RANGE                   ##NEEDED,
      GT_HBKID_H2H        TYPE TT_HBKID_H2H                          ##NEEDED.

*----------------------------------------------------------------------*
* ALV VARIABLES
*----------------------------------------------------------------------*
CONSTANTS:
  GC_STRUCTURE_1 TYPE  TABNAME  VALUE 'ZSDSFIS100',
  GC_STRUCTURE_2 TYPE  TABNAME  VALUE 'ZSDSFIT047'.

CONSTANTS:
  GC_HEADER_HEIGHT_1 TYPE  I VALUE 18,
  GC_ALV_HEIGHT_1    TYPE  I VALUE 82,
  GC_HEADER_HEIGHT_2 TYPE  I VALUE 0,
  GC_ALV_HEIGHT_2    TYPE  I VALUE 100.

*----------------------------------------------------------------------*
* MACROS
*----------------------------------------------------------------------*
DEFINE MC_SHOW_PROGRESS.
  IF SY-BATCH IS INITIAL.
*   Show progress online
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = &1 ##NUMBER_OK
        TEXT       = &2.
  ELSE.
*   Show message step in background
    MESSAGE I000(38) WITH &2 SPACE SPACE SPACE.
  ENDIF.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* Text-s05: Mode
SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-S05.
  PARAMETERS: RB_EXPRT RADIOBUTTON GROUP G3 USER-COMMAND MOD DEFAULT 'X',
              RB_UNLCK RADIOBUTTON GROUP G3.
SELECTION-SCREEN END OF BLOCK B5.

* Text-s01: General selections
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
  PARAMETERS:
    P_BUKRS TYPE BKPF-BUKRS MEMORY ID BUK OBLIGATORY MODIF ID M1,
    P_ZLSCH TYPE T042Z-ZLSCH OBLIGATORY DEFAULT 'T' MODIF ID M1,
    P_HBKID TYPE T012K-HBKID OBLIGATORY MODIF ID M1,
    P_HKTID TYPE T012K-HKTID OBLIGATORY MODIF ID M1.
SELECTION-SCREEN END OF BLOCK B1.

* Text-s07: SCB File Format (For House Bank start with SCB only)
SELECTION-SCREEN BEGIN OF BLOCK B6 WITH FRAME TITLE TEXT-S07.
  PARAMETERS: RB_DIREC TYPE CHAR1 RADIOBUTTON GROUP G5 MODIF ID SCB,
              RB_SMART TYPE CHAR1 RADIOBUTTON GROUP G5 MODIF ID SCB.
SELECTION-SCREEN END OF BLOCK B6.

* Text-s02: Payment selections
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.

* Automatic payment
  PARAMETERS: RB_AUTO TYPE CHAR1 RADIOBUTTON GROUP G1 USER-COMMAND OPT DEFAULT 'X' MODIF ID M1.
  PARAMETERS: P_LAUFD  TYPE REGUH-LAUFD MODIF ID AUT.
  SELECT-OPTIONS: S_LAUFI FOR REGUH-LAUFI MODIF ID AUT.

* Manual payment
  PARAMETERS: RB_MAN  TYPE CHAR1 RADIOBUTTON GROUP G1 MODIF ID M1.
  SELECT-OPTIONS: S_BELNR  FOR BKPF-BELNR MODIF ID MAN.
  PARAMETERS: P_GJAHR TYPE BKPF-GJAHR MODIF ID MAN.
  PARAMETERS: CB_NETAR AS CHECKBOX MODIF ID MAN.              "CH01+

* Bank Charge
  SELECTION-SCREEN BEGIN OF BLOCK B2_1 WITH FRAME TITLE TEXT-S06.
    PARAMETERS:
      RB_APPLI TYPE CHAR1 RADIOBUTTON GROUP G2 MODIF ID M1,
      RB_BENEF TYPE CHAR1 RADIOBUTTON GROUP G2 DEFAULT 'X' MODIF ID M1.
  SELECTION-SCREEN END OF BLOCK B2_1.

  PARAMETERS:
    P_VENDAT TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY MODIF ID M1.
SELECTION-SCREEN END OF BLOCK B2.

* Text-s03: Output File Path
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
  PARAMETERS: RB_LOCAL RADIOBUTTON GROUP G4 USER-COMMAND FIL DEFAULT 'X' MODIF ID M1,
              RB_H2H   RADIOBUTTON GROUP G4 MODIF ID M1.
  PARAMETERS:
    P_SVPATH TYPE STRING LOWER CASE MODIF ID H2H,
    P_OFILE  TYPE STRING LOWER CASE MODIF ID LOC.
SELECTION-SCREEN END OF BLOCK B3.

* Text-s04: Unlock document
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-S04.
  PARAMETERS: P_BUKRS2 TYPE BKPF-BUKRS MEMORY ID BUK OBLIGATORY MODIF ID M2.
  SELECT-OPTIONS: S_PAYDOC FOR BKPF-BELNR MODIF ID M2.
  PARAMETERS: P_PAYYR TYPE BKPF-GJAHR MODIF ID M2.
SELECTION-SCREEN END OF BLOCK B4.

PARAMETERS: CB_SPLIT TYPE CHAR1 AS CHECKBOX DEFAULT SPACE MODIF ID M1,
            P_MAXINV TYPE I DEFAULT 300 MODIF ID M1.

PARAMETERS: CB_TEST  TYPE CHAR1 AS CHECKBOX DEFAULT 'X'.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_AUTHORIZE_CHECK USING GC_TCODE.
  PERFORM F_GET_CONSTANTS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Set Selection Screen Format
  PERFORM F_SET_SELECTION_SCREEN_FORMAT.

* Display value for Host to Host AL11 folder
  PERFORM F_SHOW_HOST_TO_HOST_PATH CHANGING P_SVPATH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_OFILE.
* List Output File
  PERFORM F_DIRECTORY_BROWSE CHANGING P_OFILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PAYDOC-LOW.
* F4 Help for payment document
  PERFORM F_F4_PAYDOC USING 'S_PAYDOC-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PAYDOC-HIGH.
* F4 Help for payment document
  PERFORM F_F4_PAYDOC USING 'S_PAYDOC-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LAUFI-LOW.
  PERFORM F_F4_LAUFI USING 'S_LAUFI-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_LAUFI-HIGH.
  PERFORM F_F4_LAUFI USING 'S_LAUFI-HIGH'.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM EQ 'ONLI'.
    PERFORM F_VALIDATE_SELECTION_SCREEN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR: GS_PROC.

  CASE GC_TRUE.
    WHEN RB_EXPRT.
      PERFORM F_GET_DATA CHANGING GT_RESULT ##PERF_GLOBAL_PAR_OK.
    WHEN RB_UNLCK.
      PERFORM F_GET_DATA_UNLOCK CHANGING GT_RESULT_LOG ##PERF_GLOBAL_PAR_OK.
  ENDCASE.

  IF GT_RESULT IS INITIAL AND
     GT_RESULT_LOG IS INITIAL.
*   Message: No data found.
    MESSAGE S001(ZSDSCA01).
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

  CASE GC_TRUE.
    WHEN RB_EXPRT.
*     Generate file
      IF CB_TEST EQ SPACE.
        CASE P_HBKID(3).
          WHEN GC_HBKID-SMB.
            PERFORM F_SMB_GENERATE_FILE USING GT_RESULT
                                        CHANGING GF_FILENAME.

          WHEN GC_HBKID-SCB.
            PERFORM F_SCB_GENERATE_FILE USING GT_RESULT
                                        CHANGING GF_FILENAME.
        ENDCASE.

*        PERFORM F_UPDATE_LOG_TABLE USING GT_RESULT
*                                         GF_FILENAME.
      ENDIF.

*     Display Processing Result
      PERFORM F_DISPLAY_RESULT USING GT_RESULT.

    WHEN RB_UNLCK.

      IF CB_TEST EQ SPACE.
        PERFORM F_UNLOCK_DOCUMENT CHANGING GT_RESULT_LOG.
      ENDIF.

*     Display Processing Result
      PERFORM F_DISPLAY_RESULT_UNLOCK USING GT_RESULT_LOG.

  ENDCASE.
*----------------------------------------------------------------------*
* SUBROUTINE INCLUDES
*----------------------------------------------------------------------*
  INCLUDE ZSDSCAI9990 ##INCL_OK.

*----------------------------------------------------------------------*
* SUBROUTINES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Form F_AUTHORIZE_CHECK
*----------------------------------------------------------------------*
*  Check Authorization on t-code
*----------------------------------------------------------------------*
FORM F_AUTHORIZE_CHECK USING UF_TCODE  TYPE  SY-TCODE.

  AUTHORITY-CHECK OBJECT 'S_TCODE'     "Transaction Code Check
  ID 'TCD' FIELD UF_TCODE.

  IF SY-SUBRC <> 0.
*   Error You are not authorized to use transaction &
    MESSAGE S172(00) WITH UF_TCODE.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DIRECTORY_BROWSE
*----------------------------------------------------------------------*
*  Browse directory in PC
*----------------------------------------------------------------------*
FORM F_DIRECTORY_BROWSE  CHANGING CF_FILEPATH  TYPE  STRING.

* Initialize Output
  CLEAR CF_FILEPATH.

* Calling standard method to browse a directory.
  CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE(
    CHANGING
      SELECTED_FOLDER = CF_FILEPATH
    EXCEPTIONS
      CNTL_ERROR           = 1                " Control error
      ERROR_NO_GUI         = 2                " No GUI available
      NOT_SUPPORTED_BY_GUI = 3                " GUI does not support this
      OTHERS               = 4 ).

  IF SY-SUBRC NE 0.
    CLEAR CF_FILEPATH.
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_VALIDATE_SELECTION_SCREEN
*----------------------------------------------------------------------*
*  Validate Selection screen
*----------------------------------------------------------------------*
FORM F_VALIDATE_SELECTION_SCREEN .

  CASE ABAP_TRUE.
*----------------------------
*   Export text file
*----------------------------
    WHEN RB_EXPRT.

* BOI - CH01 - 14.11.2024
      IF P_HBKID(3) NE GC_HBKID-SMB.
        CLEAR: CB_NETAR.
      ENDIF.
* EOI - CH01 - 14.11.2024

*     Validate house bank id
      IF GRT_HBKID IS INITIAL.
*       House bank ID in configuration is missing
        MESSAGE E011(ZSDSFI01).
        RETURN.
      ELSEIF P_HBKID NOT IN GRT_HBKID.
*       House bank id & not found in configuration
        MESSAGE E012(ZSDSFI01) WITH P_HBKID.
        RETURN.
      ENDIF.

*     Validate application server or PC file option
      IF RB_LOCAL IS NOT INITIAL AND
         CB_TEST EQ SPACE AND
         P_OFILE IS INITIAL.
        SET CURSOR FIELD 'P_OFILE'.
*       Error: Please enter a valid filename.
        MESSAGE E002(ZSDSCA01).
        RETURN.
      ENDIF.

      IF RB_LOCAL IS NOT INITIAL AND
        SY-BATCH EQ GC_TRUE.
*       Error: Cannot process local file in background mode.
        MESSAGE E012(ZSDSCA01).
        RETURN.
      ENDIF.

* BOI - CH01 - 14.11.2024
      IF RB_H2H EQ GC_TRUE AND
         CB_NETAR EQ GC_TRUE.
*       Error: Output for option Net AP AR to Local File only
        MESSAGE E998(ZSDSFI01) WITH TEXT-E07.
        RETURN.
      ENDIF.
* EOI - CH01 - 14.11.2024

*     Validate automatic payment or manual payment option
      IF RB_AUTO EQ GC_TRUE.
        IF P_LAUFD IS INITIAL OR
           S_LAUFI IS INITIAL.
*         Make an entry in mandatory field "&"
          MESSAGE E278(00) WITH TEXT-E01.
          RETURN.
        ENDIF.
      ELSE.
        IF S_BELNR IS INITIAL OR
           P_GJAHR IS INITIAL.
*         Make an entry in mandatory field "&"
          MESSAGE E278(00) WITH TEXT-E01.
          RETURN.
        ENDIF.
      ENDIF.

*     Is house bank id is applicable for Host to Host option
      IF RB_H2H IS NOT INITIAL AND
         P_HBKID IS NOT INITIAL.
        READ TABLE GT_HBKID_H2H WITH KEY HOUSE_BANK = P_HBKID(3)
                                TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
*         House bank is not maintain for Host to Host
          MESSAGE E998(ZSDSFI01) WITH TEXT-E04.
          RETURN.
        ENDIF.
      ENDIF.

      IF P_VENDAT IS NOT INITIAL AND
         P_VENDAT < SY-DATUM.
*       Value date must greater than or equal to system date
        MESSAGE E998(ZSDSFI01) WITH TEXT-E06.
        RETURN.
      ENDIF.

*----------------------------
*   Unlock document
*----------------------------
    WHEN RB_UNLCK.
      IF S_PAYDOC IS INITIAL OR
         P_BUKRS2 IS INITIAL OR
         P_PAYYR IS INITIAL.
*       Make an entry in mandatory field "&"
        MESSAGE E278(00) WITH TEXT-E02.
        RETURN.
      ENDIF.
  ENDCASE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_GET_CONSTANTS
*----------------------------------------------------------------------*
*  Get GenC Constants
*----------------------------------------------------------------------*
FORM F_GET_CONSTANTS .

  CONSTANTS:
    LC_HOUSE_BANK_ID           TYPE ZSDSDE_PARAM_NAME VALUE 'HOUSE_BANK_ID',
    LC_BANK_GL_ACCOUNT         TYPE ZSDSDE_PARAM_NAME VALUE 'BANK_GL_ACCOUNT',
    LC_VENDOR_GROUP_EMPLOYEE   TYPE ZSDSDE_PARAM_NAME VALUE 'VENDOR_GROUP_EMPLOYEE',
    LC_SCB_COMPANY_ID          TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_COMPANY_ID',
    LC_SCB_CHANNEL_ID          TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_CHANNEL_ID',
    LC_SCB_PRODUCT_CODE        TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_PRODUCT_CODE',
    LC_SCB_INTERNAL_REF        TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_INTERNAL_REF',
    LC_SCB_PRODUCT_CODE_DIRECT TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_PRODUCT_CODE_DIRECT ',
    LC_SCB_WHT_PRESENT         TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_WHT_PRESENT',
    LC_SCB_INV_PRESENT         TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_INV_PRESENT',
    LC_SCB_CREDIT_ADVICE_REQ   TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_CREDIT_ADVICE_REQ',
    LC_SCB_DELIVERY_MODE       TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_DELIVERY_MODE',
    LC_SCB_RECV_BANK_NAME      TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_RECV_BANK_NAME',
    LC_SCB_BENEFICIARY_NOTI    TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_BENEFICIARY_NOTI',
    LC_SCB_SERVICE_TYPE        TYPE ZSDSDE_PARAM_NAME VALUE 'SCB_SERVICE_TYPE',
    LC_SMB_ENV_SM              TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_SM',
    LC_SMB_ENV_TRANSFERTYPE    TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_TRANSFERTYPE',
    LC_SMB_ENV_DRBRANCH        TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_DRBRANCH',
    LC_SMB_ENV_DRGL            TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_DRGL',
    LC_SMB_ENV_DRGLSUB         TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_DRGLSUB',
    LC_SMB_ENV_DRSSNO          TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_DRSSNO',
    LC_SMB_ENV_DRSUBNO         TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_DRSUBNO',
    LC_SMB_ENV_DRMKT           TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_DRMKT',
    LC_SMB_ENV_CHARGEBRNCH     TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_CHARGEBRNCH',
    LC_SMB_ENV_CHARGEGL        TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_CHARGEGL',
    LC_SMB_ENV_CHARGEGLSUB     TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_CHARGEGLSUB',
    LC_SMB_ENV_CHARGESS        TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_CHARGESS',
    LC_SMB_ENV_CHARGESUB       TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_CHARGESUB',
    LC_SMB_ENV_CHARGEMKT       TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_ENV_CHARGEMKT',
    LC_SMB_INS_EMAILNOTIF      TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_INS_EMAILNOTIF',
    LC_SMB_INS_FAXNOTIF        TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_INS_FAXNOTIF',
    LC_SMB_INS_CALCMODEID      TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_INS_CALCMODEID',
    LC_SMB_INS_TRANSACTIONCD   TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_INS_TRANSACTIONCD',
    LC_SMB_INS_PAYMENTBYID     TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_INS_PAYMENTBYID',
    LC_SMB_INV_CALCFOR         TYPE ZSDSDE_PARAM_NAME VALUE 'SMB_INV_CALCFOR',
    LC_HOST2HOST_INTFNO        TYPE ZSDSDE_PARAM_NAME VALUE 'HOST2HOST_INTFNO'.

  STATICS:
    LF_READ TYPE FLAG.

  DATA:
    LT_GENC TYPE ZCL_SDSCA_UTILITIES=>TT_GEN_C.

  DATA:
    LF_REPID TYPE PROGRAMM.

* Check Already Read?
  IF LF_READ EQ GC_TRUE.
    RETURN.
  ENDIF.

* Initialize Output
  CLEAR: GRT_HBKID,
         GRT_BANK_GL_ACCOUNT,
         GRT_VENGRP_EMP,
         GS_GENC_SCB,
         GS_GENC_SMB,
         GT_HBKID_H2H.

* Assign REPID
  LF_REPID = SY-REPID.

* Read All GenC constants for program
  CALL METHOD ZCL_SDSCA_UTILITIES=>GET_GEN_C
    EXPORTING
      IF_REPID = LF_REPID
    IMPORTING
      ET_GEN_C = LT_GENC.

* Mark Read Flag
  LF_READ = GC_TRUE.

* Assign GenC Constants
  LOOP AT LT_GENC ASSIGNING FIELD-SYMBOL(<L_GENC>).

    CASE <L_GENC>-PARAM.
      WHEN LC_HOUSE_BANK_ID.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                OPTION = <L_GENC>-PARAM_OPTION
                LOW    = <L_GENC>-VALUE_LOW
                HIGH   = <L_GENC>-VALUE_HIGH )
       INTO TABLE GRT_HBKID.

*     Bank GL Account
      WHEN LC_BANK_GL_ACCOUNT.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_BANK_GL_ACCOUNT.

*     Employee vendor group
      WHEN LC_VENDOR_GROUP_EMPLOYEE.
        INSERT VALUE #( SIGN   = <L_GENC>-PARAM_SIGN
                        OPTION = <L_GENC>-PARAM_OPTION
                        LOW    = <L_GENC>-VALUE_LOW
                        HIGH   = <L_GENC>-VALUE_HIGH )
               INTO TABLE GRT_VENGRP_EMP.

*     Fix value for SCB bank
      WHEN LC_SCB_COMPANY_ID.
        GS_GENC_SCB-SCB_COMPANY_ID = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_CHANNEL_ID.
        GS_GENC_SCB-SCB_CHANNEL_ID = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_PRODUCT_CODE.
        GS_GENC_SCB-SCB_PRODUCT_CODE = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_INTERNAL_REF.
        GS_GENC_SCB-SCB_INTERNAL_REF = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_PRODUCT_CODE_DIRECT.
        GS_GENC_SCB-SCB_PRODUCT_CODE_DIRECT = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_WHT_PRESENT.
        GS_GENC_SCB-SCB_WHT_PRESENT = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_INV_PRESENT.
        GS_GENC_SCB-SCB_INV_PRESENT = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_CREDIT_ADVICE_REQ.
        GS_GENC_SCB-SCB_CREDIT_ADVICE_REQ = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_DELIVERY_MODE.
        GS_GENC_SCB-SCB_DELIVERY_MODE = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_RECV_BANK_NAME.
        GS_GENC_SCB-SCB_RECV_BANK_NAME = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_BENEFICIARY_NOTI.
        GS_GENC_SCB-SCB_BENEFICIARY_NOTI = <L_GENC>-VALUE_LOW.
      WHEN LC_SCB_SERVICE_TYPE.
        GS_GENC_SCB-SCB_SERVICE_TYPE = <L_GENC>-VALUE_LOW.

*     Fix value for SMBC bank
      WHEN LC_SMB_ENV_SM.
        GS_GENC_SMB-SMB_ENV_SM = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_TRANSFERTYPE.
        GS_GENC_SMB-SMB_ENV_TRANSFERTYPE = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_DRBRANCH.
        GS_GENC_SMB-SMB_ENV_DRBRANCH = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_DRGL.
        GS_GENC_SMB-SMB_ENV_DRGL = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_DRGLSUB.
        GS_GENC_SMB-SMB_ENV_DRGLSUB = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_DRSSNO.
        GS_GENC_SMB-SMB_ENV_DRSSNO = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_DRSUBNO.
        GS_GENC_SMB-SMB_ENV_DRSUBNO = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_DRMKT.
        GS_GENC_SMB-SMB_ENV_DRMKT = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_CHARGEBRNCH.
        GS_GENC_SMB-SMB_ENV_CHARGEBRNCH = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_CHARGEGL.
        GS_GENC_SMB-SMB_ENV_CHARGEGL = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_CHARGEGLSUB.
        GS_GENC_SMB-SMB_ENV_CHARGEGLSUB = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_CHARGESS.
        GS_GENC_SMB-SMB_ENV_CHARGESS = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_CHARGESUB.
        GS_GENC_SMB-SMB_ENV_CHARGESUB = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_ENV_CHARGEMKT.
        GS_GENC_SMB-SMB_ENV_CHARGEMKT = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_INS_EMAILNOTIF.
        GS_GENC_SMB-SMB_INS_EMAILNOTIF = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_INS_FAXNOTIF.
        GS_GENC_SMB-SMB_INS_FAXNOTIF = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_INS_CALCMODEID.
        GS_GENC_SMB-SMB_INS_CALCMODEID = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_INS_TRANSACTIONCD.
        GS_GENC_SMB-SMB_INS_TRANSACTIONCD = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_INS_PAYMENTBYID.
        GS_GENC_SMB-SMB_INS_PAYMENTBYID = <L_GENC>-VALUE_LOW.
      WHEN LC_SMB_INV_CALCFOR.
        GS_GENC_SMB-SMB_INV_CALCFOR = <L_GENC>-VALUE_LOW.

*     HOST2HOST_INTFNO
      WHEN LC_HOST2HOST_INTFNO.
        INSERT VALUE #( HOUSE_BANK  = <L_GENC>-PARAM_EXT
                        INTFNO      = <L_GENC>-VALUE_LOW )
               INTO TABLE GT_HBKID_H2H.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_SET_SELECTION_SCREEN_FORMAT
*----------------------------------------------------------------------*
*  Set Selection Screen Format
*----------------------------------------------------------------------*
FORM F_SET_SELECTION_SCREEN_FORMAT .
  LOOP AT SCREEN.
    CASE SCREEN-GROUP1.
*     Automatic payment
      WHEN 'AUT'.
        IF RB_AUTO EQ GC_TRUE AND RB_EXPRT EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
          SCREEN-REQUIRED = 2.
        ELSE.
          SCREEN-ACTIVE = 0.
          SCREEN-REQUIRED = 0.
        ENDIF.
        MODIFY SCREEN.

*     Manual payment
      WHEN 'MAN'.
        IF RB_MAN EQ GC_TRUE AND RB_EXPRT EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
          SCREEN-REQUIRED = 2.
* BOI - CH01 - 14.11.2024
          IF SCREEN-NAME = 'CB_NETAR'.
            IF P_HBKID(3) EQ GC_HBKID-SCB.
              SCREEN-ACTIVE = 0.
              SCREEN-REQUIRED = 0.
            ENDIF.
          ENDIF.
* EOI - CH01 - 14.11.2024
        ELSE.
          SCREEN-ACTIVE = 0.
          SCREEN-REQUIRED = 0.
        ENDIF.
        MODIFY SCREEN.

*     Export document
      WHEN 'M1'.
        IF RB_EXPRT EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
          SCREEN-REQUIRED = 2.
        ELSE.
          SCREEN-ACTIVE = 0.
          SCREEN-REQUIRED = 0.
        ENDIF.
        MODIFY SCREEN.

*     Unlock document
      WHEN 'M2'.
        IF RB_UNLCK EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
          SCREEN-REQUIRED = 2.
        ELSE.
          SCREEN-ACTIVE = 0.
          SCREEN-REQUIRED = 0.
        ENDIF.
        MODIFY SCREEN.

*     Local file
      WHEN 'LOC'.
        IF RB_EXPRT EQ GC_TRUE AND
           RB_LOCAL EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
          SCREEN-REQUIRED = 2.
        ELSE.
          SCREEN-ACTIVE = 0.
          SCREEN-REQUIRED = 0.
        ENDIF.
        MODIFY SCREEN.

*     Host to Host
      WHEN 'H2H'.
        IF RB_EXPRT EQ GC_TRUE AND
           RB_H2H EQ GC_TRUE.
          SCREEN-ACTIVE = 1.
          SCREEN-INPUT = 0.
        ELSE.
          SCREEN-ACTIVE = 0.
          SCREEN-INPUT = 0.
        ENDIF.
        MODIFY SCREEN.

*     SCB File Format
      WHEN 'SCB'.
        IF RB_EXPRT EQ GC_TRUE AND P_HBKID(3) EQ GC_HBKID-SCB.
          SCREEN-ACTIVE = 1.
        ELSE.
          SCREEN-ACTIVE = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.

    CASE SCREEN-NAME.
      WHEN 'CB_SPLIT' OR
           'P_MAXINV'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_SHOW_HOST_TO_HOST_PATH
*---------------------------------------------------------------------*
* Show AL11 folder for option Host to Host
*---------------------------------------------------------------------*
FORM F_SHOW_HOST_TO_HOST_PATH CHANGING CF_SVPATH TYPE STRING.
  CONSTANTS:
    LC_INBOUND  TYPE  TEXT20 VALUE '10_INBOUND',
    LC_OUTBOUND TYPE  TEXT20 VALUE '20_OUTBOUND'.

  DATA:
    LF_FILENAME   TYPE STRING,
    LF_FILENAME_I TYPE STRING.

  CLEAR: CF_SVPATH.

  IF RB_H2H IS INITIAL OR
     RB_EXPRT IS INITIAL OR
     P_HBKID IS INITIAL.
    CLEAR: CB_SPLIT.
    RETURN.
  ENDIF.

  IF P_HBKID(3) EQ GC_HBKID-SMB.
    CB_SPLIT = 'X'.
  ELSE.
    CLEAR: CB_SPLIT.
  ENDIF.

  READ TABLE GT_HBKID_H2H INTO DATA(LS_H2H)
                          WITH KEY HOUSE_BANK = P_HBKID(3).
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

* Get Interface Setting
  SELECT SINGLE *
    FROM ZSDSCAC004
   WHERE INTFNO   EQ @LS_H2H-INTFNO
     AND SYSID    EQ @SY-SYSID
    INTO @DATA(LS_SETTING).
  IF SY-SUBRC NE 0.
    SELECT SINGLE *
      FROM ZSDSCAC004
     WHERE INTFNO   EQ @LS_H2H-INTFNO
       AND SYSID    EQ @SPACE
      INTO @LS_SETTING.
  ENDIF.

  IF LS_SETTING IS INITIAL.
    RETURN.
  ENDIF.

  CASE LS_SETTING-INOUT_FLAG.
    WHEN 'I'.
      LF_FILENAME_I = LC_INBOUND.
    WHEN 'O'.
      LF_FILENAME_I = LC_OUTBOUND.
  ENDCASE.
  LF_FILENAME_I = LS_SETTING-SYSNAME && '/' &&
                  LS_SETTING-WRICEF.

  CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
    EXPORTING
      LOGICAL_PATH               = LS_SETTING-PATHINTERN
      OPERATING_SYSTEM           = SY-OPSYS
      FILE_NAME                  = LF_FILENAME_I
      ELEMINATE_BLANKS           = 'X'
    IMPORTING
      FILE_NAME_WITH_PATH        = LF_FILENAME
    EXCEPTIONS
      PATH_NOT_FOUND             = 1
      MISSING_PARAMETER          = 2
      OPERATING_SYSTEM_NOT_FOUND = 3
      FILE_SYSTEM_NOT_FOUND      = 4
      OTHERS                     = 5.
  IF SY-SUBRC EQ 0 .
    CF_SVPATH = LF_FILENAME  .
  ENDIF.

ENDFORM.
*----------------------------------------------------------------------*
*  Form  F_GET_DATA
*----------------------------------------------------------------------*
*  Get data for process
*----------------------------------------------------------------------*
FORM F_GET_DATA CHANGING CT_RESULT TYPE TT_RESULT.

  CLEAR: GT_RESULT.

  PERFORM F_GET_HOUSE_BANK_KEY CHANGING GF_HOUSE_BANK_KEY.

  CASE GC_TRUE.
    WHEN RB_AUTO.
*     Automatic payment
      PERFORM F_GET_DATA_AUTOMATIC CHANGING CT_RESULT.

    WHEN RB_MAN.
*     Manual payment
      PERFORM F_GET_DATA_MANUAL CHANGING CT_RESULT.

  ENDCASE.

* Filter out payment document which already exported to file
  PERFORM F_FILTER_EXPORTED_DOC CHANGING CT_RESULT.

  SORT CT_RESULT BY PAYDOC_GJAHR
                    PAYDOC_BELNR
                    INVDOC_GJAHR
                    INVDOC_BELNR.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_DATA_AUTOMATIC
*---------------------------------------------------------------------*
* Get data for option Automatic payment
*---------------------------------------------------------------------*
FORM F_GET_DATA_AUTOMATIC CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LT_REGUH          TYPE TT_REGUH,
        LT_REGUP          TYPE TT_REGUP,
        LT_LIFNR          TYPE TT_LIFNR,
        LT_ADRNR          TYPE TT_ADRNR,
        LT_PAYDOC_KEY     TYPE TT_FIDOC_KEY,
        LT_INVDOC_KEY     TYPE TT_FIDOC_KEY,
        LT_VENDOR_DETAIL  TYPE TT_VENDOR_DETAIL,
        LT_ONETIME_DETAIL TYPE TT_ONETIME_DETAIL,
        LT_ADDR_DETAIL    TYPE TT_ADDR_DETAIL,
        LT_INVDOC_DETAIL  TYPE TT_INVDOC_DETAIL_AUTO,
        LT_WHT_DETAIL     TYPE TT_WHT_DETAIL.

  CLEAR: CT_RESULT.

  PERFORM F_GET_REGUH CHANGING LT_REGUH
                               LT_LIFNR
                               LT_PAYDOC_KEY.

  PERFORM F_GET_REGUP USING LT_REGUH
                      CHANGING LT_REGUP
                               LT_INVDOC_KEY.

  PERFORM F_GET_VENDOR_DETAIL USING LT_LIFNR
                           CHANGING LT_VENDOR_DETAIL
                                    LT_ADRNR.

  PERFORM F_GET_ONETIME_VENDOR USING LT_PAYDOC_KEY
                            CHANGING LT_ONETIME_DETAIL
                                     LT_ADRNR.

  PERFORM F_GET_ADDR_DETAIL USING LT_ADRNR
                         CHANGING LT_ADDR_DETAIL.

  PERFORM F_GET_WHT_DETAIL USING LT_PAYDOC_KEY
                        CHANGING LT_WHT_DETAIL.

  PERFORM F_GET_INVOICE_DETAIL_AUTO USING LT_INVDOC_KEY
                                 CHANGING LT_INVDOC_DETAIL.

  PERFORM F_COLLECT_RESULT_AUTO USING LT_REGUH
                                      LT_REGUP
                                      LT_VENDOR_DETAIL
                                      LT_ONETIME_DETAIL
                                      LT_ADDR_DETAIL
                                      LT_WHT_DETAIL
                                      LT_INVDOC_DETAIL
                             CHANGING CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_REGUH
*---------------------------------------------------------------------*
* Get Settlement data from payment program
*---------------------------------------------------------------------*
FORM F_GET_REGUH  CHANGING CT_REGUH TYPE TT_REGUH
                           CT_LIFNR TYPE TT_LIFNR
                           CT_PAYDOC_KEY TYPE TT_FIDOC_KEY.

  DATA: LF_TABIX TYPE SY-TABIX,
        LS_LIFNR TYPE TS_LIFNR.

  FIELD-SYMBOLS: <L_REGUH> TYPE TS_REGUH.

  CLEAR: CT_REGUH, CT_LIFNR, CT_PAYDOC_KEY.

  SELECT LAUFD,
         LAUFI,
         XVORL,
         ZBUKR,
         LIFNR,
         KUNNR,
         EMPFG,
         VBLNR,
         WAERS,
         HBKID,
         HKTID,
         UBKNT,
         ZALDT,
         RBETR,
         RWBTR,
         VALUT,
         RZAWE,
         UBHKT                       ##TOO_MANY_ITAB_FIELDS
  FROM REGUH
  INTO TABLE @CT_REGUH
  WHERE LAUFD EQ @P_LAUFD
    AND LAUFI IN @S_LAUFI
    AND ZBUKR EQ @P_BUKRS
    AND RZAWE EQ @P_ZLSCH
    AND XVORL EQ @SPACE
    AND HBKID EQ @P_HBKID "@GRT_HBKID
    AND HKTID EQ @P_HKTID
    ORDER BY PRIMARY KEY.

* Get year from Value date in REGUH
  LOOP AT CT_REGUH ASSIGNING <L_REGUH>.
*    <L_REGUH>-GJAHR = <L_REGUH>-VALUT(4).   "CH02-

*   BOI - 420000241 - CH02 - 14.01.2025
*   Determine FI Fiscal Year from posting date
    PERFORM F_FI_PERIOD_DETERMINE USING <L_REGUH>-ZALDT
                                  CHANGING <L_REGUH>-GJAHR.
*   EOI - 420000241 - CH02 - 14.01.2025
  ENDLOOP.

  IF CT_REGUH IS NOT INITIAL.
    SELECT BUKRS,
           BELNR,
           GJAHR
      FROM BKPF
      INTO TABLE @DATA(LT_BKPF)
      FOR ALL ENTRIES IN @CT_REGUH
      WHERE BUKRS EQ @CT_REGUH-ZBUKR
        AND BELNR EQ @CT_REGUH-VBLNR
        AND GJAHR EQ @CT_REGUH-GJAHR
        AND XREVERSAL EQ @SPACE.

    SORT LT_BKPF BY BUKRS BELNR GJAHR.
  ENDIF.

* Check no reversal document
  LOOP AT CT_REGUH ASSIGNING <L_REGUH>.
    CLEAR: LS_LIFNR.

    LF_TABIX = SY-TABIX.
    READ TABLE LT_BKPF WITH KEY BUKRS = <L_REGUH>-ZBUKR
                                BELNR = <L_REGUH>-VBLNR
                                GJAHR = <L_REGUH>-GJAHR
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      DELETE CT_REGUH INDEX LF_TABIX.
      CONTINUE.
    ENDIF.

    LS_LIFNR-LIFNR = <L_REGUH>-LIFNR.
    COLLECT LS_LIFNR INTO CT_LIFNR.
  ENDLOOP.

* Keep document key
  CT_PAYDOC_KEY = CORRESPONDING #( CT_REGUH MAPPING BUKRS = ZBUKR
                                                    BELNR = VBLNR
                                                    GJAHR = GJAHR ).

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_REGUP
*---------------------------------------------------------------------*
* Get Processed items from payment program
*---------------------------------------------------------------------*
FORM F_GET_REGUP USING UT_REGUH TYPE TT_REGUH
                 CHANGING CT_REGUP TYPE TT_REGUP
                          CT_INVDOC_KEY TYPE TT_FIDOC_KEY.

  CLEAR: CT_REGUP, CT_INVDOC_KEY.

  IF UT_REGUH IS NOT INITIAL.
    SELECT LAUFD,
           LAUFI,
           XVORL,
           ZBUKR,
           LIFNR,
           KUNNR,
           EMPFG,
           VBLNR,
           BUKRS,
           BELNR,
           GJAHR,
           BUZEI,
           BLART,
           SHKZG,
           DMBTR,
           XCPDD
    FROM REGUP
    INTO TABLE @CT_REGUP
    FOR ALL ENTRIES IN @UT_REGUH
    WHERE ZBUKR EQ @UT_REGUH-ZBUKR
      AND VBLNR EQ @UT_REGUH-VBLNR
      AND XVORL EQ @SPACE
      ORDER BY PRIMARY KEY.

    IF SY-SUBRC EQ 0.
*     Keep document key
      CT_INVDOC_KEY = CORRESPONDING #( CT_REGUP MAPPING BUKRS = BUKRS
                                                        BELNR = BELNR
                                                        GJAHR = GJAHR
                                                        BUZEI = BUZEI ).
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_VENDOR_DETAIL
*---------------------------------------------------------------------*
* Get vendor master data
*---------------------------------------------------------------------*
FORM F_GET_VENDOR_DETAIL  USING    UT_LIFNR TYPE TT_LIFNR
                          CHANGING CT_VENDOR_DETAIL TYPE TT_VENDOR_DETAIL
                                   CT_ADRNR TYPE TT_ADRNR.

  DATA: LF_HOUSE_BANK TYPE T012-BANKL.

  CLEAR: CT_VENDOR_DETAIL, CT_ADRNR.

  IF UT_LIFNR IS INITIAL.
    RETURN.
  ENDIF.

  CONCATENATE GF_HOUSE_BANK_KEY(3) '%'
   INTO LF_HOUSE_BANK.

  IF P_HBKID(3) EQ GC_HBKID-SCB.
    CASE ABAP_TRUE.
      WHEN RB_DIREC.
*       SCB direct (First 3 digit of SDS house bank key = vendor bank key)
        SELECT LFBK~LIFNR,
               LFBK~BANKS,
               LFBK~BANKL,
               LFBK~BANKN,
               LFBK~BVTYP,
               BNKA~BANKA,
               BNKA~BRNCH,
               LFA1~NAME1,
               LFA1~NAME2,
               LFA1~NAME3,
               LFA1~NAME4,
               LFA1~STCD1,
               LFA1~STCD3,
               LFA1~ADRNR,
               LFA1~KTOKK,
               LFA1~XCPDK,
               TSAD3T~TITLE_MEDI,                      "#EC CI_BUFFJOIN
               BUT000~NAME_FIRST,
               BUT000~NAME_LAST
        FROM LFBK
        INNER JOIN LFA1
          ON LFA1~LIFNR EQ LFBK~LIFNR
        LEFT OUTER JOIN BUT000
          ON ( BUT000~PARTNER EQ LFA1~LIFNR )
        LEFT OUTER JOIN TSAD3T
          ON ( BUT000~TITLE EQ TSAD3T~TITLE AND
               TSAD3T~LANGU EQ 'E' )
        INNER JOIN BNKA
          ON ( BNKA~BANKS EQ LFBK~BANKS AND
               BNKA~BANKL EQ LFBK~BANKL )
        INTO TABLE @CT_VENDOR_DETAIL
        FOR ALL ENTRIES IN @UT_LIFNR
        WHERE LFBK~LIFNR EQ @UT_LIFNR-LIFNR
          AND LFBK~BANKL LIKE @LF_HOUSE_BANK.

      WHEN RB_SMART.
*       SCB SMART (First 3 digit of SDS house bank key <> vendor bank key)
        SELECT LFBK~LIFNR,
               LFBK~BANKS,
               LFBK~BANKL,
               LFBK~BANKN,
               LFBK~BVTYP,
               BNKA~BANKA,
               BNKA~BRNCH,
               LFA1~NAME1,
               LFA1~NAME2,
               LFA1~NAME3,
               LFA1~NAME4,
               LFA1~STCD1,
               LFA1~STCD3,
               LFA1~ADRNR,
               LFA1~KTOKK,
               LFA1~XCPDK,
               TSAD3T~TITLE_MEDI,                      "#EC CI_BUFFJOIN
               BUT000~NAME_FIRST,
               BUT000~NAME_LAST
        FROM LFBK
        INNER JOIN LFA1
          ON LFA1~LIFNR EQ LFBK~LIFNR
        LEFT OUTER JOIN BUT000
          ON ( BUT000~PARTNER EQ LFA1~LIFNR )
        LEFT OUTER JOIN TSAD3T
          ON ( BUT000~TITLE EQ TSAD3T~TITLE AND
               TSAD3T~LANGU EQ 'E' )
        INNER JOIN BNKA
          ON ( BNKA~BANKS EQ LFBK~BANKS AND
               BNKA~BANKL EQ LFBK~BANKL )
        INTO TABLE @CT_VENDOR_DETAIL
        FOR ALL ENTRIES IN @UT_LIFNR
        WHERE LFBK~LIFNR EQ @UT_LIFNR-LIFNR
          AND LFBK~BANKL NOT LIKE @LF_HOUSE_BANK.
    ENDCASE.
  ELSE.

    SELECT LFBK~LIFNR,
           LFBK~BANKS,
           LFBK~BANKL,
           LFBK~BANKN,
           LFBK~BVTYP,
           BNKA~BANKA,
           BNKA~BRNCH,
           LFA1~NAME1,
           LFA1~NAME2,
           LFA1~NAME3,
           LFA1~NAME4,
           LFA1~STCD1,
           LFA1~STCD3,
           LFA1~ADRNR,
           LFA1~KTOKK,
           LFA1~XCPDK,
           TSAD3T~TITLE_MEDI,                          "#EC CI_BUFFJOIN
           BUT000~NAME_FIRST,
           BUT000~NAME_LAST
    FROM LFBK
    INNER JOIN LFA1
      ON LFA1~LIFNR EQ LFBK~LIFNR
    LEFT OUTER JOIN BUT000
      ON ( BUT000~PARTNER EQ LFA1~LIFNR )
    LEFT OUTER JOIN TSAD3T
      ON ( BUT000~TITLE EQ TSAD3T~TITLE AND
           TSAD3T~LANGU EQ 'E' )
    INNER JOIN BNKA
      ON ( BNKA~BANKS EQ LFBK~BANKS AND
           BNKA~BANKL EQ LFBK~BANKL )
    INTO TABLE @CT_VENDOR_DETAIL
    FOR ALL ENTRIES IN @UT_LIFNR
    WHERE LFBK~LIFNR EQ @UT_LIFNR-LIFNR.

  ENDIF.

  IF SY-SUBRC EQ 0.
*   Keep address number key
    CT_ADRNR = CORRESPONDING #( CT_VENDOR_DETAIL MAPPING ADDRNUMBER = ADRNR ).
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_ONETIME_VENDOR
*--------------------------------------------------------------------*
* Get one time vendor information
*---------------------------------------------------------------------*
FORM F_GET_ONETIME_VENDOR  USING    UT_PAYDOC_KEY TYPE TT_FIDOC_KEY
                           CHANGING CT_ONETIME_DETAIL TYPE TT_ONETIME_DETAIL
                                    CT_ADRNR TYPE TT_ADRNR.
  DATA: LS_ADRNR TYPE TS_ADRNR.

  CLEAR: CT_ONETIME_DETAIL.

  IF UT_PAYDOC_KEY IS NOT INITIAL.
    SELECT BSEC~BUKRS,
           BSEC~BELNR,
           BSEC~GJAHR,
           BSEC~BUZEI,
           BSEC~NAME1,
           BSEC~NAME2,
           BSEC~NAME3,
           BSEC~NAME4,
           BSEC~PSTLZ,
           BSEC~ORT01,
           BSEC~STRAS,
           BSEC~BANKN,
           BSEC~BANKL,
           BSEC~BANKS,
           BNKA~BANKA,
           BNKA~BRNCH,
           BSEC~STCD1,
           BSEC~STCD3,
           BSEC~ADRNR,
           BSEC~INTAD
    FROM BSEC                                           "#EC CI_NOORDER
    INNER JOIN BNKA
      ON ( BNKA~BANKS EQ BSEC~BANKS AND
           BNKA~BANKL EQ BSEC~BANKL )
    INTO TABLE @CT_ONETIME_DETAIL
    FOR ALL ENTRIES IN @UT_PAYDOC_KEY
    WHERE BSEC~BUKRS EQ @UT_PAYDOC_KEY-BUKRS
      AND BSEC~BELNR EQ @UT_PAYDOC_KEY-BELNR
      AND BSEC~GJAHR EQ @UT_PAYDOC_KEY-GJAHR.

    IF SY-SUBRC EQ 0.
      SORT CT_ONETIME_DETAIL BY BUKRS BELNR GJAHR BUZEI.

*     Append address number of one time vendor to CT_ADRNR
      LOOP AT CT_ONETIME_DETAIL ASSIGNING FIELD-SYMBOL(<L_ONETIME>).
        CLEAR: LS_ADRNR.
        LS_ADRNR-ADDRNUMBER = <L_ONETIME>-ADRNR.
        COLLECT LS_ADRNR INTO CT_ADRNR.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_ADDR_DETAIL
*&---------------------------------------------------------------------*
*& Get address data from ADRC, ADR6
*&---------------------------------------------------------------------*
FORM F_GET_ADDR_DETAIL  USING    UT_ADRNR TYPE TT_ADRNR
                        CHANGING CT_ADDR_DETAIL TYPE TT_ADDR_DETAIL.

  CLEAR: CT_ADDR_DETAIL.

  CHECK UT_ADRNR IS NOT INITIAL.

  SELECT ADRC~ADDRNUMBER,
         ADRC~DATE_FROM,
         ADRC~NATION,
         ADRC~NAME1,
         ADRC~NAME2,
         ADRC~NAME3,
         ADRC~NAME4,
         ADRC~TEL_NUMBER,
         ADRC~HOUSE_NUM1,
         ADRC~STREET,
         ADRC~STR_SUPPL1,
         ADRC~STR_SUPPL2,
         ADRC~STR_SUPPL3,
         ADRC~CITY2,
         ADRC~CITY1,
         ADRC~POST_CODE1,
         ADRC~PERS_ADDR,
         ADR6~SMTP_ADDR,
         ADR6~CONSNUMBER,
         ADR6~FLGDEFAULT
    FROM ADRC
    LEFT OUTER JOIN ADR6
     ON ADRC~ADDRNUMBER EQ ADR6~ADDRNUMBER
    INTO TABLE @CT_ADDR_DETAIL
    FOR ALL ENTRIES IN @UT_ADRNR
    WHERE ADRC~ADDRNUMBER EQ @UT_ADRNR-ADDRNUMBER
      AND ( NATION EQ @SPACE OR
            NATION EQ @GC_NATION_I ).

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_WHT_DETAIL
*---------------------------------------------------------------------*
* Get withholding tax data
* A payment document will contain only 1 WHT code which not blank
*---------------------------------------------------------------------*
FORM F_GET_WHT_DETAIL  USING    UT_FIDOC_KEY TYPE TT_FIDOC_KEY
                       CHANGING CT_WHT_DETAIL TYPE TT_WHT_DETAIL.

  DATA: LF_LAND1       TYPE T001-LAND1,
        LS_SUM_PAY_WHT TYPE TS_SUM_PAY_WHT.

  CLEAR: CT_WHT_DETAIL.

  IF UT_FIDOC_KEY IS NOT INITIAL.
    SELECT SINGLE LAND1 FROM T001 INTO @LF_LAND1 WHERE BUKRS EQ @P_BUKRS.

    IF SY-SUBRC EQ 0.
*     Get Withholding tax code and text from T059Z and T059ZT
      SELECT *
      FROM T059ZT
      INTO TABLE @DATA(LT_T059ZT)
      WHERE SPRAS EQ @SY-LANGU
        AND LAND1 EQ @LF_LAND1.

      IF SY-SUBRC EQ 0.
        SELECT LAND1,
               WITHT,
               WT_WITHCD,
               QSATZ,
               QEKAR
          FROM T059Z
          INTO TABLE @DATA(LT_T059Z)
          WHERE T059Z~LAND1 EQ @LF_LAND1.
      ENDIF.

      SORT LT_T059Z BY WITHT WT_WITHCD.
      SORT LT_T059ZT BY WITHT WT_WITHCD.
    ENDIF.

*   Get withholding tax data (WITH_ITEM)
    SELECT BUKRS,
           BELNR,
           GJAHR,
           BUZEI,
           WITHT,
           WT_WITHCD,
           WT_QSSHH,
           WT_QBSHH,
           CTNUMBER,
           QSREC              ##TOO_MANY_ITAB_FIELDS
      FROM WITH_ITEM
      INTO TABLE @CT_WHT_DETAIL
      FOR ALL ENTRIES IN @UT_FIDOC_KEY
      WHERE BELNR EQ @UT_FIDOC_KEY-BELNR
        AND BUKRS EQ @UT_FIDOC_KEY-BUKRS
        AND GJAHR EQ @UT_FIDOC_KEY-GJAHR
        AND WT_WITHCD NE @SPACE.

*   Fill in withholding tax code information from T059Z, T059ZT
    LOOP AT CT_WHT_DETAIL ASSIGNING FIELD-SYMBOL(<L_WHT>).
      CLEAR: LS_SUM_PAY_WHT.

      READ TABLE LT_T059Z INTO DATA(LS_T059Z)
                          WITH KEY WITHT = <L_WHT>-WITHT
                                   WT_WITHCD = <L_WHT>-WT_WITHCD
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_WHT>-QSATZ = LS_T059Z-QSATZ.
        <L_WHT>-QEKAR = LS_T059Z-QEKAR.
      ENDIF.

      READ TABLE LT_T059ZT INTO DATA(LS_T059ZT)
                          WITH KEY WITHT = <L_WHT>-WITHT
                                   WT_WITHCD = <L_WHT>-WT_WITHCD
                          BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_WHT>-TEXT40 = LS_T059ZT-TEXT40.
      ENDIF.

      LS_SUM_PAY_WHT-PAYDOC_BELNR = <L_WHT>-BELNR.
      LS_SUM_PAY_WHT-PAYDOC_GJAHR = <L_WHT>-GJAHR.
      LS_SUM_PAY_WHT-WITHT = <L_WHT>-WITHT.
      LS_SUM_PAY_WHT-WT_WITHCD = <L_WHT>-WT_WITHCD.
      LS_SUM_PAY_WHT-WT_QSSHH = <L_WHT>-WT_QSSHH.
      LS_SUM_PAY_WHT-WT_QBSHH = <L_WHT>-WT_QBSHH.

      COLLECT LS_SUM_PAY_WHT INTO GT_SUM_PAY_WHT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_INVOICE_DETAIL_AUTO
*---------------------------------------------------------------------*
* For option: Automatic payment
* Get invoice document data from BSET, BKPF, BSEG
*---------------------------------------------------------------------*
FORM F_GET_INVOICE_DETAIL_AUTO  USING  UT_INVDOC_KEY TYPE TT_FIDOC_KEY
                              CHANGING CT_INVDOC_DETAIL TYPE TT_INVDOC_DETAIL_AUTO.

  DATA: LT_WHT_DETAIL_INV TYPE TT_WHT_DETAIL.

  CLEAR: CT_INVDOC_DETAIL.

  IF UT_INVDOC_KEY IS NOT INITIAL.
    SELECT BKPF~BUKRS,
           BKPF~BELNR,
           BKPF~GJAHR,
           BSEG~BUZEI,
           BKPF~BLDAT,
           BKPF~XBLNR,
           BKPF~XREVERSED,
           BKPF~XREVERSING,
           BKPF~STBLG,
           BKPF~STJAH,
           BSEG~SGTXT,
           BSEG~REBZG,
           BSEG~REBZJ,
           BSEG~REBZZ
      FROM BKPF
      INNER JOIN BSEG
        ON ( BKPF~BUKRS EQ BSEG~BUKRS AND
             BKPF~BELNR EQ BSEG~BELNR AND
             BKPF~GJAHR EQ BSEG~GJAHR )
      INTO TABLE @CT_INVDOC_DETAIL            ##TOO_MANY_ITAB_FIELDS
      FOR ALL ENTRIES IN @UT_INVDOC_KEY
      WHERE BKPF~BUKRS EQ @UT_INVDOC_KEY-BUKRS
        AND BKPF~BELNR EQ @UT_INVDOC_KEY-BELNR
        AND BKPF~GJAHR EQ @UT_INVDOC_KEY-GJAHR
        AND BSEG~BUZEI EQ @UT_INVDOC_KEY-BUZEI
        AND BSEG~KOART EQ @GC_KOART-VENDOR.

    PERFORM F_GET_WHT_DETAIL USING UT_INVDOC_KEY
                          CHANGING LT_WHT_DETAIL_INV.

  ENDIF.

  LOOP AT CT_INVDOC_DETAIL ASSIGNING FIELD-SYMBOL(<L_INVDOC_DETAIL>).

*   Remove reversed doc and reversing doc
    IF <L_INVDOC_DETAIL>-XREVERSING NE SPACE.
      IF <L_INVDOC_DETAIL>-REBZG IS NOT INITIAL AND
         <L_INVDOC_DETAIL>-REBZJ IS NOT INITIAL AND
         <L_INVDOC_DETAIL>-REBZZ IS NOT INITIAL.
        DELETE CT_INVDOC_DETAIL WHERE BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                  AND BELNR = <L_INVDOC_DETAIL>-REBZG
                                  AND GJAHR = <L_INVDOC_DETAIL>-REBZJ
                                  AND BUZEI = <L_INVDOC_DETAIL>-REBZZ.

      ELSEIF <L_INVDOC_DETAIL>-STBLG IS NOT INITIAL AND
             <L_INVDOC_DETAIL>-STJAH IS NOT INITIAL.
        DELETE CT_INVDOC_DETAIL WHERE BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                       AND BELNR = <L_INVDOC_DETAIL>-STBLG
                                       AND GJAHR =  <L_INVDOC_DETAIL>-STJAH.
      ENDIF.

      DELETE CT_INVDOC_DETAIL WHERE BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                AND BELNR = <L_INVDOC_DETAIL>-BELNR
                                AND GJAHR = <L_INVDOC_DETAIL>-GJAHR
                                AND BUZEI = <L_INVDOC_DETAIL>-BUZEI.

      CONTINUE.
    ENDIF.

*   Withholding tax detail from invoice
    READ TABLE LT_WHT_DETAIL_INV ASSIGNING FIELD-SYMBOL(<L_WHT>)
                                 WITH KEY BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                          BELNR = <L_INVDOC_DETAIL>-BELNR
                                          GJAHR = <L_INVDOC_DETAIL>-GJAHR
                                          BUZEI = <L_INVDOC_DETAIL>-BUZEI.
    IF SY-SUBRC EQ 0.
      <L_INVDOC_DETAIL>-WT_QSSHH = <L_WHT>-WT_QSSHH * -1.
      <L_INVDOC_DETAIL>-WT_QBSHH = <L_WHT>-WT_QBSHH * -1.
    ENDIF.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_COLLECT_RESULT_AUTO
*---------------------------------------------------------------------*
* Option: Automatic payment
* Collect data into internal table GT_RESULT
*---------------------------------------------------------------------*
FORM F_COLLECT_RESULT_AUTO  USING    UT_REGUH TYPE TT_REGUH
                                     UT_REGUP TYPE TT_REGUP
                                     UT_VENDOR_DETAIL TYPE TT_VENDOR_DETAIL
                                     UT_ONETIME_DETAIL TYPE TT_ONETIME_DETAIL
                                     UT_ADDR_DETAIL TYPE TT_ADDR_DETAIL
                                     UT_WHT_DETAIL TYPE TT_WHT_DETAIL
                                     UT_INVDOC_DETAIL TYPE TT_INVDOC_DETAIL_AUTO
                            CHANGING CT_RESULT TYPE TT_RESULT.
  DATA: LS_RESULT      TYPE TS_RESULT,
        LS_SUM_PAY     TYPE TS_SUM_PAY,
        LF_FOUND       TYPE FLAG,
        LF_SKIP_VENDOR TYPE C.

  CLEAR: CT_RESULT, GT_SUM_PAY.

  LOOP AT UT_REGUH ASSIGNING FIELD-SYMBOL(<L_REGUH>).
    CLEAR: LS_RESULT, LS_SUM_PAY.

    LS_RESULT-BUKRS = P_BUKRS.
    MOVE-CORRESPONDING <L_REGUH> TO LS_RESULT.

    LS_RESULT-PAYDOC_BELNR = <L_REGUH>-VBLNR.
*    LS_RESULT-PAYDOC_GJAHR = <L_REGUH>-VALUT(4).   "CH02-

*   BOI - 420000241 - CH02 - 14.01.2025
*   Determine FI Fiscal Year from posting date
    PERFORM F_FI_PERIOD_DETERMINE USING <L_REGUH>-ZALDT
                                  CHANGING LS_RESULT-PAYDOC_GJAHR.
*   EOI - 420000241 - CH02 - 14.01.2025

    LS_RESULT-PAY_AMT = <L_REGUH>-RBETR * -1.
    LS_RESULT-UKONT = <L_REGUH>-UBHKT.
    LS_RESULT-SDS_BANKL = GF_HOUSE_BANK_KEY.
    LS_RESULT-VALUT = P_VENDAT.

    REPLACE ALL OCCURRENCES OF '-' IN LS_RESULT-UBKNT WITH '' .
    CONDENSE LS_RESULT-UBKNT NO-GAPS.

*   Withholding tax detail
    PERFORM F_READ_WHT_DETAIL USING LS_RESULT-BUKRS
                                    LS_RESULT-PAYDOC_BELNR
                                    LS_RESULT-PAYDOC_GJAHR
                                    UT_WHT_DETAIL
                           CHANGING LS_RESULT.

    MOVE-CORRESPONDING LS_RESULT TO LS_SUM_PAY.

    CLEAR: LS_SUM_PAY-INVOICEAMOUNT.
    LS_SUM_PAY-WHTAMOUNT = LS_RESULT-WT_QBSHH.
    LS_SUM_PAY-GROSS_AMOUNT = LS_RESULT-WT_QSSHH.
    LS_SUM_PAY-PAYMENTAMOUNT = LS_RESULT-PAY_AMT.
    COLLECT LS_SUM_PAY INTO GT_SUM_PAY.
* BOI - CH01 - 14.11.2024
    GS_PROC-TOTWHTAMT = GS_PROC-TOTWHTAMT + LS_SUM_PAY-WHTAMOUNT.
    GS_PROC-TOTPAYAMT = GS_PROC-TOTPAYAMT + LS_SUM_PAY-PAYMENTAMOUNT.
* EOI - CH01 - 14.11.2024

    CLEAR: LS_SUM_PAY-PAYMENTAMOUNT, LS_SUM_PAY-GROSS_AMOUNT.

*   Invoice Detail
    LOOP AT UT_REGUP ASSIGNING FIELD-SYMBOL(<L_REGUP>)
                     WHERE LAUFD EQ <L_REGUH>-LAUFD
                       AND LAUFI EQ <L_REGUH>-LAUFI
                       AND XVORL EQ <L_REGUH>-XVORL
                       AND ZBUKR EQ <L_REGUH>-ZBUKR
                       AND LIFNR EQ <L_REGUH>-LIFNR
                       AND KUNNR EQ <L_REGUH>-KUNNR
                       AND EMPFG EQ <L_REGUH>-EMPFG
                       AND VBLNR EQ <L_REGUH>-VBLNR.

      LS_RESULT-BELNR = <L_REGUP>-BELNR.
      LS_RESULT-INVDOC_BELNR = <L_REGUP>-BELNR.
      LS_RESULT-INVDOC_GJAHR = <L_REGUP>-GJAHR.

      IF <L_REGUP>-SHKZG = GC_SHKZG-DEBIT.
        <L_REGUP>-DMBTR = <L_REGUP>-DMBTR * -1.
      ENDIF.

      LS_RESULT-INV_AMT = <L_REGUP>-DMBTR.

      PERFORM F_READ_INVDOC_DETAIL USING <L_REGUP>-BUKRS
                                         <L_REGUP>-BELNR
                                         <L_REGUP>-GJAHR
                                         <L_REGUP>-BUZEI
                                         UT_INVDOC_DETAIL
                                 CHANGING LS_RESULT
                                          LF_FOUND.
      IF LF_FOUND EQ SPACE.
        CONTINUE.
      ENDIF.

*     Vendor Detail
      PERFORM F_READ_VENDOR_DETAIL USING <L_REGUP>-LIFNR
                                         <L_REGUP>-XCPDD
                                         UT_VENDOR_DETAIL
                                         UT_ONETIME_DETAIL
                                         UT_ADDR_DETAIL
                                         SPACE
                                   CHANGING LS_RESULT
                                            LF_SKIP_VENDOR.

      IF LF_SKIP_VENDOR EQ ABAP_TRUE.
        CONTINUE.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '-' IN LS_RESULT-BANKN WITH '' .
      CONDENSE LS_RESULT-BANKN NO-GAPS.

      CLEAR: LS_SUM_PAY-WHTAMOUNT.
      LS_SUM_PAY-INVOICEAMOUNT = LS_RESULT-INV_AMT.

      COLLECT LS_SUM_PAY INTO GT_SUM_PAY.

* BOI - CH01 - 14.11.2024
      GS_PROC-TOTINVAMT = GS_PROC-TOTINVAMT + LS_SUM_PAY-INVOICEAMOUNT.
* EOI - CH01 - 14.11.2024

      PERFORM F_SET_STATUS_MESSAGE CHANGING LS_RESULT.

      INSERT LS_RESULT INTO TABLE CT_RESULT.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT
*----------------------------------------------------------------------*
*  Display Processing Result
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT  USING  UT_RESULT TYPE TT_RESULT.

* BOI - CH01 - 14.11.2024
* Set processing information to display in ALV header
  GS_PROC-DATUM = SY-DATUM.
  GS_PROC-UZEIT = SY-UZEIT.
  GS_PROC-UNAME = SY-UNAME.
  GS_PROC-FILENAME = GF_FILENAME.
* EOI - CH01 - 14.11.2024

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
*  GF_ALV_HEADER_1 = SPACE.     "CH01-
  GF_ALV_HEADER_1 = GC_TRUE.    "CH01+

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_1.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_1.
  ASSIGN UT_RESULT TO <G_LIST_1>.            "#EC CI_FLDEXT_OK[2610650]
* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT CHANGING GT_FIELDCAT_1.
* Sort data
  PERFORM F_ALV_SORT_RESULT CHANGING GT_SORT_1.
* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_DISPLAY_RESULT_UNLOCK
*----------------------------------------------------------------------*
*  Display Processing Result for unlock document
*----------------------------------------------------------------------*
FORM F_DISPLAY_RESULT_UNLOCK  USING  UT_RESULT_LOG TYPE TT_RESULT_LOG.

* Show progress
* Text-p99 : Generating ALV Report . . .
  MC_SHOW_PROGRESS 99 TEXT-P99.

* Set Container name
  GF_CONTAINER_1 = 'CTL_ALV_1'.
* Disable Header area
  GF_ALV_HEADER_1 = SPACE.

* ALV Layout
  PERFORM F_ALV_LAYOUT CHANGING GS_LAYOUT_1
                                GS_VARIANT_1
                                GS_PRINT_1.

* Assign Output Data
* Assign Size
  GF_HEADER_HIGHT_1 = GC_HEADER_HEIGHT_2.
  GF_ALV_HEIGHT_1   = GC_ALV_HEIGHT_2.
  ASSIGN UT_RESULT_LOG TO <G_LIST_1>.        "#EC CI_FLDEXT_OK[2610650]

* Build Field cat
  PERFORM F_ALV_BUILD_FIELDCAT_LOCK CHANGING GT_FIELDCAT_1.

* Call ALV Screen
  CALL SCREEN 9000.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_LAYOUT
*----------------------------------------------------------------------*
*  ALV Layout for Report
*----------------------------------------------------------------------*
FORM F_ALV_LAYOUT CHANGING CS_LAYOUT  TYPE  LVC_S_LAYO
                           CS_VARIANT TYPE  DISVARIANT
                           CS_PRINT   TYPE  LVC_S_PRNT.

* Initialize Output
  CLEAR: CS_LAYOUT, CS_VARIANT, CS_PRINT.

* determine layout
  CS_LAYOUT-SEL_MODE   = 'A'.
  CS_LAYOUT-CWIDTH_OPT = GC_TRUE.
  CS_LAYOUT-ZEBRA      = GC_TRUE.

* For Variant Saving
  CS_VARIANT-REPORT  = SY-REPID.

  CS_PRINT-NO_COLWOPT = GC_TRUE.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

  FIELD-SYMBOLS:
    <L_FIELDCAT>   TYPE  LVC_S_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_1
                              CHANGING CT_FIELDCAT.

  LOOP AT CT_FIELDCAT ASSIGNING <L_FIELDCAT>.

    CASE <L_FIELDCAT>-FIELDNAME.
      WHEN 'BUKRS'.
        <L_FIELDCAT>-KEY = GC_TRUE.
      WHEN 'HBKID'.
        <L_FIELDCAT>-KEY = GC_TRUE.
      WHEN 'VBLNR'.
        <L_FIELDCAT>-KEY = GC_TRUE.
      WHEN 'INV_AMT' OR
           'WT_QBSHH_INV'.
        <L_FIELDCAT>-DO_SUM = GC_TRUE.
      WHEN 'REBZG' OR
           'REBZJ' OR
           'REBZZ' OR
           'REBZT'.
        <L_FIELDCAT>-NO_OUT = GC_TRUE.
      WHEN 'WAERS' OR
           'PAYDOC_BELNR' OR
           'PAYDOC_GJAHR' OR
           'INVDOC_BELNR' OR
           'INVDOC_GJAHR' OR
           'KTOKK' OR
           'PAYEE_ADDR_EN' OR
           'PAYEE_ADDR_IT' OR
           'WT_QSSHH' OR     "WHT Base Paymentdoc
           'WT_QBSHH'.       "WHT Amount Paymentdoc
        <L_FIELDCAT>-TECH = GC_TRUE.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_BUILD_FIELDCAT_LOCK
*----------------------------------------------------------------------*
*  ALV Field Catalog for Report
*----------------------------------------------------------------------*
FORM F_ALV_BUILD_FIELDCAT_LOCK CHANGING CT_FIELDCAT  TYPE LVC_T_FCAT.

* Build Field cat from Structure.
  PERFORM F_PREPARE_FIELDCAT_O  USING  GC_STRUCTURE_2
                              CHANGING CT_FIELDCAT.

ENDFORM.
*----------------------------------------------------------------------*
*  Form F_ALV_SORT_RESULT
*----------------------------------------------------------------------*
*  Maintain SORT data for Result ALV
*----------------------------------------------------------------------*
FORM F_ALV_SORT_RESULT  CHANGING CT_SORT TYPE LVC_T_SORT.

  CONSTANTS:
    LC_SORT1 TYPE  LVC_S_SORT-FIELDNAME VALUE 'BUKRS',
    LC_SORT2 TYPE  LVC_S_SORT-FIELDNAME VALUE 'HBKID',
    LC_SORT3 TYPE  LVC_S_SORT-FIELDNAME VALUE 'VBLNR'.

  DATA:
    LS_SORT  TYPE  LVC_S_SORT.


* Initialize Output
  CLEAR: CT_SORT.

* Sort by BUKRS
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 1.
  LS_SORT-FIELDNAME = LC_SORT1.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by HBKID
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 2.
  LS_SORT-FIELDNAME = LC_SORT2.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = SPACE.
  APPEND LS_SORT TO CT_SORT.

* Sort by VBLNR
  CLEAR LS_SORT.
  LS_SORT-SPOS      = 3.
  LS_SORT-FIELDNAME = LC_SORT3.
  LS_SORT-UP        = GC_TRUE.
  LS_SORT-SUBTOT    = GC_TRUE.
  APPEND LS_SORT TO CT_SORT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_READ_VENDOR_DETAIL
*---------------------------------------------------------------------*
* Return detail of vendor data
*---------------------------------------------------------------------*
FORM F_READ_VENDOR_DETAIL  USING    UF_LIFNR TYPE TS_VENDOR_DETAIL-LIFNR
                                    UF_XCPDD TYPE XCPDD
                                    UT_VENDOR_DETAIL TYPE TT_VENDOR_DETAIL
                                    UT_ONETIME_DETAIL TYPE TT_ONETIME_DETAIL
                                    UT_ADDR_DETAIL TYPE TT_ADDR_DETAIL
                                    UF_BVTYP TYPE LFBK-BVTYP
                           CHANGING CS_RESULT TYPE TS_RESULT
                                    CF_SKIP TYPE C.

  DATA: LF_ADRNR TYPE ADRC-ADDRNUMBER.
  FIELD-SYMBOLS: <L_VENDOR> TYPE TS_VENDOR_DETAIL.

* Initialize output
  CLEAR: CS_RESULT-BANKN,
         CS_RESULT-BANKA,
         CS_RESULT-BANKL,
         CS_RESULT-BRNCH,
         CS_RESULT-EMAIL,
         CS_RESULT-TAXID,
         CS_RESULT-PAYEE_NAME_EN,
         CS_RESULT-PAYEE_NAME_IT,
         CS_RESULT-PAYEE_ADDR_EN,
         CS_RESULT-PAYEE_ADDR_IT,
         CS_RESULT-EMAIL.
  CLEAR: CF_SKIP.

  CHECK UF_LIFNR IS NOT INITIAL.

  CASE UF_BVTYP.
    WHEN SPACE.
      READ TABLE UT_VENDOR_DETAIL ASSIGNING <L_VENDOR>
                                  WITH KEY LIFNR = UF_LIFNR
                                  BINARY SEARCH.
    WHEN OTHERS.
      READ TABLE UT_VENDOR_DETAIL ASSIGNING <L_VENDOR>
                                  WITH KEY LIFNR = UF_LIFNR
                                           BVTYP = UF_BVTYP.
  ENDCASE.

  IF SY-SUBRC EQ 0.
    CASE UF_XCPDD.   "Indicator: Address and Bank Data Set Individually
      WHEN GC_TRUE.
* ------------------------------------
*       One time vendor
* ------------------------------------
        READ TABLE UT_ONETIME_DETAIL ASSIGNING FIELD-SYMBOL(<L_ONETIME>)
                                     WITH KEY BUKRS = CS_RESULT-BUKRS
                                              BELNR = CS_RESULT-PAYDOC_BELNR
                                              GJAHR = CS_RESULT-PAYDOC_GJAHR.
        IF SY-SUBRC EQ 0.
          CS_RESULT-BANKN = <L_ONETIME>-BANKN.
          CS_RESULT-BANKA = <L_ONETIME>-BANKA.
          CS_RESULT-BANKL = <L_ONETIME>-BANKL.
          CS_RESULT-BRNCH = <L_ONETIME>-BRNCH.
          CS_RESULT-EMAIL = <L_ONETIME>-INTAD.

          IF <L_ONETIME>-STCD3 IS NOT INITIAL.
            CS_RESULT-TAXID = <L_ONETIME>-STCD3.
          ELSE.
            CS_RESULT-TAXID = <L_ONETIME>-STCD1.
          ENDIF.

*         Payee name
          CONCATENATE <L_ONETIME>-NAME1
                      <L_ONETIME>-NAME2
                      <L_ONETIME>-NAME3
                      <L_ONETIME>-NAME4
            INTO CS_RESULT-PAYEE_NAME_EN.

          CS_RESULT-PAYEE_NAME_IT = CS_RESULT-PAYEE_NAME_EN.

*         Payee address
          CONCATENATE <L_ONETIME>-STRAS
                      <L_ONETIME>-ORT01
                      <L_ONETIME>-PSTLZ
            INTO CS_RESULT-PAYEE_ADDR_EN.

          CS_RESULT-PAYEE_ADDR_IT = CS_RESULT-PAYEE_ADDR_EN.
        ENDIF.


      WHEN SPACE.
* ------------------------------------
*       Not a one time vendor
* ------------------------------------
        CS_RESULT-BANKN = <L_VENDOR>-BANKN.
        CS_RESULT-BANKA = <L_VENDOR>-BANKA.
        CS_RESULT-BANKL = <L_VENDOR>-BANKL.
        CS_RESULT-BRNCH = <L_VENDOR>-BRNCH.
        CS_RESULT-KTOKK = <L_VENDOR>-KTOKK.
        LF_ADRNR = <L_VENDOR>-ADRNR.

        IF <L_VENDOR>-STCD3 IS NOT INITIAL.
          CS_RESULT-TAXID = <L_VENDOR>-STCD3.
        ELSE.
          CS_RESULT-TAXID = <L_VENDOR>-STCD1.
        ENDIF.
    ENDCASE.
  ELSE.
*   Not found vendor, or vendor is not in scope
    CF_SKIP = ABAP_TRUE.
  ENDIF.

* ------------------------------------
* Read address data
* ------------------------------------
  IF LF_ADRNR IS NOT INITIAL.
*   Address EN version
    READ TABLE UT_ADDR_DETAIL ASSIGNING FIELD-SYMBOL(<L_ADDR_EN>)
                              WITH KEY ADDRNUMBER = LF_ADRNR
                                       NATION = SPACE.
    IF SY-SUBRC EQ 0.
*     Payee name EN
      IF <L_ADDR_EN>-PERS_ADDR EQ GC_TRUE.
        CONCATENATE <L_VENDOR>-TITLE
                    <L_VENDOR>-NAME_FIRST
                    <L_VENDOR>-NAME_LAST
        INTO CS_RESULT-PAYEE_NAME_EN
        SEPARATED BY SPACE.
      ELSE.
        CONCATENATE <L_ADDR_EN>-NAME1
                    <L_ADDR_EN>-NAME2
                    <L_ADDR_EN>-NAME3
                    <L_ADDR_EN>-NAME4
          INTO CS_RESULT-PAYEE_NAME_EN.
      ENDIF.

*     Payee address EN
      CONCATENATE <L_ADDR_EN>-HOUSE_NUM1
                  <L_ADDR_EN>-STREET
                  <L_ADDR_EN>-STR_SUPPL1
                  <L_ADDR_EN>-STR_SUPPL2
                  <L_ADDR_EN>-STR_SUPPL3
                  <L_ADDR_EN>-CITY2
                  <L_ADDR_EN>-CITY1
                  <L_ADDR_EN>-POST_CODE1
        INTO CS_RESULT-PAYEE_ADDR_EN.

      IF <L_ADDR_EN>-TEL_NUMBER IS NOT INITIAL.
        CS_RESULT-TEL_NUMBER = <L_ADDR_EN>-TEL_NUMBER.
      ENDIF.

      IF <L_ADDR_EN>-SMTP_ADDR IS NOT INITIAL.
        CS_RESULT-EMAIL = <L_ADDR_EN>-SMTP_ADDR.
      ENDIF.

    ENDIF.

*   Address Int. version
    READ TABLE UT_ADDR_DETAIL ASSIGNING FIELD-SYMBOL(<L_ADDR_IT>)
                              WITH KEY ADDRNUMBER = LF_ADRNR
                                       NATION = GC_NATION_I.
    IF SY-SUBRC EQ 0.
*     Payee name Int. version
      CONCATENATE <L_ADDR_EN>-NAME1
                  <L_ADDR_EN>-NAME2
                  <L_ADDR_EN>-NAME3
                  <L_ADDR_EN>-NAME4
        INTO CS_RESULT-PAYEE_NAME_IT.

*     Payee address Int. version
      CONCATENATE <L_ADDR_EN>-HOUSE_NUM1
                  <L_ADDR_EN>-STREET
                  <L_ADDR_EN>-STR_SUPPL1
                  <L_ADDR_EN>-STR_SUPPL2
                  <L_ADDR_EN>-STR_SUPPL3
                  <L_ADDR_EN>-CITY2
                  <L_ADDR_EN>-CITY1
                  <L_ADDR_EN>-POST_CODE1
        INTO CS_RESULT-PAYEE_ADDR_IT.

      IF <L_ADDR_IT>-TEL_NUMBER IS NOT INITIAL.
        CS_RESULT-TEL_NUMBER = <L_ADDR_IT>-TEL_NUMBER.
      ENDIF.

      IF <L_ADDR_IT>-SMTP_ADDR IS NOT INITIAL.
        CS_RESULT-EMAIL = <L_ADDR_IT>-SMTP_ADDR.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_READ_INVDOC_DETAIL
*---------------------------------------------------------------------*
* Read invoice document detail
*---------------------------------------------------------------------*
FORM F_READ_INVDOC_DETAIL  USING UF_BUKRS TYPE BKPF-BUKRS
                                 UF_BELNR TYPE BKPF-BELNR
                                 UF_GJAHR TYPE BKPF-GJAHR
                                 UF_BUZEI TYPE BSEG-BUZEI
                                 UT_INVDOC_DETAIL TYPE TT_INVDOC_DETAIL_AUTO
                        CHANGING CS_RESULT TYPE TS_RESULT
                                 CF_FOUND TYPE FLAG.

* Initialize output
  CLEAR: CS_RESULT-XBLNR,
         CS_RESULT-BLDAT,
         CS_RESULT-SGTXT,
         CF_FOUND.

  READ TABLE UT_INVDOC_DETAIL ASSIGNING FIELD-SYMBOL(<L_INVDOC>)
                              WITH KEY BUKRS = UF_BUKRS
                                       BELNR = UF_BELNR
                                       GJAHR = UF_GJAHR
                                       BUZEI = UF_BUZEI
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CS_RESULT-BLDAT = <L_INVDOC>-BLDAT.
    CS_RESULT-XBLNR = <L_INVDOC>-XBLNR.
    CS_RESULT-SGTXT = <L_INVDOC>-SGTXT.
    CS_RESULT-WT_QBSHH_INV = <L_INVDOC>-WT_QBSHH.

    CF_FOUND = GC_TRUE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SET_STATUS_MESSAGE
*---------------------------------------------------------------------*
* Set status and message of each line
*---------------------------------------------------------------------*
FORM F_SET_STATUS_MESSAGE  CHANGING CS_RESULT TYPE TS_RESULT.

  IF CS_RESULT-STATUS IS NOT INITIAL AND
     CS_RESULT-MESSAGE IS NOT INITIAL.
    RETURN.
  ENDIF.

  IF P_VENDAT <> CS_RESULT-ZALDT.
    CS_RESULT-STATUS = ICON_LED_YELLOW.
*   Please check Date transfer to vendor is not equal to Posting Date
    CS_RESULT-MESSAGE = TEXT-001.
  ENDIF.

  IF CS_RESULT-MESSAGE IS INITIAL.
    IF CS_RESULT-WT_WITHCD(1) = 'E'.
      IF CS_RESULT-PAYEE_ADDR_EN IS INITIAL OR
         CS_RESULT-EMAIL IS INITIAL.
        CS_RESULT-STATUS = ICON_LED_RED.
*       Required Fields Missing: Payee Address or Email
        CS_RESULT-MESSAGE = TEXT-004.
      ENDIF.
    ENDIF.
  ENDIF.

  IF CS_RESULT-MESSAGE IS INITIAL.
    IF CS_RESULT-PAY_AMT LE 0.
      CS_RESULT-STATUS = ICON_LED_RED.
*     Invalid value: Payment Amount
      CS_RESULT-MESSAGE = TEXT-005.
    ENDIF.
  ENDIF.

  IF CS_RESULT-STATUS IS INITIAL.
    IF CB_TEST EQ GC_TRUE.
      CS_RESULT-STATUS = ICON_LED_YELLOW.
    ELSE.
      CS_RESULT-STATUS = ICON_LED_GREEN.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_READ_WHT_DETAIL
*---------------------------------------------------------------------*
* Read withholding tax data in payment document
* A payment document will contain only 1 WHT code which not blank
*---------------------------------------------------------------------*
FORM F_READ_WHT_DETAIL  USING    UF_BUKRS TYPE BKPF-BUKRS
                                 UF_BELNR TYPE BKPF-BELNR
                                 UF_GJAHR TYPE BKPF-GJAHR
                                 UT_WHT_DETAIL TYPE TT_WHT_DETAIL
                        CHANGING CS_RESULT TYPE TS_RESULT.

  DATA: LF_SUM_QSSHH TYPE WITH_ITEM-WT_QSSHH,
        LF_SUM_QBSHH TYPE WITH_ITEM-WT_QBSHH,
        LF_NONEWHT   TYPE FLAG,
        LF_EWHT      TYPE FLAG.

* Initialize output
  CLEAR: CS_RESULT-QSREC,
         CS_RESULT-WT_WITHCD,
         CS_RESULT-WT_TEXT,
         CS_RESULT-WT_QSSHH,
         CS_RESULT-WT_QBSHH,
         CS_RESULT-CTNUMBER.

  READ TABLE UT_WHT_DETAIL ASSIGNING FIELD-SYMBOL(<L_WHT>)
                           WITH KEY BUKRS = UF_BUKRS
                                    BELNR = UF_BELNR
                                    GJAHR = UF_GJAHR
                           BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CS_RESULT-QSREC = <L_WHT>-QSREC.
    CS_RESULT-WT_WITHCD = <L_WHT>-WT_WITHCD.
    CS_RESULT-WT_TEXT = <L_WHT>-TEXT40.
    CS_RESULT-WT_QSSHH = <L_WHT>-WT_QSSHH.
    CS_RESULT-WT_QBSHH = <L_WHT>-WT_QBSHH.
    CS_RESULT-CTNUMBER = <L_WHT>-CTNUMBER.
  ENDIF.

  LOOP AT UT_WHT_DETAIL ASSIGNING <L_WHT>
                        WHERE BUKRS = UF_BUKRS
                          AND BELNR = UF_BELNR
                          AND GJAHR = UF_GJAHR.
    IF <L_WHT>-WT_WITHCD(1) EQ 'E'.
      LF_EWHT = 'X'.
    ELSE.
      LF_NONEWHT = 'X'.
    ENDIF.

    LF_SUM_QSSHH = LF_SUM_QSSHH + <L_WHT>-WT_QSSHH.
    LF_SUM_QBSHH = LF_SUM_QBSHH + <L_WHT>-WT_QBSHH.
  ENDLOOP.

  CS_RESULT-WT_QSSHH = LF_SUM_QSSHH.
  CS_RESULT-WT_QBSHH = LF_SUM_QBSHH.

  IF LF_EWHT IS NOT INITIAL AND
     LF_NONEWHT IS NOT INITIAL.
*   Payment document mix EWHT and Non EWHT
    CS_RESULT-STATUS = ICON_LED_RED.
    CS_RESULT-MESSAGE = TEXT-E05.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_DATA_MANUAL
*---------------------------------------------------------------------*
* Get data for option Manual payment
*---------------------------------------------------------------------*
FORM F_GET_DATA_MANUAL CHANGING CT_RESULT TYPE TT_RESULT.
  DATA: LT_LIFNR                   TYPE TT_LIFNR,
        LT_ADRNR                   TYPE TT_ADRNR,
        LT_PAYDOC_KEY              TYPE TT_FIDOC_KEY,
        LT_PAYMENT_DOCUMENT        TYPE TT_PAYMENT_DOCUMENT,
        LT_PAYMENT_DOCUMENT_BANKGL TYPE TT_PAYMENT_DOCUMENT_BANKGL,
        LT_VENDOR_DETAIL           TYPE TT_VENDOR_DETAIL,
        LT_ONETIME_DETAIL          TYPE TT_ONETIME_DETAIL,
        LT_ADDR_DETAIL             TYPE TT_ADDR_DETAIL,
        LT_INVDOC_DETAIL           TYPE TT_INVDOC_DETAIL_MANUAL,
        LT_WHT_DETAIL              TYPE TT_WHT_DETAIL,
        LT_HOUSE_BANK_ACC          TYPE TT_HOUSE_BANK_ACC.

  CLEAR: CT_RESULT.

  PERFORM F_GET_PAYMENT_DOCUMENT CHANGING LT_PAYMENT_DOCUMENT
                                          LT_PAYMENT_DOCUMENT_BANKGL
                                          LT_LIFNR
                                          LT_PAYDOC_KEY.

  PERFORM F_GET_VENDOR_DETAIL USING LT_LIFNR
                           CHANGING LT_VENDOR_DETAIL
                                    LT_ADRNR.

  PERFORM F_GET_ONETIME_VENDOR USING LT_PAYDOC_KEY
                               CHANGING LT_ONETIME_DETAIL
                                        LT_ADRNR.

  PERFORM F_GET_ADDR_DETAIL USING LT_ADRNR
                            CHANGING LT_ADDR_DETAIL.

  PERFORM F_GET_WHT_DETAIL USING LT_PAYDOC_KEY
                           CHANGING LT_WHT_DETAIL.

  PERFORM F_GET_HOUSE_BANK_ACCOUNT CHANGING LT_HOUSE_BANK_ACC.

  PERFORM F_GET_INVOICE_DETAIL_MANUAL USING LT_PAYDOC_KEY
                                   CHANGING LT_INVDOC_DETAIL.

  PERFORM F_COLLECT_RESULT_MANUAL USING LT_PAYMENT_DOCUMENT
                                        LT_PAYMENT_DOCUMENT_BANKGL
                                        LT_VENDOR_DETAIL
                                        LT_ONETIME_DETAIL
                                        LT_ADDR_DETAIL
                                        LT_WHT_DETAIL
                                        LT_INVDOC_DETAIL
                                        LT_HOUSE_BANK_ACC
                               CHANGING CT_RESULT.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_PAYMENT_DOCUMENT
*---------------------------------------------------------------------*
* Get payment document data from BKPF, BSEG
* If paymment has vendor line item more than one, choose first record
*---------------------------------------------------------------------*
FORM F_GET_PAYMENT_DOCUMENT CHANGING CT_PAYMENT_DOCUMENT TYPE TT_PAYMENT_DOCUMENT
                                     CT_PAYMENT_DOCUMENT_BANKGL TYPE TT_PAYMENT_DOCUMENT_BANKGL
                                     CT_LIFNR TYPE TT_LIFNR
                                     CT_PAYDOC_KEY TYPE TT_FIDOC_KEY.

  TYPES: BEGIN OF LS_INVDOC_KEY_PARTIAL,
           REBZG TYPE BSEG-REBZG,
           REBZJ TYPE BSEG-REBZJ,
           REBZZ TYPE BSEG-REBZZ,
           REBZT TYPE BSEG-REBZT,
           ZLSCH TYPE BSEG-ZLSCH,
         END OF LS_INVDOC_KEY_PARTIAL.
  DATA: LT_INVDOC_KEY_PARTIAL TYPE STANDARD TABLE OF LS_INVDOC_KEY_PARTIAL.

  DATA: LS_LIFNR TYPE TS_LIFNR.

  CLEAR: CT_PAYMENT_DOCUMENT,
         CT_PAYMENT_DOCUMENT_BANKGL,
         CT_LIFNR,
         CT_PAYDOC_KEY.

* Payment document header
  SELECT BKPF~BUKRS,
         BKPF~BELNR,
         BKPF~GJAHR,
         BKPF~BLART,
         BKPF~BUDAT,
         BKPF~WAERS,
         BSEG~BUZEI,
         BSEG~ZLSCH,
         BSEG~LIFNR,
         BSEG~EMPFB,
         BSEG~HBKID,
         BSEG~HKTID,
         BSEG~BVTYP,
         BSEG~HKONT,
         BSEG~SHKZG,
         BSEG~DMBTR,
         BSEG~XCPDD,
         BSEG~VALUT,
         BSEG~REBZG,
         BSEG~REBZJ,
         BSEG~REBZZ,
         BSEG~REBZT,
         BSEG~NEBTR
    FROM BKPF
    INNER JOIN BSEG
    ON ( BKPF~BUKRS EQ BSEG~BUKRS AND
         BKPF~BELNR EQ BSEG~BELNR AND
         BKPF~GJAHR EQ BSEG~GJAHR )
    INTO TABLE @CT_PAYMENT_DOCUMENT
    WHERE BKPF~BUKRS EQ @P_BUKRS
      AND BKPF~BELNR IN @S_BELNR
      AND BKPF~GJAHR EQ @P_GJAHR
      AND BKPF~XREVERSAL EQ @SPACE
      AND BSEG~KOART EQ @GC_KOART-VENDOR
      AND ( BSEG~ZLSCH EQ @P_ZLSCH OR
            BSEG~ZLSCH EQ @SPACE ).

  IF SY-SUBRC EQ 0.

    LT_INVDOC_KEY_PARTIAL[] = CORRESPONDING #( CT_PAYMENT_DOCUMENT ).
    SORT LT_INVDOC_KEY_PARTIAL BY REBZG REBZJ REBZZ.
    DELETE ADJACENT DUPLICATES FROM LT_INVDOC_KEY_PARTIAL
      COMPARING REBZG REBZJ REBZZ.

    SORT CT_PAYMENT_DOCUMENT BY BUKRS BELNR GJAHR.

*   If paymment has vendor line item more than one, choose first record
    DELETE ADJACENT DUPLICATES FROM CT_PAYMENT_DOCUMENT
      COMPARING BUKRS BELNR GJAHR.
  ENDIF.

* Keep document key
  CT_PAYDOC_KEY = CORRESPONDING #( CT_PAYMENT_DOCUMENT ).

* Get payment method from invoice document case partial
  IF LT_INVDOC_KEY_PARTIAL IS NOT INITIAL.
    SELECT BUKRS,
           BELNR,
           GJAHR,
           BUZEI,
           ZLSCH
      FROM BSEG
      INTO TABLE @DATA(LT_BSEG_INVDOC_PARTIAL)
      FOR ALL ENTRIES IN @LT_INVDOC_KEY_PARTIAL
      WHERE BUKRS EQ @P_BUKRS
        AND BELNR EQ @LT_INVDOC_KEY_PARTIAL-REBZG
        AND GJAHR EQ @LT_INVDOC_KEY_PARTIAL-REBZJ
        AND BUZEI EQ @LT_INVDOC_KEY_PARTIAL-REBZZ
        AND KOART EQ @GC_KOART-VENDOR
      ORDER BY PRIMARY KEY.
  ENDIF.

* Keep vendor number from BSEG
* If BSEG-EMPFB, vendor number = BSEG-EMPFB
* Otherwise, vendor number = BSEG-LIFNR
  LOOP AT CT_PAYMENT_DOCUMENT ASSIGNING FIELD-SYMBOL(<L_PAYDOC>).
    CLEAR: LS_LIFNR.
    IF <L_PAYDOC>-EMPFB IS NOT INITIAL.
      LS_LIFNR-LIFNR = <L_PAYDOC>-EMPFB.
    ELSE.
      LS_LIFNR-LIFNR = <L_PAYDOC>-LIFNR.
    ENDIF.

*   Read payment method from invoice document for case partial
    IF <L_PAYDOC>-ZLSCH IS INITIAL AND
       <L_PAYDOC>-REBZT EQ 'Z'.          "Partial
      READ TABLE LT_BSEG_INVDOC_PARTIAL INTO DATA(LS_BSEG_INVDOC_PARTIAL)
                                        WITH KEY BUKRS = P_BUKRS
                                                 BELNR = <L_PAYDOC>-REBZG
                                                 GJAHR = <L_PAYDOC>-REBZJ
                                                 BUZEI = <L_PAYDOC>-REBZZ
                                        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <L_PAYDOC>-ZLSCH = LS_BSEG_INVDOC_PARTIAL-ZLSCH.
      ENDIF.
    ENDIF.

    COLLECT LS_LIFNR INTO CT_LIFNR.
  ENDLOOP.

* Get payment document (GL Account line where HKONT in GENC BANK_GL_ACCOUNT)
  IF GRT_BANK_GL_ACCOUNT IS NOT INITIAL.
    SELECT BUKRS,
           BELNR,
           GJAHR,
           BUZEI,
           HKONT,
           SHKZG,
           DMBTR
      FROM BSEG
      INTO TABLE @CT_PAYMENT_DOCUMENT_BANKGL
      WHERE BUKRS EQ @P_BUKRS
        AND BELNR IN @S_BELNR
        AND GJAHR EQ @P_GJAHR
        AND HKONT IN @GRT_BANK_GL_ACCOUNT
      ORDER BY PRIMARY KEY.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_HOUSE_BANK_ACCOUNT
*---------------------------------------------------------------------*
* Get house bank account and account determinaton for payment
*---------------------------------------------------------------------*
FORM F_GET_HOUSE_BANK_ACCOUNT  CHANGING CT_HOUSE_BANK_ACC TYPE TT_HOUSE_BANK_ACC.

  DATA: LS_HOUSE_BANK_ACC TYPE TS_HOUSE_BANK_ACC.

  CLEAR: CT_HOUSE_BANK_ACC.

* Get House Bank Accounts
  SELECT BUKRS,
         HBKID,
         HKTID,
         BANKN
    FROM T012K
    INTO TABLE @DATA(LT_T012K)
    WHERE BUKRS EQ @P_BUKRS
      AND HBKID EQ @P_HBKID
    ORDER BY PRIMARY KEY.

* Get Account determination for payment program
  SELECT *
    FROM T042I
    INTO TABLE @DATA(LT_T042I)
    WHERE ZBUKR EQ @P_BUKRS
      AND HBKID EQ @P_HBKID
      AND ZLSCH EQ @P_ZLSCH
    ORDER BY PRIMARY KEY.

  LOOP AT LT_T042I ASSIGNING FIELD-SYMBOL(<L_T042I>).
    CLEAR: LS_HOUSE_BANK_ACC.
    MOVE-CORRESPONDING <L_T042I> TO LS_HOUSE_BANK_ACC.

    READ TABLE LT_T012K INTO DATA(LS_T012K)
                        WITH KEY BUKRS = <L_T042I>-ZBUKR
                                 HBKID = <L_T042I>-HBKID
                                 HKTID = <L_T042I>-HKTID
                        BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      LS_HOUSE_BANK_ACC-BANKN = LS_T012K-BANKN.
    ENDIF.

    INSERT LS_HOUSE_BANK_ACC INTO TABLE CT_HOUSE_BANK_ACC.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_INVOICE_DETAIL_MANUAL
*---------------------------------------------------------------------*
* For option: Manual payment
* Get invoice data from BSE_CLR, BKPF, BSEG
*---------------------------------------------------------------------*
FORM F_GET_INVOICE_DETAIL_MANUAL  USING    UT_PAYDOC_KEY TYPE TT_FIDOC_KEY
                                  CHANGING CT_INVDOC_DETAIL TYPE TT_INVDOC_DETAIL_MANUAL.
  DATA: LT_INVDOC_KEY     TYPE TT_FIDOC_KEY,
        LT_WHT_DETAIL_INV TYPE TT_WHT_DETAIL.

  CLEAR: CT_INVDOC_DETAIL.

  IF UT_PAYDOC_KEY IS NOT INITIAL.
    SELECT BSE_CLR~BUKRS_CLR,
           BSE_CLR~BELNR_CLR,
           BSE_CLR~GJAHR_CLR,
           BSE_CLR~INDEX_CLR,
           BSE_CLR~WAERS,
           BSE_CLR~CLRIN,
           BSE_CLR~BUKRS,
           BSE_CLR~BELNR,
           BSE_CLR~GJAHR,
           BSE_CLR~BUZEI,
           BSE_CLR~SHKZG,
           BSE_CLR~DMBTR,
           BSE_CLR~DIFHW,
           BKPF~BLDAT,
           BKPF~XBLNR,
           BKPF~XREVERSED,
           BKPF~XREVERSING,
           BKPF~STBLG,
           BKPF~STJAH,
           BSEG~REBZG,
           BSEG~REBZJ,
           BSEG~REBZZ,
           BSEG~SGTXT,
           BSEG~XCPDD
      FROM BSE_CLR
      INNER JOIN BKPF
      ON ( BSE_CLR~BUKRS_CLR EQ BKPF~BUKRS AND
           BSE_CLR~BELNR_CLR EQ BKPF~BELNR AND
           BSE_CLR~GJAHR_CLR EQ BKPF~GJAHR )
      INNER JOIN BSEG
      ON ( BSE_CLR~BUKRS_CLR EQ BSEG~BUKRS AND
           BSE_CLR~BELNR_CLR EQ BSEG~BELNR AND
           BSE_CLR~GJAHR_CLR EQ BSEG~GJAHR AND
           BSEG~KOART EQ @GC_KOART-VENDOR )
      INTO TABLE @CT_INVDOC_DETAIL           ##TOO_MANY_ITAB_FIELDS
      FOR ALL ENTRIES IN @UT_PAYDOC_KEY
      WHERE BUKRS_CLR EQ @UT_PAYDOC_KEY-BUKRS
        AND BELNR_CLR EQ @UT_PAYDOC_KEY-BELNR
        AND GJAHR_CLR EQ @UT_PAYDOC_KEY-GJAHR
        AND ( BSE_CLR~KOART EQ @GC_KOART-VENDOR OR
              BSE_CLR~KOART EQ @GC_KOART-CUSTOMER )
        AND BSE_CLR~UMSKZ EQ @SPACE.
  ENDIF.

  IF CT_INVDOC_DETAIL IS NOT INITIAL.
*   Keep document key
    LT_INVDOC_KEY = CORRESPONDING #( CT_INVDOC_DETAIL MAPPING BUKRS = BUKRS
                                                      BELNR = BELNR
                                                      GJAHR = GJAHR
                                                      BUZEI = BUZEI ).
    DELETE ADJACENT DUPLICATES FROM LT_INVDOC_KEY
       COMPARING BUKRS BELNR GJAHR BUZEI.

    PERFORM F_GET_WHT_DETAIL USING LT_INVDOC_KEY
                      CHANGING LT_WHT_DETAIL_INV.

    LOOP AT CT_INVDOC_DETAIL ASSIGNING FIELD-SYMBOL(<L_INVDOC_DETAIL>).

*     Remove reversed doc and reversing doc
      IF <L_INVDOC_DETAIL>-XREVERSING NE SPACE.

        IF <L_INVDOC_DETAIL>-REBZG IS NOT INITIAL AND
           <L_INVDOC_DETAIL>-REBZJ IS NOT INITIAL AND
           <L_INVDOC_DETAIL>-REBZZ IS NOT INITIAL.
          DELETE CT_INVDOC_DETAIL USING KEY INVDOC
                                  WHERE BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                    AND BELNR = <L_INVDOC_DETAIL>-REBZG
                                    AND GJAHR = <L_INVDOC_DETAIL>-REBZJ
                                    AND BUZEI = <L_INVDOC_DETAIL>-REBZZ.

        ELSEIF <L_INVDOC_DETAIL>-STBLG IS NOT INITIAL AND
               <L_INVDOC_DETAIL>-STJAH IS NOT INITIAL.
          DELETE CT_INVDOC_DETAIL USING KEY INVDOC
                                  WHERE BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                    AND BELNR = <L_INVDOC_DETAIL>-STBLG
                                    AND GJAHR =  <L_INVDOC_DETAIL>-STJAH.
        ENDIF.

        DELETE CT_INVDOC_DETAIL USING KEY INVDOC
                                WHERE BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                  AND BELNR = <L_INVDOC_DETAIL>-BELNR
                                  AND GJAHR = <L_INVDOC_DETAIL>-GJAHR
                                  AND BUZEI = <L_INVDOC_DETAIL>-BUZEI.

        CONTINUE.
      ENDIF.

*     Withholding tax detail from invoice
      READ TABLE LT_WHT_DETAIL_INV ASSIGNING FIELD-SYMBOL(<L_WHT>)
                                   WITH KEY BUKRS = <L_INVDOC_DETAIL>-BUKRS
                                            BELNR = <L_INVDOC_DETAIL>-BELNR
                                            GJAHR = <L_INVDOC_DETAIL>-GJAHR
                                            BUZEI = <L_INVDOC_DETAIL>-BUZEI.
      IF SY-SUBRC EQ 0.
        <L_INVDOC_DETAIL>-WT_QSSHH = <L_WHT>-WT_QSSHH * -1.
        <L_INVDOC_DETAIL>-WT_QBSHH = <L_WHT>-WT_QBSHH * -1.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_COLLECT_RESULT_MANUAL
*---------------------------------------------------------------------*
* Option: Manual payment
* Collect data into internal table GT_RESULT
*---------------------------------------------------------------------*
FORM F_COLLECT_RESULT_MANUAL  USING    UT_PAYMENT_DOCUMENT TYPE TT_PAYMENT_DOCUMENT
                                       UT_PAYMENT_DOCUMENT_BANKGL TYPE TT_PAYMENT_DOCUMENT_BANKGL
                                       UT_VENDOR_DETAIL TYPE TT_VENDOR_DETAIL
                                       UT_ONETIME_DETAIL TYPE TT_ONETIME_DETAIL
                                       UT_ADDR_DETAIL TYPE TT_ADDR_DETAIL
                                       UT_WHT_DETAIL TYPE TT_WHT_DETAIL
                                       UT_INVDOC_DETAIL TYPE TT_INVDOC_DETAIL_MANUAL
                                       UT_HOUSE_BANK_ACC TYPE TT_HOUSE_BANK_ACC
                              CHANGING CT_RESULT TYPE TT_RESULT.

  DATA: LS_RESULT      TYPE TS_RESULT,
        LS_SUM_PAY     TYPE TS_SUM_PAY,
        LF_BSEG_HKONT  TYPE BSEG-HKONT,
        LF_KTOKK       TYPE LFA1-KTOKK,
        LF_SKIP_VENDOR TYPE C.

  CLEAR: CT_RESULT, GT_SUM_PAY.

  LOOP AT UT_PAYMENT_DOCUMENT ASSIGNING FIELD-SYMBOL(<L_PAYMENT_DOC>).
    CLEAR: LS_RESULT, LS_SUM_PAY.

    LS_RESULT-BUKRS = P_BUKRS.

    PERFORM F_SET_STATUS_MESSAGE CHANGING LS_RESULT.

*   House bank account
    PERFORM F_READ_HOUSE_BANK_ACC USING LS_RESULT-BUKRS
                                        <L_PAYMENT_DOC>-HBKID
                                        <L_PAYMENT_DOC>-ZLSCH
                                        <L_PAYMENT_DOC>-WAERS
                                        UT_HOUSE_BANK_ACC
                                  CHANGING LS_RESULT.

*   Payment GL Account and Payment Amount
    PERFORM F_READ_PAYMENT_BANKGL USING LS_RESULT-BUKRS
                                        <L_PAYMENT_DOC>-BELNR
                                        <L_PAYMENT_DOC>-GJAHR
                                        UT_PAYMENT_DOCUMENT_BANKGL
                                        UT_HOUSE_BANK_ACC
                                  CHANGING LF_BSEG_HKONT
                                           LS_RESULT-PAY_AMT
                                           LS_RESULT-HBKID
                                           LS_RESULT-HKTID
                                           LS_RESULT-UBKNT
                                           LS_RESULT-UKONT.

    IF <L_PAYMENT_DOC>-REBZT = 'Z'.    "Partial
      LS_RESULT-PAY_AMT = <L_PAYMENT_DOC>-NEBTR.
      LS_RESULT-REBZG = <L_PAYMENT_DOC>-REBZG.
      LS_RESULT-REBZJ = <L_PAYMENT_DOC>-REBZJ.
      LS_RESULT-REBZZ = <L_PAYMENT_DOC>-REBZZ.
      LS_RESULT-REBZT = <L_PAYMENT_DOC>-REBZT.
    ENDIF.

*   Validate GL account in Payment document(BSEG-HKONT) with T042I-UKONT
    PERFORM F_VALIDATE_BANK_GL_ACCOUNT USING LF_BSEG_HKONT
                                             LS_RESULT-UKONT
                                             <L_PAYMENT_DOC>-ZLSCH
                                       CHANGING LS_RESULT-STATUS
                                                LS_RESULT-MESSAGE.

    LS_RESULT-SDS_BANKL = GF_HOUSE_BANK_KEY.
    LS_RESULT-VBLNR = <L_PAYMENT_DOC>-BELNR.
    LS_RESULT-ZALDT = <L_PAYMENT_DOC>-BUDAT.
    LS_RESULT-PAYDOC_BELNR = <L_PAYMENT_DOC>-BELNR.
    LS_RESULT-PAYDOC_GJAHR = <L_PAYMENT_DOC>-GJAHR.
    LS_RESULT-WAERS = <L_PAYMENT_DOC>-WAERS.
    LS_RESULT-VALUT = P_VENDAT.

    REPLACE ALL OCCURRENCES OF '-' IN LS_RESULT-UBKNT WITH '' .
    CONDENSE LS_RESULT-UBKNT NO-GAPS.

*   Get vendor group
    PERFORM F_READ_VENDOR_GROUP USING <L_PAYMENT_DOC>-LIFNR
                                      UT_VENDOR_DETAIL
                                CHANGING LF_KTOKK.

*   Fill vendor number depends on vendor group
    IF LF_KTOKK IN GRT_VENGRP_EMP.
*     Employee vendor group: Vendor code = BSEG-LIFNR
      LS_RESULT-LIFNR = <L_PAYMENT_DOC>-LIFNR.
    ELSE.
*     Normal vendor group
*     If BSEG-EMPFB <> blank , vendor number = BSEG-EMPFB
*     Otherwise, vendor number = BSEG-LIFNR
      IF <L_PAYMENT_DOC>-EMPFB IS NOT INITIAL.
        LS_RESULT-LIFNR = <L_PAYMENT_DOC>-EMPFB.
      ELSE.
        LS_RESULT-LIFNR = <L_PAYMENT_DOC>-LIFNR.
      ENDIF.
    ENDIF.

*   Withholding tax detail
    PERFORM F_READ_WHT_DETAIL USING LS_RESULT-BUKRS
                                    LS_RESULT-PAYDOC_BELNR
                                    LS_RESULT-PAYDOC_GJAHR
                                    UT_WHT_DETAIL
                           CHANGING LS_RESULT.

    MOVE-CORRESPONDING LS_RESULT TO LS_SUM_PAY.

    CLEAR: LS_SUM_PAY-INVOICEAMOUNT.
    LS_SUM_PAY-WHTAMOUNT = LS_RESULT-WT_QBSHH.
    LS_SUM_PAY-GROSS_AMOUNT = LS_RESULT-WT_QSSHH.
    LS_SUM_PAY-PAYMENTAMOUNT = LS_RESULT-PAY_AMT.
    COLLECT LS_SUM_PAY INTO GT_SUM_PAY.

* BOI - CH01 - 14.11.2024
    GS_PROC-TOTWHTAMT = GS_PROC-TOTWHTAMT + LS_SUM_PAY-WHTAMOUNT.
    GS_PROC-TOTPAYAMT = GS_PROC-TOTPAYAMT + LS_SUM_PAY-PAYMENTAMOUNT.
* EOI - CH01 - 14.11.2024

    CLEAR: LS_SUM_PAY-PAYMENTAMOUNT, LS_SUM_PAY-GROSS_AMOUNT.

*   Invoice detail
    LOOP AT UT_INVDOC_DETAIL ASSIGNING FIELD-SYMBOL(<L_INVDOC_DETAIL>)
                             WHERE BUKRS_CLR EQ <L_PAYMENT_DOC>-BUKRS
                               AND BELNR_CLR EQ <L_PAYMENT_DOC>-BELNR
                               AND GJAHR_CLR EQ <L_PAYMENT_DOC>-GJAHR.

      LS_RESULT-BELNR = <L_INVDOC_DETAIL>-BELNR.
      LS_RESULT-INVDOC_BELNR = <L_INVDOC_DETAIL>-BELNR.
      LS_RESULT-INVDOC_GJAHR = <L_INVDOC_DETAIL>-GJAHR.
      LS_RESULT-BLDAT = <L_INVDOC_DETAIL>-BLDAT.
      LS_RESULT-XBLNR = <L_INVDOC_DETAIL>-XBLNR.
      LS_RESULT-SGTXT = <L_INVDOC_DETAIL>-SGTXT.

      LS_RESULT-WT_QBSHH_INV = <L_INVDOC_DETAIL>-WT_QBSHH.

      IF <L_PAYMENT_DOC>-REBZT = 'Z'.    "Partial
        LS_RESULT-WT_QBSHH_INV = LS_RESULT-WT_QBSHH.  "WHT Amount from payment
      ENDIF.

      IF <L_INVDOC_DETAIL>-SHKZG = GC_SHKZG-DEBIT.
        <L_INVDOC_DETAIL>-DMBTR = <L_INVDOC_DETAIL>-DMBTR * -1.
      ENDIF.

*     Invoice amount = AMOUNT - BSE_CLR-DIFHW (partial amount)
      LS_RESULT-INV_AMT = <L_INVDOC_DETAIL>-DMBTR + <L_INVDOC_DETAIL>-DIFHW.

      PERFORM F_READ_VENDOR_DETAIL USING LS_RESULT-LIFNR
                                         <L_INVDOC_DETAIL>-XCPDD
                                         UT_VENDOR_DETAIL
                                         UT_ONETIME_DETAIL
                                         UT_ADDR_DETAIL
                                         <L_PAYMENT_DOC>-BVTYP
                                   CHANGING LS_RESULT
                                            LF_SKIP_VENDOR.
      IF LF_SKIP_VENDOR EQ ABAP_TRUE.
        CONTINUE.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '-' IN LS_RESULT-BANKN WITH '' .
      CONDENSE LS_RESULT-BANKN NO-GAPS.

      CLEAR: LS_SUM_PAY-WHTAMOUNT.
      LS_SUM_PAY-INVOICEAMOUNT = LS_RESULT-INV_AMT.

      COLLECT LS_SUM_PAY INTO GT_SUM_PAY.

* BOI - CH01 - 14.11.2024
      GS_PROC-TOTINVAMT = GS_PROC-TOTINVAMT + LS_SUM_PAY-INVOICEAMOUNT.
* EOI - CH01 - 14.11.2024

      PERFORM F_SET_STATUS_MESSAGE CHANGING LS_RESULT.

      INSERT LS_RESULT INTO TABLE CT_RESULT.

    ENDLOOP.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_READ_VENDOR_GROUP
*---------------------------------------------------------------------*
* To read vendor account group
*---------------------------------------------------------------------*
FORM F_READ_VENDOR_GROUP  USING    UF_LIFNR TYPE LFA1-LIFNR
                                   UT_VENDOR_DETAIL TYPE TT_VENDOR_DETAIL
                          CHANGING CF_KTOKK TYPE LFA1-KTOKK.

  CLEAR: CF_KTOKK.

  READ TABLE UT_VENDOR_DETAIL INTO DATA(LS_VENDOR_DETAIL)
                              WITH KEY LIFNR = UF_LIFNR
                              BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CF_KTOKK = LS_VENDOR_DETAIL-KTOKK.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_READ_HOUSE_BANK_ACC
*---------------------------------------------------------------------*
* Read house bank account detail from T042I, T012K
*---------------------------------------------------------------------*
FORM F_READ_HOUSE_BANK_ACC  USING    UF_BUKRS TYPE BSEG-BUKRS
                                     UF_HBKID TYPE BSEG-HBKID
                                     UF_ZLSCH TYPE BSEG-ZLSCH
                                     UF_WAERS TYPE BKPF-WAERS
                                     UT_HOUSE_BANK_ACC TYPE TT_HOUSE_BANK_ACC
                            CHANGING CS_RESULT TYPE TS_RESULT.

* Initialize output
  CLEAR: CS_RESULT-HBKID,
         CS_RESULT-HKTID,
         CS_RESULT-UBKNT,
         CS_RESULT-UKONT.

  READ TABLE UT_HOUSE_BANK_ACC INTO DATA(LS_HOUSE_BANK_ACC)
                               WITH KEY ZBUKR = UF_BUKRS
                                        HBKID = UF_HBKID
                                        ZLSCH = UF_ZLSCH
                                        WAERS = UF_WAERS
                               BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    CS_RESULT-HBKID = LS_HOUSE_BANK_ACC-HBKID.
    CS_RESULT-HKTID = LS_HOUSE_BANK_ACC-HKTID.
    CS_RESULT-UBKNT = LS_HOUSE_BANK_ACC-BANKN.
    CS_RESULT-UKONT = LS_HOUSE_BANK_ACC-UKONT.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_READ_PAYMENT_BANKGL
*---------------------------------------------------------------------*
* Read payment document Bank GL Account item
*---------------------------------------------------------------------*
FORM F_READ_PAYMENT_BANKGL  USING    UF_BUKRS TYPE BSEG-BUKRS
                                     UF_BELNR TYPE BSEG-BELNR
                                     UF_GJAHR TYPE BSEG-GJAHR
                                     UT_PAYMENT_DOCUMENT_BANKGL TYPE TT_PAYMENT_DOCUMENT_BANKGL
                                     UT_HOUSE_BANK_ACC TYPE TT_HOUSE_BANK_ACC
                            CHANGING CF_BSEG_HKONT TYPE BSEG-HKONT
                                     CF_PAY_AMT TYPE TS_RESULT-PAY_AMT
                                     CF_HBKID TYPE TS_RESULT-HBKID
                                     CF_HKTID TYPE TS_RESULT-HKTID
                                     CF_UBKNT TYPE TS_RESULT-UBKNT
                                     CF_UKONT TYPE TS_RESULT-UKONT.

  CLEAR: CF_BSEG_HKONT,
         CF_PAY_AMT.

  READ TABLE UT_PAYMENT_DOCUMENT_BANKGL ASSIGNING FIELD-SYMBOL(<L_BANKGL>)
                                        WITH KEY BUKRS = UF_BUKRS
                                                 BELNR = UF_BELNR
                                                 GJAHR = UF_GJAHR.
  IF SY-SUBRC EQ 0.
    CF_BSEG_HKONT = <L_BANKGL>-HKONT.
    CF_PAY_AMT = <L_BANKGL>-DMBTR.
  ENDIF.


* Read House bank account from GL
  IF CF_BSEG_HKONT IS NOT INITIAL AND
     CF_UKONT IS INITIAL.
    READ TABLE UT_HOUSE_BANK_ACC INTO DATA(LS_HOUSE_BANK_ACC)
                                 WITH KEY UKONT
                                 COMPONENTS UKONT = CF_BSEG_HKONT.
    IF SY-SUBRC EQ 0.
      CF_HBKID = LS_HOUSE_BANK_ACC-HBKID.
      CF_HKTID = LS_HOUSE_BANK_ACC-HKTID.
      CF_UBKNT = LS_HOUSE_BANK_ACC-BANKN.
      CF_UKONT = LS_HOUSE_BANK_ACC-UKONT.
    ENDIF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_VALIDATE_BANK_GL_ACCOUNT
*---------------------------------------------------------------------*
* Validate bank gl account in payment document (BSEG-HKONT)
* with gl account in T042I-UKONT
* If not match, then show error
*---------------------------------------------------------------------*
FORM F_VALIDATE_BANK_GL_ACCOUNT  USING    UF_HKONT TYPE BSEG-HKONT
                                          UF_UKONT TYPE T042I-UKONT
                                          UF_ZLSCH TYPE BSEG-ZLSCH
                                 CHANGING CF_STATUS TYPE TS_RESULT-STATUS
                                          CF_MESSAGE TYPE TS_RESULT-MESSAGE.

* Bank GL account in payment document not match the GL in T042I
  IF UF_HKONT <> UF_UKONT.
    CF_STATUS = ICON_RED_LIGHT.

*   GL bank account of ... is not match with payment method ...
    CONCATENATE TEXT-002
                UF_HKONT
                TEXT-003
                UF_ZLSCH
      INTO CF_MESSAGE SEPARATED BY SPACE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GENERATE_FILE
*---------------------------------------------------------------------*
* Prepare output file for SCB bank
*---------------------------------------------------------------------*
FORM F_SCB_GENERATE_FILE  USING UT_RESULT TYPE TT_RESULT
                          CHANGING CF_FILENAME TYPE STRING.

  DATA: LT_FILE   TYPE TT_FILE,
        LT_RESULT TYPE TT_RESULT.

  CLEAR: LT_FILE, CF_FILENAME.

* Generate file include only records which not error
  LT_RESULT[] = UT_RESULT[].
  DELETE LT_RESULT WHERE STATUS EQ ICON_RED_LIGHT.

  IF LT_RESULT IS INITIAL.
*   Cannot find input data for file creation.
    MESSAGE S008(ZSDSCA01).
    RETURN.
  ENDIF.

  CASE ABAP_TRUE.
    WHEN RB_DIREC.
*     SCB Direct format
      PERFORM F_SCB_GENERATE_DIRECT USING LT_RESULT
                                 CHANGING LT_FILE.
    WHEN RB_SMART.
*     SCB Smart format
      PERFORM F_SCB_GENERATE_SMART USING LT_RESULT
                                CHANGING LT_FILE.
  ENDCASE.

* Generate file
  IF LT_FILE IS NOT INITIAL.
    CASE ABAP_TRUE.
      WHEN RB_LOCAL.
        PERFORM F_DOWNLOAD_FILE_LOCAL_SCB USING LT_FILE
                                       CHANGING CF_FILENAME.
      WHEN RB_H2H.
*       Do nothing, No H2H functionality for SCB
    ENDCASE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_HEADER_SMART
*---------------------------------------------------------------------*
* Populate header record to SCB file (Record type 001)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_HEADER_SMART  USING UT_RESULT TYPE TT_RESULT
                          CHANGING CT_FILE TYPE TT_FILE.

  DATA: LS_FILE       TYPE TS_FILE,
        LS_SCB_HEADER TYPE TS_SCB_HEADER.

  READ TABLE UT_RESULT INTO DATA(LS_RESULT) INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LS_SCB_HEADER-RECORDTYPE = GC_SCB_RECTYPE-HEADER.
  LS_SCB_HEADER-COMPANYID = GS_GENC_SCB-SCB_COMPANY_ID.

  IF LS_RESULT-KTOKK IN GRT_VENGRP_EMP.
    LS_SCB_HEADER-DESCRIPTION = TEXT-T02.   "Employee
  ELSE.
    LS_SCB_HEADER-DESCRIPTION = TEXT-T01.   "Market Place
  ENDIF.

  LS_SCB_HEADER-FILEDATE = SY-DATUM.
  LS_SCB_HEADER-FILETIME = '000000'.   "Fix to 000000
  LS_SCB_HEADER-CHANNELID = GS_GENC_SCB-SCB_CHANNEL_ID.

  CONCATENATE LS_SCB_HEADER-RECORDTYPE
              LS_SCB_HEADER-COMPANYID
              LS_SCB_HEADER-DESCRIPTION
              LS_SCB_HEADER-FILEDATE
              LS_SCB_HEADER-FILETIME
              LS_SCB_HEADER-CHANNELID
   INTO LS_FILE RESPECTING BLANKS.

  CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
    INTO LS_FILE RESPECTING BLANKS.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_DEBIT_SMART
*---------------------------------------------------------------------*
* Populate debit record to SCB file (Record type 002)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_DEBIT_SMART  USING UT_RESULT TYPE TT_RESULT
                         CHANGING CT_FILE TYPE TT_FILE
                                  CF_COUNT_DEBIT TYPE NUM06
                                  CF_COUNT_CREDIT TYPE NUM06
                                  CF_TOTAL_AMOUNT TYPE ZSDSDE_PAY_AMT.

  DATA: LS_FILE                 TYPE TS_FILE,
        LS_SCB_DEBIT            TYPE TS_SCB_DEBIT,
        LS_SUM_BY_VALUE_DATE    TYPE TS_SUM_BY_VALUE_DATE,
        LS_SUM_CREDIT_BY_VENDOR TYPE TS_SUM_CREDIT_BY_VENDOR,
        LS_PREVIOUS             TYPE TS_RESULT,
        LF_INTERNAL_REF(8)      TYPE N,
        LF_VAT_REGIS_PAYER      TYPE T001-STCEG.

  DATA: LT_SUM_BY_VALUE_DATE TYPE TT_SUM_BY_VALUE_DATE,
        LT_SUM_BY_VENDOR     TYPE TT_SUM_CREDIT_BY_VENDOR,
        LT_RESULT_CREDIT     TYPE TT_RESULT.

  CLEAR: CF_COUNT_DEBIT,
         CF_COUNT_CREDIT,
         CF_TOTAL_AMOUNT,
         LF_VAT_REGIS_PAYER,
         LT_SUM_BY_VALUE_DATE,
         LT_SUM_BY_VENDOR,
         LT_RESULT_CREDIT.

* VAT registration no. of the payer
  SELECT SINGLE STCEG
    FROM T001
    INTO LF_VAT_REGIS_PAYER
    WHERE BUKRS EQ P_BUKRS.
  IF SY-SUBRC NE 0.
    CLEAR: LF_VAT_REGIS_PAYER.
  ELSE.
    LF_VAT_REGIS_PAYER = LF_VAT_REGIS_PAYER+2(13).
  ENDIF.

* Prepare sum of number credit and payment amount
* 1 Output file will have only 1 record type 001 (Debit Detail)
  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>).
    CLEAR: LS_SUM_BY_VALUE_DATE, LS_SUM_CREDIT_BY_VENDOR.

    IF <L_RESULT>-PAYDOC_BELNR EQ LS_PREVIOUS-PAYDOC_BELNR AND
       <L_RESULT>-PAYDOC_GJAHR EQ LS_PREVIOUS-PAYDOC_GJAHR.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <L_RESULT> TO LS_SUM_CREDIT_BY_VENDOR.

    COLLECT LS_SUM_CREDIT_BY_VENDOR INTO LT_SUM_BY_VENDOR.

    LS_PREVIOUS = <L_RESULT>.

    LS_SUM_BY_VALUE_DATE-UBKNT = <L_RESULT>-UBKNT.
    LS_SUM_BY_VALUE_DATE-VALUT = <L_RESULT>-VALUT.
    LS_SUM_BY_VALUE_DATE-PAY_AMT = <L_RESULT>-PAY_AMT.

    COLLECT LS_SUM_BY_VALUE_DATE INTO LT_SUM_BY_VALUE_DATE.
  ENDLOOP.

  LT_RESULT_CREDIT[] = CORRESPONDING #( LT_SUM_BY_VENDOR[] ).

* Group Data by Company bank (UBKNT) and Value date (VALUT)
  LOOP AT LT_SUM_BY_VALUE_DATE ASSIGNING FIELD-SYMBOL(<L_SUM>).
*   Record type 002 (Debit)
    CLEAR: LS_SCB_DEBIT, LF_INTERNAL_REF.

*   Count total of debit record and sum total payment amount
    CF_COUNT_DEBIT = CF_COUNT_DEBIT + 1.
    CF_TOTAL_AMOUNT = CF_TOTAL_AMOUNT + <L_SUM>-PAY_AMT.

*   Internal reference = Running number for credit/debit record grouping
    LF_INTERNAL_REF = LF_INTERNAL_REF + 1.

    LS_SCB_DEBIT-RECORDTYPE = GC_SCB_RECTYPE-DEBIT.
    LS_SCB_DEBIT-PRODUCTCODE = GS_GENC_SCB-SCB_PRODUCT_CODE.
    LS_SCB_DEBIT-VALUEDATE = <L_SUM>-VALUT.
    LS_SCB_DEBIT-TAXID = LF_VAT_REGIS_PAYER. "SPACE.
    LS_SCB_DEBIT-DEBITACCOUNT = <L_SUM>-UBKNT.
    CONCATENATE '0' <L_SUM>-UBKNT+3(1) INTO LS_SCB_DEBIT-ACCOUNTTYPE.
    CONCATENATE '0' <L_SUM>-UBKNT+0(3) INTO LS_SCB_DEBIT-DEBITBRANCH.
    LS_SCB_DEBIT-DEBITCURR = GC_THB.

    PERFORM F_WRITE_NUMBER USING <L_SUM>-PAY_AMT
                           CHANGING LS_SCB_DEBIT-DEBITAMOUNT.

    LS_SCB_DEBIT-INTERNALREF = LF_INTERNAL_REF.

    LS_SCB_DEBIT-NUMCREDIT = LINES( LT_RESULT_CREDIT ).

    LS_SCB_DEBIT-FEEDEBITACC = <L_SUM>-UBKNT.

    CONCATENATE '0' <L_SUM>-UBKNT+3(1) INTO LS_SCB_DEBIT-ACCOUNTTYPEFEE.
    CONCATENATE '0' <L_SUM>-UBKNT+0(3) INTO LS_SCB_DEBIT-DEBITBRANCHFEE.

    LS_SCB_DEBIT-TRANSACTIONREF = ' '.

    CONCATENATE LS_SCB_DEBIT-RECORDTYPE
                LS_SCB_DEBIT-PRODUCTCODE
                LS_SCB_DEBIT-VALUEDATE
                LS_SCB_DEBIT-TAXID
                LS_SCB_DEBIT-DEBITACCOUNT
                LS_SCB_DEBIT-ACCOUNTTYPE
                LS_SCB_DEBIT-DEBITBRANCH
                LS_SCB_DEBIT-DEBITCURR
                LS_SCB_DEBIT-DEBITAMOUNT
                LS_SCB_DEBIT-INTERNALREF
                LS_SCB_DEBIT-NUMCREDIT
                LS_SCB_DEBIT-FEEDEBITACC
                LS_SCB_DEBIT-ACCOUNTTYPEFEE
                LS_SCB_DEBIT-DEBITBRANCHFEE
                LS_SCB_DEBIT-TRANSACTIONREF
       INTO LS_FILE RESPECTING BLANKS.

    CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
      INTO LS_FILE RESPECTING BLANKS.

    APPEND LS_FILE TO CT_FILE.

*   Record type 003 (Credit)
    PERFORM F_SCB_GEN_CREDIT_SMART USING <L_SUM>-UBKNT
                                         <L_SUM>-VALUT
                                         LT_RESULT_CREDIT
                                         LS_SCB_DEBIT-INTERNALREF
                                CHANGING CT_FILE
                                         CF_COUNT_CREDIT.

  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_CREDIT_SMART
*---------------------------------------------------------------------*
* Populate credit record to SCB file (Record type 003)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_CREDIT_SMART USING UF_UBKNT TYPE TS_RESULT-UBKNT
                                  UF_VALUT TYPE TS_RESULT-VALUT
                                  UT_RESULT TYPE TT_RESULT
                                  UF_INTERNAL_REF TYPE CHAR8
                         CHANGING CT_FILE TYPE TT_FILE
                                  CF_COUNT_CREDIT TYPE NUM06.

  DATA: LS_FILE       TYPE TS_FILE,
        LS_SCB_CREDIT TYPE TS_SCB_CREDIT.
  DATA: LF_CREDITSEQNO(6) TYPE N.

  IF UF_UBKNT IS INITIAL OR
     UF_VALUT IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR: LF_CREDITSEQNO.
* Loop invoice document under the same value date
  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE UBKNT EQ UF_UBKNT
                      AND VALUT EQ UF_VALUT.

    CLEAR: LS_SCB_CREDIT, LS_FILE.

    CF_COUNT_CREDIT = CF_COUNT_CREDIT + 1.
    LF_CREDITSEQNO = LF_CREDITSEQNO + 1.

    LS_SCB_CREDIT-RECORDTYPE = GC_SCB_RECTYPE-CREDIT.
    LS_SCB_CREDIT-CREDITSEQNO = LF_CREDITSEQNO.
    LS_SCB_CREDIT-CREDITACCOUNT = <L_RESULT>-BANKN.
    LS_SCB_CREDIT-BENEFICIARY = <L_RESULT>-PAYEE_NAME_EN. "SPACE.

    PERFORM F_WRITE_NUMBER USING <L_RESULT>-PAY_AMT
                           CHANGING LS_SCB_CREDIT-CREDITAMOUNT.

    LS_SCB_CREDIT-CREDITCURR = GC_THB.
    LS_SCB_CREDIT-RECVBANKNAME = <L_RESULT>-BANKA.

*   Receving bank code = 0+First 3 digit of vendor bank key
    CONCATENATE '0' <L_RESULT>-BANKL(3)
    INTO LS_SCB_CREDIT-RECVBANKCODE.

    LS_SCB_CREDIT-INTERNALREF = UF_INTERNAL_REF.

    LS_SCB_CREDIT-FILLER = ' '.

    CONCATENATE LS_SCB_CREDIT-RECORDTYPE
                LS_SCB_CREDIT-CREDITSEQNO
                LS_SCB_CREDIT-CREDITACCOUNT
                LS_SCB_CREDIT-BENEFICIARY
                LS_SCB_CREDIT-CREDITAMOUNT
                LS_SCB_CREDIT-CREDITCURR
                LS_SCB_CREDIT-RECVBANKNAME
                LS_SCB_CREDIT-RECVBANKCODE
                LS_SCB_CREDIT-TAXID
                LS_SCB_CREDIT-MOBILE
                LS_SCB_CREDIT-INTERNALREF
                LS_SCB_CREDIT-REMARK
                LS_SCB_CREDIT-PROXYTYPE
                LS_SCB_CREDIT-FILLER
    INTO LS_FILE RESPECTING BLANKS.

    CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
      INTO LS_FILE RESPECTING BLANKS.

    APPEND LS_FILE TO CT_FILE.

  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_TRAILER
*---------------------------------------------------------------------*
* Populate trailer record to SCB file (Record type 999)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_TRAILER  USING  UF_COUNT_DEBIT TYPE NUM06
                               UF_COUNT_CREDIT TYPE NUM06
                               UF_TOTAL_AMOUNT TYPE ZSDSDE_PAY_AMT
                      CHANGING CT_FILE TYPE TT_FILE.

  DATA: LS_FILE        TYPE TS_FILE,
        LS_SCB_TRAILER TYPE TS_SCB_TRAILER.

  LS_SCB_TRAILER-RECORDTYPE = GC_SCB_RECTYPE-TRAILER.
  LS_SCB_TRAILER-COUNTDEBIT = UF_COUNT_DEBIT.
  LS_SCB_TRAILER-COUNTCREDIT = UF_COUNT_CREDIT.

  PERFORM F_WRITE_NUMBER USING UF_TOTAL_AMOUNT
                         CHANGING LS_SCB_TRAILER-TOTALAMOUNT.

  CONCATENATE LS_SCB_TRAILER-RECORDTYPE
              LS_SCB_TRAILER-COUNTDEBIT
              LS_SCB_TRAILER-COUNTCREDIT
              LS_SCB_TRAILER-TOTALAMOUNT
    INTO LS_FILE RESPECTING BLANKS.

  CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
    INTO LS_FILE RESPECTING BLANKS.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GENERATE_FILE
*---------------------------------------------------------------------*
* Prepare output file for SMBC bank
*---------------------------------------------------------------------*
FORM F_SMB_GENERATE_FILE  USING UT_RESULT TYPE TT_RESULT
                          CHANGING CF_FILENAME TYPE STRING.

  DATA: LT_FILE       TYPE TT_FILE,
        LS_SMB_HEADER TYPE TS_SMB_HEADER,
        LT_SMB_PAY    TYPE TT_SMB_PAY,
        LT_SMB_INV    TYPE TT_SMB_INV,
        LT_RESULT     TYPE TT_RESULT.

  CLEAR: LT_FILE, CF_FILENAME.

* Generate file include only records which not error
  LT_RESULT[] = UT_RESULT[].
  DELETE LT_RESULT WHERE STATUS EQ ICON_RED_LIGHT.

  IF LT_RESULT IS INITIAL.
*   Cannot find input data for file creation.
    MESSAGE S008(ZSDSCA01).
    RETURN.
  ENDIF.

* Record type ENV: Header record
  PERFORM F_SMB_GEN_HEADER USING LT_RESULT
                        CHANGING LS_SMB_HEADER.

* Record type INS: INS record (Payment)
* Record type INV: INV record (Invoice)
  PERFORM F_SMB_GEN_PAY USING LT_RESULT
                     CHANGING LT_SMB_PAY
                              LT_SMB_INV.

* Do not split records by limit for option Net AP AR
  IF CB_NETAR EQ SPACE.    "CH01+
* Split records according to Min-Max Amount of Payee bank
    PERFORM F_SMB_SPLIT_RECORD CHANGING LT_SMB_PAY
                                        LT_SMB_INV.
  ENDIF.                  "CH01+

  PERFORM F_SMB_GEN_FILE USING LS_SMB_HEADER
                               LT_SMB_PAY
                               LT_SMB_INV
                      CHANGING LT_FILE.

* Generate file
  IF LT_FILE IS NOT INITIAL.
    CASE ABAP_TRUE.
      WHEN RB_LOCAL.
        PERFORM F_DOWNLOAD_FILE_LOCAL_SMB USING LT_FILE
                                       CHANGING CF_FILENAME.
      WHEN RB_H2H.
        PERFORM F_CREATE_INTERFACE_FILE USING LT_FILE
                                     CHANGING CF_FILENAME.
    ENDCASE.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GEN_HEADER
*---------------------------------------------------------------------*
* Populate header record to SMBC file (Record type ENV)
*---------------------------------------------------------------------*
FORM F_SMB_GEN_HEADER  USING    UT_RESULT TYPE TT_RESULT ##NEEDED
                       CHANGING CS_SMB_HEADER TYPE TS_SMB_HEADER.

  DATA: LS_SMB_HEADER TYPE TS_SMB_HEADER.

  CLEAR: CS_SMB_HEADER.

  READ TABLE UT_RESULT INTO DATA(LS_RESULT) INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LS_SMB_HEADER-ENV = GC_SMB_RECTYPE-ENV.
  LS_SMB_HEADER-DEFAULTID = SPACE.
  LS_SMB_HEADER-SM = GS_GENC_SMB-SMB_ENV_SM.
  LS_SMB_HEADER-TRANSFERTYPE = GS_GENC_SMB-SMB_ENV_TRANSFERTYPE.
  LS_SMB_HEADER-DEBITTYPE = SPACE.

  LS_SMB_HEADER-VALUEDATE = P_VENDAT.
  LS_SMB_HEADER-CCY = GC_THB.
  LS_SMB_HEADER-DEBITBRANCHCD = GS_GENC_SMB-SMB_ENV_DRBRANCH.
  LS_SMB_HEADER-DEBITGLCD = GS_GENC_SMB-SMB_ENV_DRGL.
  LS_SMB_HEADER-DEBITGLSUBCD = GS_GENC_SMB-SMB_ENV_DRGLSUB.
  LS_SMB_HEADER-DEBITSSNO = GS_GENC_SMB-SMB_ENV_DRSSNO.
  LS_SMB_HEADER-DEBITACCOUNTNO = LS_RESULT-UBKNT.
  LS_SMB_HEADER-DEBITSUBNO = GS_GENC_SMB-SMB_ENV_DRSUBNO.
  LS_SMB_HEADER-DEBITCCY = GC_THB.
  LS_SMB_HEADER-DEBITMKTCD = GS_GENC_SMB-SMB_ENV_DRMKT.
  LS_SMB_HEADER-CHARGEBRANCHCD = GS_GENC_SMB-SMB_ENV_CHARGEBRNCH.
  LS_SMB_HEADER-CHARGEGLCD = GS_GENC_SMB-SMB_ENV_CHARGEGL.
  LS_SMB_HEADER-CHARGEGLSUBCD = GS_GENC_SMB-SMB_ENV_CHARGEGLSUB.
  LS_SMB_HEADER-CHARGESSNO = GS_GENC_SMB-SMB_ENV_CHARGESS.
  LS_SMB_HEADER-CHARGEACCOUNTNO = LS_RESULT-UBKNT.
  LS_SMB_HEADER-CHARGESUBNO = GS_GENC_SMB-SMB_ENV_CHARGESUB.
  LS_SMB_HEADER-CHARGECCY = GC_THB.
  LS_SMB_HEADER-CHARGEMKTCD = GS_GENC_SMB-SMB_ENV_CHARGEMKT.
  LS_SMB_HEADER-REMITCCY = SPACE.
  LS_SMB_HEADER-VALUEDATETO = SPACE.
  LS_SMB_HEADER-FREQUENCY = SPACE.
  LS_SMB_HEADER-SUBSTITUTEDAYFLG = SPACE.

  CS_SMB_HEADER = LS_SMB_HEADER.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GEN_PAY
*---------------------------------------------------------------------*
* Populate payment record to SMBC file (Record type INS)
*---------------------------------------------------------------------*
FORM F_SMB_GEN_PAY  USING    UT_RESULT TYPE TT_RESULT
                    CHANGING CT_SMB_PAY TYPE TT_SMB_PAY
                             CT_SMB_INV TYPE TT_SMB_INV.

  DATA: LS_SMB_PAY     TYPE TS_SMB_PAY,
        LF_RUNNING(20) TYPE N,
        LF_SPLIT_INV   TYPE C.

  LF_SPLIT_INV = CB_SPLIT.

  SORT GT_SUM_PAY BY PAYDOC_GJAHR PAYDOC_BELNR.

* Group Data by Payment document
  LOOP AT GT_SUM_PAY ASSIGNING FIELD-SYMBOL(<L_SMB_SUM_PAY>).
    LF_RUNNING = LF_RUNNING + 1.

    READ TABLE UT_RESULT INTO DATA(LS_RESULT)
                         WITH KEY PAYDOC_GJAHR = <L_SMB_SUM_PAY>-PAYDOC_GJAHR
                                  PAYDOC_BELNR = <L_SMB_SUM_PAY>-PAYDOC_BELNR
                         BINARY SEARCH.
    IF SY-SUBRC EQ 0.

      CLEAR: LS_SMB_PAY.
      LS_SMB_PAY-INS = GC_SMB_RECTYPE-INS.

      WRITE LF_RUNNING TO LS_SMB_PAY-DEFAULTID NO-ZERO.
      CONDENSE LS_SMB_PAY-DEFAULTID NO-GAPS.

      LS_SMB_PAY-PAYEENO = LS_RESULT-LIFNR.
      LS_SMB_PAY-PAYEENAME = LS_RESULT-PAYEE_NAME_EN.
      LS_SMB_PAY-ADDESS = LS_RESULT-PAYEE_ADDR_EN.
      LS_SMB_PAY-BANKNAME = LS_RESULT-BANKA.
      LS_SMB_PAY-BANKCODE = LS_RESULT-BANKL.
      LS_SMB_PAY-BRANCHNAME = LS_RESULT-BRNCH.
      LS_SMB_PAY-ACCOUNTNO = LS_RESULT-BANKN.

      IF LS_RESULT-REBZT EQ 'Z'.
        LS_SMB_PAY-PAYMENTAMOUNT = LS_RESULT-PAY_AMT.
      ELSE.
        LS_SMB_PAY-PAYMENTAMOUNT = LS_RESULT-PAY_AMT.
      ENDIF.
      CONDENSE LS_SMB_PAY-PAYMENTAMOUNT NO-GAPS.

      CASE ABAP_TRUE.
        WHEN RB_APPLI.
          LS_SMB_PAY-OURCHARGE = GC_SMB_BANKCHARGE-APPLI.
        WHEN RB_BENEF.
          LS_SMB_PAY-OURCHARGE = GC_SMB_BANKCHARGE-BENEF.
      ENDCASE.

      LS_SMB_PAY-INSTRUCTIONREF = LS_RESULT-PAYDOC_BELNR.
      LS_SMB_PAY-PHONENO = LS_RESULT-TEL_NUMBER.
      LS_SMB_PAY-EMAILADDRESS = LS_RESULT-EMAIL.
      LS_SMB_PAY-EMAILNOTIFICATION = GS_GENC_SMB-SMB_INS_EMAILNOTIF.

      CASE LS_RESULT-QSREC.
        WHEN '53'.
          LS_SMB_PAY-WHTID = LS_RESULT-TAXID.
        WHEN '03'.
          LS_SMB_PAY-CARDID = LS_RESULT-TAXID.
      ENDCASE.

      LS_SMB_PAY-WHTFORMID = LS_RESULT-QSREC.

*     To support both Host2Host and smart web, support only QSREC = 03 or 53
*     If QSREC <> 03 or 53, then set it to blank
      IF LS_SMB_PAY-WHTFORMID NE '03' AND
         LS_SMB_PAY-WHTFORMID NE '53'.
        CLEAR: LS_SMB_PAY-WHTFORMID.
      ENDIF.

      LS_SMB_PAY-FAXNOTIFICATION = GS_GENC_SMB-SMB_INS_FAXNOTIF.
      LS_SMB_PAY-CALCMODEID = GS_GENC_SMB-SMB_INS_CALCMODEID.
      LS_SMB_PAY-TRANSACTIONCD = GS_GENC_SMB-SMB_INS_TRANSACTIONCD.
      LS_SMB_PAY-PAYMENTBYID = GS_GENC_SMB-SMB_INS_PAYMENTBYID.

      LS_SMB_PAY-INVOICEAMOUNT = <L_SMB_SUM_PAY>-INVOICEAMOUNT.
      CONDENSE LS_SMB_PAY-INVOICEAMOUNT NO-GAPS.

      LS_SMB_PAY-WHTAMOUNT = <L_SMB_SUM_PAY>-WHTAMOUNT.
      CONDENSE LS_SMB_PAY-WHTAMOUNT NO-GAPS.

*     If WHT code start with E, set WHT_CERTIFICATE = 05
*     Else set WHT_CERTIFICATE = 02
      IF LS_RESULT-WT_WITHCD(1) = 'E'.
        LS_SMB_PAY-WHT_CERTIFICATE = '05'.
      ELSE.
        LS_SMB_PAY-WHT_CERTIFICATE = '02'.
      ENDIF.

      APPEND LS_SMB_PAY TO CT_SMB_PAY.

*     Split INV record when reach maximum line per 1 INS
      IF LF_SPLIT_INV EQ ABAP_TRUE.
*       Record type INV: INV record (Invoice)
        PERFORM F_SMB_GEN_INV_SPLIT USING LS_SMB_PAY
                                          LS_RESULT-PAYDOC_GJAHR
                                          LS_RESULT-PAYDOC_BELNR
                                          UT_RESULT
                                 CHANGING CT_SMB_PAY
                                          CT_SMB_INV
                                          LF_RUNNING.

        WRITE LF_RUNNING TO LS_SMB_PAY-DEFAULTID NO-ZERO.
        CONDENSE LS_SMB_PAY-DEFAULTID NO-GAPS.

      ELSE.
*       Record type INV: INV record (Invoice)
        PERFORM F_SMB_GEN_INV USING LS_RESULT-PAYDOC_GJAHR
                                    LS_RESULT-PAYDOC_BELNR
                                    LS_SMB_PAY-DEFAULTID
                                    UT_RESULT
                           CHANGING CT_SMB_INV.

      ENDIF.

*     Record type INV: INV record (WHT Line)
      PERFORM F_SMB_GEN_INV_WHT USING LS_RESULT-PAYDOC_GJAHR
                                      LS_RESULT-PAYDOC_BELNR
                                      LS_SMB_PAY-DEFAULTID
                             CHANGING CT_SMB_INV.

    ENDIF.

  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GEN_INV
*---------------------------------------------------------------------*
* Populate invoice record to SMBC file (Record type INV)
*---------------------------------------------------------------------*
FORM F_SMB_GEN_INV  USING    UF_PAYDOC_GJAHR TYPE TS_RESULT-PAYDOC_GJAHR
                             UF_PAYDOC_BELNR TYPE TS_RESULT-PAYDOC_BELNR
                             UF_DEFAULTID TYPE CHAR20
                             UT_RESULT TYPE TT_RESULT
                    CHANGING CT_SMB_INV TYPE TT_SMB_INV.

  DATA: LS_SMB_INV TYPE TS_SMB_INV.

  IF UF_PAYDOC_GJAHR IS INITIAL OR
     UF_PAYDOC_BELNR IS INITIAL.
    RETURN.
  ENDIF.

* Loop invoice document under the same payment document
  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE PAYDOC_GJAHR EQ UF_PAYDOC_GJAHR
                      AND PAYDOC_BELNR EQ UF_PAYDOC_BELNR.

    CLEAR: LS_SMB_INV.

    LS_SMB_INV-INV = GC_SMB_RECTYPE-INV.
    LS_SMB_INV-DEFAULTID = UF_DEFAULTID.
    LS_SMB_INV-REFERENCE = <L_RESULT>-XBLNR.
    LS_SMB_INV-CALCFOR = GS_GENC_SMB-SMB_INV_CALCFOR.
    LS_SMB_INV-INVOICEDATE = <L_RESULT>-BLDAT.

    CONCATENATE <L_RESULT>-INVDOC_BELNR <L_RESULT>-INVDOC_GJAHR
      INTO LS_SMB_INV-DESCRIPTION.               "Invoice Doc Number

    LS_SMB_INV-INVOICEAMOUNT = <L_RESULT>-INV_AMT.
    CONDENSE LS_SMB_INV-INVOICEAMOUNT NO-GAPS.

    LS_SMB_INV-WHTAMOUNT_INV = <L_RESULT>-WT_QBSHH_INV.   "29.10.2024
    CONDENSE LS_SMB_INV-WHTAMOUNT_INV NO-GAPS.

    APPEND LS_SMB_INV TO CT_SMB_INV.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GEN_INV_SPLIT
*---------------------------------------------------------------------*
* Populate invoice record to SMBC file (Record type INV)
* with split logic as SMBC H2H has limitation on maxinum number of
* INV record per group (of same INS) = 300 INV per 1 INS
*---------------------------------------------------------------------*
FORM F_SMB_GEN_INV_SPLIT  USING US_SMB_PAY TYPE TS_SMB_PAY
                                UF_PAYDOC_GJAHR TYPE TS_RESULT-PAYDOC_GJAHR
                                UF_PAYDOC_BELNR TYPE TS_RESULT-PAYDOC_BELNR
                                UT_RESULT TYPE TT_RESULT
                       CHANGING CT_SMB_PAY TYPE TT_SMB_PAY
                                CT_SMB_INV TYPE TT_SMB_INV
                                CF_RUNNING TYPE NUM20.

  DATA: LS_SMB_INV         TYPE TS_SMB_INV,
        LS_SMB_PAY         TYPE TS_SMB_PAY,
        LF_NUM_INV         TYPE I,
        LF_RUNNING(20)     TYPE N,
        LF_MAXINV          TYPE I,
        LF_INV_TABIX       TYPE SY-TABIX,
        LF_INS_FIRST_INDEX TYPE SY-INDEX,
        LF_SPLIT_FLG       TYPE C,
        LF_LAST_INV_FLG    TYPE C,
        LF_INVAMT_ALL      TYPE DMBTR,
        LF_PAYAMT          TYPE DMBTR,
        LF_INVAMT          TYPE DMBTR,
        LF_WHTAMT          TYPE DMBTR.

  DATA: LT_RESULT_INV TYPE TT_RESULT.

  IF UF_PAYDOC_GJAHR IS INITIAL OR
     UF_PAYDOC_BELNR IS INITIAL.
    RETURN.
  ENDIF.

* LF_INS_FIRST_INDEX = First index no in CT_FILE of this group
  LF_INS_FIRST_INDEX = LINES( CT_SMB_PAY ).
  LS_SMB_PAY = US_SMB_PAY.

  LF_RUNNING = CF_RUNNING - 1.
  LF_MAXINV = P_MAXINV.

* Keep only invoice records under this payment document
  LT_RESULT_INV[] = UT_RESULT[].
  DELETE LT_RESULT_INV WHERE PAYDOC_GJAHR NE UF_PAYDOC_GJAHR
                          OR PAYDOC_BELNR NE UF_PAYDOC_BELNR.

*---------------------
* To check and split INV records
*---------------------
  CLEAR: LF_SPLIT_FLG, LF_LAST_INV_FLG.

  DO.
    LF_RUNNING = LF_RUNNING + 1.
    CLEAR: LF_INVAMT, LF_WHTAMT,
           LS_SMB_INV, LF_NUM_INV.

    LS_SMB_PAY = US_SMB_PAY.

    WRITE LF_RUNNING TO LS_SMB_PAY-DEFAULTID NO-ZERO.
    CONDENSE LS_SMB_PAY-DEFAULTID NO-GAPS.

*   Loop invoice document under the same payment document
    LOOP AT LT_RESULT_INV ASSIGNING FIELD-SYMBOL(<L_RESULT>).
      LF_INV_TABIX = SY-TABIX.

      CLEAR: LS_SMB_INV.

      LF_NUM_INV = LF_NUM_INV + 1.

      IF LF_NUM_INV > LF_MAXINV.
        LF_SPLIT_FLG = 'X'.
        EXIT.
      ENDIF.

      LS_SMB_INV-INV = GC_SMB_RECTYPE-INV.

      WRITE LF_RUNNING TO LS_SMB_INV-DEFAULTID NO-ZERO.
      CONDENSE LS_SMB_INV-DEFAULTID NO-GAPS.

      LS_SMB_INV-REFERENCE = <L_RESULT>-XBLNR.
      LS_SMB_INV-CALCFOR = GS_GENC_SMB-SMB_INV_CALCFOR.
      LS_SMB_INV-INVOICEDATE = <L_RESULT>-BLDAT.

      LS_SMB_INV-INVOICEAMOUNT = <L_RESULT>-INV_AMT.

      CONCATENATE <L_RESULT>-INVDOC_BELNR <L_RESULT>-INVDOC_GJAHR
        INTO LS_SMB_INV-DESCRIPTION.               "Invoice Doc Number

      LS_SMB_INV-WHTAMOUNT_INV = <L_RESULT>-WT_QBSHH_INV.  "29.10.2024
      CONDENSE LS_SMB_INV-WHTAMOUNT_INV NO-GAPS.

*     Record type INV: INV record (Invoice)
      APPEND LS_SMB_INV TO CT_SMB_INV.

*     Sum amount for all invoice in this INV group
      LF_INVAMT = LF_INVAMT + <L_RESULT>-INV_AMT.
      LF_INVAMT_ALL = LF_INVAMT_ALL + <L_RESULT>-INV_AMT.
      LF_WHTAMT = LF_WHTAMT + <L_RESULT>-WT_QBSHH_INV.

      DELETE LT_RESULT_INV INDEX LF_INV_TABIX.
      IF LT_RESULT_INV IS INITIAL.
        LF_LAST_INV_FLG = 'X'.
      ENDIF.
    ENDLOOP.
    IF SY-SUBRC NE 0.  "No more INV record
      IF LF_RUNNING NE 0.
        LF_RUNNING = LF_RUNNING - 1.
        LF_LAST_INV_FLG = 'X'.
      ENDIF.
      EXIT.
    ENDIF.

    IF LF_SPLIT_FLG EQ 'X'.
*     Adjust the first INS of the group
*     Delete the existing one first
*     Then append with the updated invoice information
      IF LF_INS_FIRST_INDEX IS NOT INITIAL.
*        DELETE CT_FILE INDEX LF_INS_FIRST_INDEX.
        DELETE CT_SMB_PAY INDEX LF_INS_FIRST_INDEX.
        CLEAR: LF_INS_FIRST_INDEX.
      ENDIF.

      LS_SMB_PAY-INVOICEAMOUNT = LF_INVAMT.
      CLEAR: LS_SMB_PAY-WHTAMOUNT.

      IF LF_LAST_INV_FLG EQ SPACE.
*       Not the last set of the group
*       Payment amount = sum invoice of the same INS
*                        - sum WHT at each invoice
        LF_PAYAMT = LF_INVAMT - LF_WHTAMT.
        LS_SMB_PAY-PAYMENTAMOUNT = LF_PAYAMT.
        CONDENSE LS_SMB_PAY-PAYMENTAMOUNT NO-GAPS.

        LS_SMB_PAY-WHTAMOUNT = LF_WHTAMT.   "29.10.2024
        CONDENSE LS_SMB_PAY-WHTAMOUNT NO-GAPS.
      ELSE.
*       Last set of the group
*       Payment amount = Invoice Amount - WHT Amount
        LF_PAYAMT = LF_INVAMT - LF_WHTAMT.
        LS_SMB_PAY-PAYMENTAMOUNT = LF_PAYAMT.
        CONDENSE LS_SMB_PAY-PAYMENTAMOUNT NO-GAPS.

        LS_SMB_PAY-WHTAMOUNT = LF_WHTAMT.   "29.10.2024
        CONDENSE LS_SMB_PAY-WHTAMOUNT NO-GAPS.
      ENDIF.

      IF US_SMB_PAY-WHT_CERTIFICATE EQ '05' AND "EWHT
         LF_LAST_INV_FLG EQ SPACE.
*       If INS line is EWHT, change all INS of the group to 02
*       excep the last INS of this group, keep it to 05
        LS_SMB_PAY-WHT_CERTIFICATE = '02'.
      ENDIF.

      APPEND LS_SMB_PAY TO CT_SMB_PAY.

    ENDIF.
  ENDDO.

  CF_RUNNING = LF_RUNNING.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GEN_INV_WHT
*---------------------------------------------------------------------*
* Populate WHT record to SMBC file (Record type INV)
*---------------------------------------------------------------------*
FORM F_SMB_GEN_INV_WHT USING UF_PAYDOC_GJAHR TYPE TS_RESULT-PAYDOC_GJAHR
                             UF_PAYDOC_BELNR TYPE TS_RESULT-PAYDOC_BELNR
                             UF_DEFAULTID TYPE CHAR20
                    CHANGING CT_SMB_INV TYPE TT_SMB_INV.

  DATA: LS_SMB_INV TYPE TS_SMB_INV.

  IF UF_PAYDOC_GJAHR IS INITIAL OR
     UF_PAYDOC_BELNR IS INITIAL.
    RETURN.
  ENDIF.

* Loop WHT Summary data for payment document
  LOOP AT GT_SUM_PAY_WHT ASSIGNING FIELD-SYMBOL(<L_SUM>)
                    WHERE PAYDOC_GJAHR EQ UF_PAYDOC_GJAHR
                      AND PAYDOC_BELNR EQ UF_PAYDOC_BELNR.

    CLEAR: LS_SMB_INV.

    LS_SMB_INV-INV = GC_SMB_RECTYPE-INV.
    LS_SMB_INV-DEFAULTID = UF_DEFAULTID.
    LS_SMB_INV-REFERENCE = GC_WHT.
    LS_SMB_INV-CALCFOR = GS_GENC_SMB-SMB_INV_CALCFOR.

    LS_SMB_INV-WHTAMOUNT = <L_SUM>-WT_QBSHH.
    CONDENSE LS_SMB_INV-WHTAMOUNT NO-GAPS.



    IF <L_SUM>-WT_WITHCD(1) = 'E'.
      PERFORM F_GET_EWHT_MAPPING USING <L_SUM>-WITHT
                                       <L_SUM>-WT_WITHCD
                              CHANGING LS_SMB_INV-WHTTYPE
                                       LS_SMB_INV-DESCRIPTION.
    ELSE.
      LS_SMB_INV-WHTTYPE = '99'.
      LS_SMB_INV-DESCRIPTION = 'Non EWHT' ##NO_TEXT.
    ENDIF.

    LS_SMB_INV-GROSS_AMOUNT = <L_SUM>-WT_QSSHH.
    CONDENSE LS_SMB_INV-GROSS_AMOUNT NO-GAPS.

    APPEND LS_SMB_INV TO CT_SMB_INV.

  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_APPEND_HEADER
*---------------------------------------------------------------------*
* Append data to record type ENV
*---------------------------------------------------------------------*
FORM F_SMB_APPEND_HEADER  USING US_SMB_HEADER TYPE TS_SMB_HEADER
                       CHANGING CT_FILE TYPE TT_FILE.
  DATA: LS_FILE       TYPE TS_FILE,
        LS_SMB_HEADER TYPE TS_SMB_HEADER.

  IF US_SMB_HEADER IS INITIAL.
    RETURN.
  ENDIF.

  LS_SMB_HEADER = US_SMB_HEADER .

  CONCATENATE LS_SMB_HEADER-ENV
              LS_SMB_HEADER-DEFAULTID
              LS_SMB_HEADER-SM
              LS_SMB_HEADER-TRANSFERTYPE
              LS_SMB_HEADER-DEBITTYPE
              LS_SMB_HEADER-CUSTREF
              LS_SMB_HEADER-VALUEDATE
              LS_SMB_HEADER-CCY
              LS_SMB_HEADER-DEBITBRANCHCD
              LS_SMB_HEADER-DEBITGLCD
              LS_SMB_HEADER-DEBITGLSUBCD
              LS_SMB_HEADER-DEBITSSNO
              LS_SMB_HEADER-DEBITACCOUNTNO
              LS_SMB_HEADER-DEBITSUBNO
              LS_SMB_HEADER-DEBITCCY
              LS_SMB_HEADER-DEBITMKTCD
              LS_SMB_HEADER-CHARGEBRANCHCD
              LS_SMB_HEADER-CHARGEGLCD
              LS_SMB_HEADER-CHARGEGLSUBCD
              LS_SMB_HEADER-CHARGESSNO
              LS_SMB_HEADER-CHARGEACCOUNTNO
              LS_SMB_HEADER-CHARGESUBNO
              LS_SMB_HEADER-CHARGECCY
              LS_SMB_HEADER-CHARGEMKTCD
*              LS_SMB_HEADER-REMITCCY
*              LS_SMB_HEADER-VALUEDATETO
*              LS_SMB_HEADER-FREQUENCY
*              LS_SMB_HEADER-SUBSTITUTEDAYFLG
   INTO LS_FILE
   SEPARATED BY GC_SEPARATOR.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_APPEND_PAY
*---------------------------------------------------------------------*
* Append data to record type INS (Payment)
*---------------------------------------------------------------------*
FORM F_SMB_APPEND_PAY  USING    US_SMB_PAY TYPE TS_SMB_PAY
                       CHANGING CT_FILE TYPE TT_FILE.
  DATA: LS_FILE    TYPE TS_FILE,
        LS_SMB_PAY TYPE TS_SMB_PAY.

  IF US_SMB_PAY IS INITIAL.
    RETURN.
  ENDIF.

  LS_SMB_PAY = US_SMB_PAY.

  CONDENSE LS_SMB_PAY-PAYMENTAMOUNT NO-GAPS.
  CONDENSE LS_SMB_PAY-INVOICEAMOUNT NO-GAPS.
  CONDENSE LS_SMB_PAY-WHTAMOUNT NO-GAPS.

* Put " for text field
  CONCATENATE GC_QUOTE LS_SMB_PAY-PAYEENAME GC_QUOTE INTO LS_SMB_PAY-PAYEENAME.
  CONCATENATE GC_QUOTE LS_SMB_PAY-ADDESS GC_QUOTE INTO LS_SMB_PAY-ADDESS.
  CONCATENATE GC_QUOTE LS_SMB_PAY-PHONENO GC_QUOTE INTO LS_SMB_PAY-PHONENO.
  CONCATENATE GC_QUOTE LS_SMB_PAY-BANKNAME GC_QUOTE INTO LS_SMB_PAY-BANKNAME.
  CONCATENATE GC_QUOTE LS_SMB_PAY-BRANCHNAME GC_QUOTE INTO LS_SMB_PAY-BRANCHNAME.
  CONCATENATE GC_QUOTE LS_SMB_PAY-INSTRUCTIONREF GC_QUOTE INTO LS_SMB_PAY-INSTRUCTIONREF.
  CONCATENATE GC_QUOTE LS_SMB_PAY-EMAILADDRESS GC_QUOTE INTO LS_SMB_PAY-EMAILADDRESS.

  CONCATENATE LS_SMB_PAY-INS
              LS_SMB_PAY-DEFAULTID
              LS_SMB_PAY-PAYEENO
              LS_SMB_PAY-PAYEENAME
              LS_SMB_PAY-ADDESS
              LS_SMB_PAY-PHONENO
              LS_SMB_PAY-BANKNAME
              LS_SMB_PAY-BANKCODE
              LS_SMB_PAY-BRANCHREGIONCD
              LS_SMB_PAY-BRANCHREGIONNAME  "10
              LS_SMB_PAY-BRANCHNAME
              LS_SMB_PAY-ACCOUNTNO
              LS_SMB_PAY-PAYMENTAMOUNT
              LS_SMB_PAY-MESSAGE
              LS_SMB_PAY-OURCHARGE
              LS_SMB_PAY-INSTRUCTIONREF
              LS_SMB_PAY-EMAILADDRESS
              LS_SMB_PAY-EMAILNOTIFICATION
              LS_SMB_PAY-ZIPCD
              LS_SMB_PAY-FAXNO             "20
              LS_SMB_PAY-ATTENTION
              LS_SMB_PAY-WHTID
              LS_SMB_PAY-CARDID
              LS_SMB_PAY-WHTFORMID
              LS_SMB_PAY-FAXNOTIFICATION
              LS_SMB_PAY-PICKUPID
              LS_SMB_PAY-CALCMODEID
              LS_SMB_PAY-INVOICEAMOUNT
              LS_SMB_PAY-VATRATE
              LS_SMB_PAY-WHTAMOUNT         "30
              LS_SMB_PAY-DISCOUNTAMOUNT
              LS_SMB_PAY-REGISTEREDMAILADDRESS
              LS_SMB_PAY-WHTORDERINGNO
              LS_SMB_PAY-WHTREF
              LS_SMB_PAY-REMARKS
              LS_SMB_PAY-ONBEHALFOFNAME
              LS_SMB_PAY-ONBEHALFOFADDRESS
              LS_SMB_PAY-ONBEHALFOFWHTID
              LS_SMB_PAY-INSTRUCTIONREF2
              LS_SMB_PAY-TRANSACTIONCD      "40
              LS_SMB_PAY-INPUTKANA
              LS_SMB_PAY-BANKADDRESS
              LS_SMB_PAY-MEMO
              LS_SMB_PAY-REMITTYPE
              LS_SMB_PAY-PRIORITY
              LS_SMB_PAY-PAYERNAME
              LS_SMB_PAY-PAYERADDRESS
              LS_SMB_PAY-PAYERTYPE
              LS_SMB_PAY-UNITCODE
              LS_SMB_PAY-INDIVIDUALNO       "50
              LS_SMB_PAY-INDIVIDUALTYPE
              LS_SMB_PAY-COUNTRYNAME
              LS_SMB_PAY-BANKCOUNTRYNAME
              LS_SMB_PAY-INTERMEDBANKNAME
              LS_SMB_PAY-INTERMEDBANKCODE
              LS_SMB_PAY-INTERMEDBANKBRANCHNAME
              LS_SMB_PAY-INTERMEDBANKCOUNTRYNAME
              LS_SMB_PAY-INTERMEDBANKACCOUNTNO
              LS_SMB_PAY-FXAMOUNT
              LS_SMB_PAY-FXDEBITBRANCHCD    "60
              LS_SMB_PAY-FXDEBITGLCD
              LS_SMB_PAY-FXDEBITGLSUBCD
              LS_SMB_PAY-FXDEBITSSNO
              LS_SMB_PAY-FXDEBITACCOUNTNO
              LS_SMB_PAY-FXDEBITSUBNO
              LS_SMB_PAY-FXDEBITCCY
              LS_SMB_PAY-FXDEBITMKTCD
              LS_SMB_PAY-PURCHASEAMOUNT
              LS_SMB_PAY-PURCHASEDEBITBRANCHCD
              LS_SMB_PAY-PURCHASEDEBITGLCD   "70
              LS_SMB_PAY-PURCHASEDEBITGLSUBCD
              LS_SMB_PAY-PURCHASEDEBITSSNO
              LS_SMB_PAY-PURCHASEDEBITACCOUNTNO
              LS_SMB_PAY-PURCHASEDEBITSUBNO
              LS_SMB_PAY-PURCHASEDEBITCCY
              LS_SMB_PAY-PURCHASEDEBITMKTCD
              LS_SMB_PAY-OTHERAMOUNT
              LS_SMB_PAY-OTHERDEBITBRANCHCD
              LS_SMB_PAY-OTHERDEBITGLCD
              LS_SMB_PAY-OTHERDEBITGLSUBCD   "80
              LS_SMB_PAY-OTHERDEBITSSNO
              LS_SMB_PAY-OTHERDEBITACCOUNTNO
              LS_SMB_PAY-OTHERDEBITSUBNO
              LS_SMB_PAY-OTHERDEBITCCY
              LS_SMB_PAY-OTHERDEBITMKTCD
              LS_SMB_PAY-LOCALCOUNTRYNAME
              LS_SMB_PAY-PAYMENTTYPE
              LS_SMB_PAY-PAYMENTPROPERTY
              LS_SMB_PAY-BOPTRANSCODE1
              LS_SMB_PAY-BOPCCY1 "90
              LS_SMB_PAY-BOPAMOUNT1
              LS_SMB_PAY-BOPREMARKS1
              LS_SMB_PAY-VATAMOUNT
              LS_SMB_PAY-CALCTYPE
              LS_SMB_PAY-TAXCALCMETHODID
              LS_SMB_PAY-CHEQUENO
              LS_SMB_PAY-CLEARINGDATE
              LS_SMB_PAY-INVOICENO
              LS_SMB_PAY-DOCUMENTNO
              LS_SMB_PAY-REGIONOPTIONFLG   "100
              LS_SMB_PAY-COMPANYID
              LS_SMB_PAY-SERVICECD
              LS_SMB_PAY-ENVELOPECD
              LS_SMB_PAY-INSTRUCTIONCD
              LS_SMB_PAY-COUNT
              LS_SMB_PAY-STATUS
              LS_SMB_PAY-STATUSREMARKS
              LS_SMB_PAY-BKBRSELECTFLG
              LS_SMB_PAY-NEWPAYEEFLG
              LS_SMB_PAY-WARNING            "110
              LS_SMB_PAY-REGTIME
              LS_SMB_PAY-LASTUPDATETIME
              LS_SMB_PAY-BOPTRANSCODE2
              LS_SMB_PAY-BOPCCY2
              LS_SMB_PAY-BOPAMOUNT2
              LS_SMB_PAY-BOPREMARKS2
              LS_SMB_PAY-LASTSHIPMENTDATE
              LS_SMB_PAY-IMPORTINSPECTION
              LS_SMB_PAY-CONTRACTNO
              LS_SMB_PAY-IMPORTINSPECTIONINVOICENO  "120
              LS_SMB_PAY-SAFEAPPROVALNO
              LS_SMB_PAY-CUSTOMSUNITCODE
              LS_SMB_PAY-APPLICANTNAME
              LS_SMB_PAY-APPLICANTTELNO
              LS_SMB_PAY-MESSAGETOBANK
              LS_SMB_PAY-COUNTRYCD
              LS_SMB_PAY-BANKCOUNTRYCD
              LS_SMB_PAY-LOCALCOUNTRYCODE
              LS_SMB_PAY-CUSTOMSINFONO
              LS_SMB_PAY-TAX_FILE_NUMBER1   "130
              LS_SMB_PAY-TAX_FILE_NUMBER2
              LS_SMB_PAY-TAX_FILE_NUMBER3
              LS_SMB_PAY-TAX_HOLDER_NAME
              LS_SMB_PAY-TAX_PAYMENT_CODE
              LS_SMB_PAY-DETAILS_OF_PAYMENT
              LS_SMB_PAY-TAX_PERIOD_FROM
              LS_SMB_PAY-TAX_PERIOD_TO
              LS_SMB_PAY-TAX_PAYMENT_YEAR
              LS_SMB_PAY-REFERENCE_NUMBER
              LS_SMB_PAY-TAX_PAYER_NAME      "140
              LS_SMB_PAY-EXCISE_TAX_REFERENCE
              LS_SMB_PAY-TAX_PAYER_ID
              LS_SMB_PAY-REVENUE_S_CONTROL_CODE
              LS_SMB_PAY-DUE_DATE_MEA_S_INVOICE
              LS_SMB_PAY-INVOICE_NO_MEA_S_INVOICE
              LS_SMB_PAY-COMPANY_ID_SSO
              LS_SMB_PAY-COMPANY_S_BRANCH_ID
              LS_SMB_PAY-TERM_OF_PAYMENT
              LS_SMB_PAY-NO_OF_COMPANY_S_BRANCH
              LS_SMB_PAY-REGISTRATION_NO     "150
              LS_SMB_PAY-SUBMISSION_DATE
              LS_SMB_PAY-NO_OF_EMPLOYEE
              LS_SMB_PAY-CONTROL_CODE1
              LS_SMB_PAY-CONTROL_CODE2
              LS_SMB_PAY-TAX_PAYER_SIGNATURE_NAME
              LS_SMB_PAY-WHT_CERTIFICATE
              LS_SMB_PAY-MESSENGER_NAME
              LS_SMB_PAY-EMPLOYER
              LS_SMB_PAY-MESSENGER_ID
              LS_SMB_PAY-MESSENGER_TEL       "160
              LS_SMB_PAY-BILLER_CODE
              LS_SMB_PAY-BILLER_NAME
              LS_SMB_PAY-BILLER_CODE_NAME
              LS_SMB_PAY-BILLER_REF1
              LS_SMB_PAY-BILLER_REF2
              LS_SMB_PAY-REF_INFORMATION
              LS_SMB_PAY-BILLING_CODE
              LS_SMB_PAY-TAX_CODE
              LS_SMB_PAY-CDF_NO
              LS_SMB_PAY-CDF_DATE             "170
              LS_SMB_PAY-ON_BEHALF_OF
              LS_SMB_PAY-ON_BEHALF_OF_TAX_PAYER_NAME
              LS_SMB_PAY-ON_BEHALF_OF_TAX_PAYER_ADDR
              LS_SMB_PAY-ON_BEHALF_OF_TAX_PAYER_CODE
              LS_SMB_PAY-EXPORT_DUTIES
              LS_SMB_PAY-IMPORT_DUTIES
              LS_SMB_PAY-VALUE_ADDED_TAX
              LS_SMB_PAY-ENVIRONMENT_PROTECTION_TAX
              LS_SMB_PAY-SPECIAL_SALES_TAX
              LS_SMB_PAY-SAFE_GUARD_ANTIDUMPING_DUTIES  "180
              LS_SMB_PAY-CUSTOMS_FEE
              LS_SMB_PAY-TAX_TOAL_AMOUNT
              LS_SMB_PAY-TRESURYCD
              LS_SMB_PAY-TREASURYNAME
              LS_SMB_PAY-CHAPTER1
              LS_SMB_PAY-ECONOMICCD1
              LS_SMB_PAY-TERM1
              LS_SMB_PAY-DTAXAMOUNT1
              LS_SMB_PAY-CHAPTER2
              LS_SMB_PAY-ECONOMICCD2          "190
              LS_SMB_PAY-TERM2
              LS_SMB_PAY-DTAXAMOUNT2
              LS_SMB_PAY-DTAXTOTALAMOUNT
              LS_SMB_PAY-PAYMENTBYID
              LS_SMB_PAY-PERSONALID_TAXID
              LS_SMB_PAY-MOBILENO             "196
     INTO LS_FILE
     SEPARATED BY GC_SEPARATOR.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_APPEND_INV
*---------------------------------------------------------------------*
* Append data to record type INS (Payment)
*---------------------------------------------------------------------*
FORM F_SMB_APPEND_INV  USING    US_SMB_INV TYPE TS_SMB_INV
                       CHANGING CT_FILE TYPE TT_FILE.
  DATA: LS_FILE    TYPE TS_FILE,
        LS_SMB_INV TYPE TS_SMB_INV.

  IF US_SMB_INV IS INITIAL.
    RETURN.
  ENDIF.

  LS_SMB_INV = US_SMB_INV.

  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = LS_SMB_INV-INVOICEAMOUNT.

  CONDENSE LS_SMB_INV-INVOICEAMOUNT NO-GAPS.
  CONDENSE LS_SMB_INV-WHTAMOUNT NO-GAPS.
  CONDENSE LS_SMB_INV-GROSS_AMOUNT NO-GAPS.

* Put " for text field
  CONCATENATE GC_QUOTE LS_SMB_INV-REFERENCE GC_QUOTE INTO LS_SMB_INV-REFERENCE.
  CONCATENATE GC_QUOTE LS_SMB_INV-DESCRIPTION GC_QUOTE INTO LS_SMB_INV-DESCRIPTION.

  CONCATENATE LS_SMB_INV-INV
              LS_SMB_INV-DEFAULTID
              LS_SMB_INV-REFERENCE
              LS_SMB_INV-CALCFOR
              LS_SMB_INV-INVOICEDATE
              LS_SMB_INV-INVOICEAMOUNT
              LS_SMB_INV-VATAMOUNT
              LS_SMB_INV-VATRATE
              LS_SMB_INV-WHTAMOUNT
              LS_SMB_INV-WHTTYPE
              LS_SMB_INV-DESCRIPTION
              LS_SMB_INV-GROSS_AMOUNT
              LS_SMB_INV-INV_RESERVE2
   INTO LS_FILE
   SEPARATED BY GC_SEPARATOR.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_WRITE_NUMBER
*---------------------------------------------------------------------*
* Write number to 16 digit without decimal notation
* Example input: NNNN.DDD
*         output: 000000000NNNNDDD
*---------------------------------------------------------------------*
FORM F_WRITE_NUMBER  USING    UF_PAY_AMT TYPE ZSDSDE_PAY_AMT
                     CHANGING CF_DEBITAMOUNT TYPE CHAR16.

  DATA: LF_NUMC16(16) TYPE N.

  IF UF_PAY_AMT IS NOT INITIAL.
    WRITE UF_PAY_AMT TO CF_DEBITAMOUNT DECIMALS 3.      "#EC UOM_IN_MES
    REPLACE ALL OCCURRENCES OF '.' IN CF_DEBITAMOUNT WITH ''.
    SHIFT CF_DEBITAMOUNT RIGHT DELETING TRAILING SPACE.
    LF_NUMC16 = CF_DEBITAMOUNT.
  ELSE.
    CLEAR LF_NUMC16.
  ENDIF.
  CF_DEBITAMOUNT = LF_NUMC16.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_DOWNLOAD_FILE_LOCAL_SCB
*---------------------------------------------------------------------*
* Download file to local PC
*---------------------------------------------------------------------*
FORM F_DOWNLOAD_FILE_LOCAL_SCB  USING UT_FILE TYPE TT_FILE
                            CHANGING CF_FILENAME TYPE STRING.

  DATA: LF_FILENAME      TYPE STRING,
        LF_FILENAME_PATH TYPE STRING.
  DATA: LF_FILETYPE TYPE CHAR10 VALUE 'ASC',
        LF_CODEPAGE TYPE ABAP_ENCODING VALUE '4110',
        LF_SUFFIX   TYPE CHAR3.

  CLEAR: CF_FILENAME.

  IF UT_FILE IS INITIAL.
    RETURN.
  ENDIF.

  LF_FILETYPE = 'ASC'. "'DAT'.
  LF_CODEPAGE = '8600'.

  IF RB_DIREC EQ ABAP_TRUE.
    LF_SUFFIX = 'DIR'.
  ELSEIF RB_SMART EQ ABAP_TRUE.
    LF_SUFFIX = 'SMT'.
  ENDIF.

  GET TIME.
  IF LF_SUFFIX IS INITIAL.
    CONCATENATE 'SDS'
                P_HBKID(3)
                'OUT'
                SY-DATUM
                SY-UZEIT
      INTO LF_FILENAME SEPARATED BY '_'.
  ELSE.
    CONCATENATE 'SDS'
                P_HBKID(3)
                'OUT'
                LF_SUFFIX
                SY-DATUM
                SY-UZEIT
      INTO LF_FILENAME SEPARATED BY '_'.
  ENDIF.

  CONDENSE LF_FILENAME NO-GAPS.
  CONCATENATE LF_FILENAME GC_FILE_TXT INTO LF_FILENAME.

  CONCATENATE P_OFILE '\' LF_FILENAME INTO LF_FILENAME_PATH.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                  = LF_FILENAME_PATH
      FILETYPE                  = LF_FILETYPE
      WRITE_FIELD_SEPARATOR     = SPACE
      CODEPAGE                  = LF_CODEPAGE
      TRUNC_TRAILING_BLANKS     = SPACE
      WRITE_LF                  = SPACE
      TRUNC_TRAILING_BLANKS_EOL = SPACE
    TABLES
      DATA_TAB                  = UT_FILE
    EXCEPTIONS
      FILE_WRITE_ERROR          = 1
      NO_BATCH                  = 2
      GUI_REFUSE_FILETRANSFER   = 3
      INVALID_TYPE              = 4
      NO_AUTHORITY              = 5
      UNKNOWN_ERROR             = 6
      HEADER_NOT_ALLOWED        = 7
      SEPARATOR_NOT_ALLOWED     = 8
      FILESIZE_NOT_ALLOWED      = 9
      HEADER_TOO_LONG           = 10
      DP_ERROR_CREATE           = 11
      DP_ERROR_SEND             = 12
      DP_ERROR_WRITE            = 13
      UNKNOWN_DP_ERROR          = 14
      ACCESS_DENIED             = 15
      DP_OUT_OF_MEMORY          = 16
      DISK_FULL                 = 17
      DP_TIMEOUT                = 18
      FILE_NOT_FOUND            = 19
      DATAPROVIDER_EXCEPTION    = 20
      CONTROL_FLUSH_ERROR       = 21
      OTHERS                    = 22.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*   Output file & was generated
    MESSAGE S010(ZSDSFI01) WITH LF_FILENAME.

    CF_FILENAME = LF_FILENAME.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_DOWNLOAD_FILE_LOCAL_SMB
*---------------------------------------------------------------------*
* Download file to local PC
*---------------------------------------------------------------------*
FORM F_DOWNLOAD_FILE_LOCAL_SMB  USING UT_FILE TYPE TT_FILE
                            CHANGING CF_FILENAME TYPE STRING.

  DATA: LF_FILENAME      TYPE STRING,
        LF_FILENAME_PATH TYPE STRING.
  DATA: LF_FILETYPE TYPE CHAR10 VALUE 'ASC',
        LF_CODEPAGE TYPE ABAP_ENCODING VALUE '4110',
        LF_SUFFIX   TYPE CHAR3.

  CLEAR: CF_FILENAME.

  IF UT_FILE IS INITIAL.
    RETURN.
  ENDIF.

  LF_FILETYPE = 'DAT'. "'ASC'.
  LF_CODEPAGE = '8600'. "'4110'.

  GET TIME.
  IF LF_SUFFIX IS INITIAL.
    CONCATENATE 'SDS'
                P_HBKID(3)
                'OUT'
                SY-DATUM
                SY-UZEIT
      INTO LF_FILENAME SEPARATED BY '_'.
  ELSE.
    CONCATENATE 'SDS'
                P_HBKID(3)
                'OUT'
                LF_SUFFIX
                SY-DATUM
                SY-UZEIT
      INTO LF_FILENAME SEPARATED BY '_'.
  ENDIF.

  CONDENSE LF_FILENAME NO-GAPS.
  CONCATENATE LF_FILENAME GC_FILE_TXT INTO LF_FILENAME.

  CONCATENATE P_OFILE '\' LF_FILENAME INTO LF_FILENAME_PATH.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = LF_FILENAME_PATH
      FILETYPE                = LF_FILETYPE
      WRITE_FIELD_SEPARATOR   = SPACE
      CODEPAGE                = LF_CODEPAGE
    TABLES
      DATA_TAB                = UT_FILE
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*   Output file & was generated
    MESSAGE S010(ZSDSFI01) WITH LF_FILENAME.

    CF_FILENAME = LF_FILENAME.
  ENDIF.

ENDFORM.
**---------------------------------------------------------------------*
** Form F_CREATE_INTERFACE_FILE
**---------------------------------------------------------------------*
** Download file to application server
**---------------------------------------------------------------------*
FORM F_CREATE_INTERFACE_FILE USING UT_FILE TYPE TT_FILE  ##CALLED
                             CHANGING CF_FILENAME TYPE STRING.

  DATA: LS_RETURN     TYPE BAPIRET2,
        LF_INTFNO     TYPE ZSDSCAC004-INTFNO,
        LF_FILENAME   TYPE STRING,
        LF_SUFFIX     TYPE CHAR3,
        LF_FILEPATH_O TYPE STRING   ##NEEDED,
        LF_FILENAME_O TYPE STRING   ##NEEDED.

  DATA: LT_DATATXT TYPE ZCL_SDSCA_FILE_INTERFACE=>TT_DATATXT.

  CLEAR: CF_FILENAME.

  IF UT_FILE IS INITIAL.
    RETURN.
  ENDIF.

  READ TABLE GT_HBKID_H2H INTO DATA(LS_H2H)
                          WITH KEY HOUSE_BANK = P_HBKID(3).
  IF SY-SUBRC NE 0.
    RETURN.
  ELSE.
    LF_INTFNO = LS_H2H-INTFNO.
  ENDIF.

  LT_DATATXT[] = UT_FILE[].

  IF P_HBKID(3) EQ GC_HBKID-SCB.
    IF RB_DIREC EQ ABAP_TRUE.
      LF_SUFFIX = 'DIR'.
    ELSEIF RB_SMART EQ ABAP_TRUE.
      LF_SUFFIX = 'SMT'.
    ENDIF.
  ELSE.
    CLEAR LF_SUFFIX.
  ENDIF.

  IF LF_SUFFIX IS INITIAL.
    CONCATENATE 'SDS'
                P_HBKID(3)
                'OUT'
                SY-DATUM
                SY-UZEIT
      INTO LF_FILENAME SEPARATED BY '_'.
  ELSE.
    CONCATENATE 'SDS'
                P_HBKID(3)
                'OUT'
                LF_SUFFIX
                SY-DATUM
                SY-UZEIT
      INTO LF_FILENAME SEPARATED BY '_'.
  ENDIF.

  CONCATENATE LF_FILENAME GC_FILE_TXT INTO LF_FILENAME.

  CALL METHOD ZCL_SDSCA_FILE_INTERFACE=>CREATE_INTERFACE_FILE
    EXPORTING
      IF_INTFNO   = LF_INTFNO
      IF_FILENAME = LF_FILENAME
      IT_DATATXT  = LT_DATATXT
    IMPORTING
      ES_RETURN   = LS_RETURN
      EF_FILEPATH = LF_FILEPATH_O
      EF_FILENAME = LF_FILENAME_O.

  IF LS_RETURN-TYPE NE 'S'.
*   Error
    MESSAGE ID LS_RETURN-ID TYPE 'I'
            NUMBER LS_RETURN-NUMBER
            DISPLAY LIKE LS_RETURN-TYPE
            WITH LS_RETURN-MESSAGE_V1 LS_RETURN-MESSAGE_V2
                 LS_RETURN-MESSAGE_V3 LS_RETURN-MESSAGE_V4.
    RETURN.
  ELSE.
    CF_FILENAME = LF_FILENAME.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_UPDATE_LOG_TABLE
*---------------------------------------------------------------------*
* Update payment document to log table ZSDSFIT047
*---------------------------------------------------------------------*
FORM F_UPDATE_LOG_TABLE  USING UT_RESULT TYPE TT_RESULT  ##CALLED
                               UF_FILENAME TYPE STRING.

  DATA: LT_LOG TYPE STANDARD TABLE OF ZSDSFIT047,
        LS_LOG TYPE ZSDSFIT047.

  CHECK CB_TEST IS INITIAL AND
        UT_RESULT IS NOT INITIAL AND
        UF_FILENAME IS NOT INITIAL.

  LT_LOG[] = CORRESPONDING #( UT_RESULT MAPPING BELNR = PAYDOC_BELNR
                                                GJAHR = PAYDOC_GJAHR ).

  SORT LT_LOG BY BUKRS GJAHR BELNR.
  DELETE ADJACENT DUPLICATES FROM LT_LOG COMPARING BUKRS GJAHR BELNR.

  LS_LOG-STATUS = GC_TRUE.
  LS_LOG-FILENAME = UF_FILENAME.
  LS_LOG-ZCRT_DATE = SY-DATUM.
  LS_LOG-ZCRT_TIME = SY-UZEIT.
  LS_LOG-ZCRT_USER = SY-UNAME.
  LS_LOG-ZCRT_PGM = SY-REPID.

  IF RB_H2H EQ 'X'.
    LS_LOG-STATUS_H2H = GC_TRUE.
  ENDIF.

  MODIFY LT_LOG FROM LS_LOG
         TRANSPORTING STATUS STATUS_H2H FILENAME
         ZCRT_DATE ZCRT_TIME ZCRT_USER ZCRT_PGM
         WHERE FILENAME IS INITIAL.
  IF SY-SUBRC EQ 0.
    MODIFY ZSDSFIT047 FROM TABLE LT_LOG.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_FILTER_EXPORTED_DOC
*---------------------------------------------------------------------*
* To filter out payment document which already been exported
*---------------------------------------------------------------------*
FORM F_FILTER_EXPORTED_DOC  CHANGING CT_RESULT TYPE TT_RESULT.
  DATA: LT_LOG TYPE STANDARD TABLE OF ZSDSFIT047.

  CHECK CT_RESULT IS NOT INITIAL.

  LT_LOG[] = CORRESPONDING #( CT_RESULT MAPPING BELNR = PAYDOC_BELNR
                                                GJAHR = PAYDOC_GJAHR ).

  SORT LT_LOG BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM LT_LOG COMPARING BUKRS BELNR GJAHR.

  IF LT_LOG IS NOT INITIAL.
    SELECT *                                  "#EC CI_ALL_FIELDS_NEEDED
      FROM ZSDSFIT047
      INTO TABLE @DATA(LT_LOG_DB)
      FOR ALL ENTRIES IN @LT_LOG
      WHERE BUKRS EQ @LT_LOG-BUKRS
        AND BELNR EQ @LT_LOG-BELNR
        AND GJAHR EQ @LT_LOG-GJAHR
        AND STATUS EQ @ABAP_TRUE.
  ENDIF.

  LOOP AT LT_LOG_DB ASSIGNING FIELD-SYMBOL(<L_LOG_DB>).
*   Remove payment document which already been exported to file
    DELETE CT_RESULT WHERE BUKRS = <L_LOG_DB>-BUKRS
                       AND PAYDOC_BELNR = <L_LOG_DB>-BELNR
                       AND PAYDOC_GJAHR = <L_LOG_DB>-GJAHR.
  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_F4_PAYDOC
*---------------------------------------------------------------------*
* F4 Help for payment document number to be unlocked
*---------------------------------------------------------------------*
FORM F_F4_PAYDOC  USING UF_DYN_FLD TYPE HELP_INFO-DYNPROFLD.

  DATA: LF_RET_FIELD TYPE DFIES-FIELDNAME VALUE 'BELNR'.

  CHECK P_PAYYR IS NOT INITIAL.

  SELECT *
    FROM ZSDSFIT047
    INTO TABLE @DATA(LT_LOG)
    WHERE BUKRS EQ @P_BUKRS2
      AND GJAHR EQ @P_PAYYR
      AND STATUS EQ @GC_TRUE.

  IF SY-SUBRC EQ 0.
    SORT LT_LOG BY FILENAME BUKRS BELNR GJAHR.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        RETFIELD     = LF_RET_FIELD
        DYNPPROG     = SY-REPID
        DYNPNR       = SY-DYNNR
        DYNPROFIELD  = UF_DYN_FLD
        WINDOW_TITLE = TEXT-F01
        VALUE_ORG    = 'S'
      TABLES
        VALUE_TAB    = LT_LOG.

  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_F4_LAUFI
*---------------------------------------------------------------------*
* F4 Help for payment document number to be unlocked
*---------------------------------------------------------------------*
FORM F_F4_LAUFI  USING UF_DYN_FLD TYPE HELP_INFO-DYNPROFLD.
  DATA: LF_LAUFI TYPE F110V-LAUFI ##NEEDED.

  DATA: LT_TLAUFK TYPE TABLE OF ILAUFK.

  CLEAR: LT_TLAUFK.

  INSERT VALUE #( SIGN   = 'I'
                  LAUFK = SPACE )
         INTO TABLE LT_TLAUFK.

  CALL FUNCTION 'F4_ZAHLLAUF'
    EXPORTING
      F1TYP = 'D'
      F2NME = UF_DYN_FLD
    IMPORTING
      LAUFD = P_LAUFD
      LAUFI = LF_LAUFI
    TABLES
      LAUFK = LT_TLAUFK.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_DATA_UNLOCK
*---------------------------------------------------------------------*
* Get data for unlock document from table ZSDSFIT047
*---------------------------------------------------------------------*
FORM F_GET_DATA_UNLOCK  CHANGING CT_RESULT_LOG TYPE TT_RESULT_LOG.
  CLEAR: CT_RESULT_LOG.

  SELECT *
    FROM ZSDSFIT047
    INTO TABLE CT_RESULT_LOG
    WHERE BUKRS EQ P_BUKRS2
      AND BELNR IN S_PAYDOC
      AND GJAHR EQ P_PAYYR
      AND STATUS EQ ABAP_TRUE.

  IF SY-SUBRC EQ 0.
    SORT CT_RESULT_LOG BY BUKRS BELNR GJAHR.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_UNLOCK_DOCUMENT
*---------------------------------------------------------------------*
* Unlock document (set status = blank) in table ZSDSFIT047
*---------------------------------------------------------------------*
FORM F_UNLOCK_DOCUMENT  CHANGING CT_RESULT_LOG TYPE TT_RESULT_LOG.
  DATA: LS_LOG TYPE ZSDSFIT047.

  CHECK CT_RESULT_LOG IS NOT INITIAL.

  LS_LOG-STATUS = SPACE.
  LS_LOG-ZUPD_DATE = SY-DATUM.
  LS_LOG-ZUPD_TIME = SY-UZEIT.
  LS_LOG-ZUPD_USER = SY-UNAME.
  LS_LOG-ZUPD_PGM = SY-REPID.

  MODIFY CT_RESULT_LOG FROM LS_LOG
         TRANSPORTING STATUS ZUPD_DATE ZUPD_TIME ZUPD_USER ZUPD_PGM
         WHERE STATUS IS NOT INITIAL.

  IF SY-SUBRC EQ 0.
    MODIFY ZSDSFIT047 FROM TABLE CT_RESULT_LOG.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_GEN_FILE
*---------------------------------------------------------------------*
* Append data to file SMB
*---------------------------------------------------------------------*
FORM F_SMB_GEN_FILE  USING    US_SMB_HEADER TYPE TS_SMB_HEADER
                              UT_SMB_PAY TYPE TT_SMB_PAY
                              UT_SMB_INV TYPE TT_SMB_INV
                     CHANGING CT_FILE TYPE TT_FILE.

  DATA: LS_PREV_PAY TYPE TS_SMB_PAY.

  PERFORM F_SMB_APPEND_HEADER USING US_SMB_HEADER
                           CHANGING CT_FILE.

  LOOP AT UT_SMB_PAY ASSIGNING FIELD-SYMBOL(<L_SMB_PAY>).

    IF ( <L_SMB_PAY>-ZNEW_ENV_FLG EQ GC_TRUE AND SY-TABIX > 1 )
       OR
       ( <L_SMB_PAY>-DEFAULTID NE LS_PREV_PAY-DEFAULTID AND LS_PREV_PAY-ZNEW_ENV_FLG EQ GC_TRUE ).

      PERFORM F_SMB_APPEND_HEADER USING US_SMB_HEADER
                               CHANGING CT_FILE.
    ENDIF.

    PERFORM F_SMB_APPEND_PAY USING <L_SMB_PAY>
                          CHANGING CT_FILE.

    LOOP AT UT_SMB_INV ASSIGNING FIELD-SYMBOL(<L_SMB_INV>)
                          WHERE DEFAULTID EQ <L_SMB_PAY>-DEFAULTID
                            AND ZSPLIT_ID EQ <L_SMB_PAY>-ZSPLIT_ID.

      PERFORM F_SMB_APPEND_INV USING <L_SMB_INV>
                            CHANGING CT_FILE.

    ENDLOOP.

    LS_PREV_PAY = <L_SMB_PAY>.

  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_SMB_SPLIT_RECORD
*---------------------------------------------------------------------*
* Split record by checking configuration in table ZSDSFIC031
* Conditions:
*   If Total Payment amount in one INS > ZSDSFIC031-LIMIT_AMOUNT
*                                      <= ZSDSFIC031-MAX_PAYMENT
*   Then
*    - Split Invoice amount in each INV record to <= ZSDSFIC031-LIMIT_AMOUNT
*
*   Also check Total number of INV line per 1 INS must <= P_MAXINV
*---------------------------------------------------------------------*
FORM F_SMB_SPLIT_RECORD  CHANGING CT_SMB_PAY TYPE TT_SMB_PAY
                                  CT_SMB_INV TYPE TT_SMB_INV.
  TYPES: BEGIN OF LS_PAY_SUM,
           DEFAULTID     TYPE TS_SMB_PAY-DEFAULTID,
           ZSPLIT_ID     TYPE TS_SMB_PAY-ZSPLIT_ID,
           INVOICEAMOUNT TYPE BSEG-DMBTR,
           PAYMENTAMOUNT TYPE BSEG-DMBTR,
           WHTAMOUNT     TYPE BSEG-DMBTR,
         END OF LS_PAY_SUM.
  DATA: LS_PAY_SUM       TYPE  LS_PAY_SUM,
        LT_PAY_SUM       TYPE STANDARD TABLE OF LS_PAY_SUM,
        LT_PAY_SUM_SPLIT TYPE STANDARD TABLE OF LS_PAY_SUM.    "CH01+

  DATA: LT_SMB_PAY TYPE TT_SMB_PAY,
        LT_SMB_INV TYPE TT_SMB_INV.

  DATA: LF_PAYMENTAMOUNT TYPE BSEG-DMBTR,
        LF_INVOICEAMOUNT TYPE BSEG-DMBTR,
        LF_SUM_INV_AMT   TYPE BSEG-DMBTR,
        LF_TABIX         TYPE SY-TABIX,
        LF_ZSPLIT_ID(20) TYPE N,
        LF_NEXT_RECORD   TYPE SY-TABIX.       "CH03+

  FIELD-SYMBOLS: <L_PAY_SUM> TYPE LS_PAY_SUM,
                 <L_SMB_PAY> TYPE TS_SMB_PAY.

* Table to Control Split Limit Payment Amount
  SELECT *
    FROM ZSDSFIC031                                     "#EC CI_NOWHERE
    INTO TABLE @DATA(LT_ZSDSFIC031)
    ORDER BY PRIMARY KEY.

  LOOP AT CT_SMB_PAY INTO DATA(LS_SMB_PAY) ##INTO_OK.
    CLEAR: LF_SUM_INV_AMT, LF_ZSPLIT_ID.
    CLEAR: LS_PAY_SUM.

    MOVE-CORRESPONDING LS_SMB_PAY TO LS_PAY_SUM.
    CLEAR: LS_PAY_SUM-PAYMENTAMOUNT.

*   Read start index of the same DEFAULTID in INV record
    READ TABLE CT_SMB_INV WITH KEY DEFAULTID = LS_SMB_PAY-DEFAULTID
                          TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      LF_TABIX = SY-TABIX.
    ELSE.
      CONTINUE.
    ENDIF.

*----------------------------------------------------------
* Check ZSDSFIC031 for bank code to be splitted the record
*----------------------------------------------------------
    READ TABLE LT_ZSDSFIC031 INTO DATA(LS_ZSDSFIC031)
                             WITH KEY BANK_CODE = LS_SMB_PAY-BANKCODE(3)
                             BINARY SEARCH.
    IF SY-SUBRC NE 0.
*     Bank code not found in configuration, no need to split by amount
      LOOP AT CT_SMB_INV ASSIGNING FIELD-SYMBOL(<L_SMB_INV>) FROM LF_TABIX.
        IF <L_SMB_INV>-DEFAULTID NE LS_SMB_PAY-DEFAULTID .
          EXIT.
        ENDIF.
        APPEND <L_SMB_INV> TO LT_SMB_INV.

        LS_PAY_SUM-ZSPLIT_ID = <L_SMB_INV>-ZSPLIT_ID.
        LS_PAY_SUM-INVOICEAMOUNT = <L_SMB_INV>-INVOICEAMOUNT.
        LS_PAY_SUM-WHTAMOUNT = <L_SMB_INV>-WHTAMOUNT.

        COLLECT LS_PAY_SUM INTO LT_PAY_SUM.
      ENDLOOP.

      CONTINUE.
    ENDIF.

*----------------------------------------------------------
*   Split record if payment amount between ZSDSFIC031-LIMIT_AMOUNT and MAX_PAYMENT
    LF_PAYMENTAMOUNT = LS_SMB_PAY-PAYMENTAMOUNT.
    IF LF_PAYMENTAMOUNT GT LS_ZSDSFIC031-LIMIT_AMOUNT AND
       LF_PAYMENTAMOUNT LE LS_ZSDSFIC031-MAX_PAYMENT.

      LF_ZSPLIT_ID  =  1.

      LOOP AT CT_SMB_INV INTO DATA(LS_SMB_INV) FROM LF_TABIX.

        IF LS_SMB_INV-DEFAULTID NE LS_SMB_PAY-DEFAULTID .
          EXIT.
        ENDIF.

        LF_INVOICEAMOUNT = LS_SMB_INV-INVOICEAMOUNT.

        IF LS_SMB_INV-REFERENCE = GC_WHT AND
           LS_SMB_INV-INVOICEAMOUNT IS INITIAL.
*         This is INV WHT line, no need to split
          LS_SMB_INV-ZSPLIT_ID = LF_ZSPLIT_ID.
          APPEND LS_SMB_INV TO LT_SMB_INV.

          LS_PAY_SUM-ZSPLIT_ID = LS_SMB_INV-ZSPLIT_ID.
          LS_PAY_SUM-INVOICEAMOUNT =  LS_SMB_INV-INVOICEAMOUNT.

          CLEAR: LS_PAY_SUM-WHTAMOUNT.              "CH01+
*          LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT.  "CH01-

*           COLLECT LS_PAY_SUM INTO LT_PAY_SUM.       "CH01-
          COLLECT LS_PAY_SUM INTO LT_PAY_SUM_SPLIT.   "CH01+
        ELSE.
          DO.
            LS_SMB_INV-ZSPLIT_ID = LF_ZSPLIT_ID.

            IF LF_INVOICEAMOUNT GT LS_ZSDSFIC031-LIMIT_AMOUNT.
              IF LF_SUM_INV_AMT IS NOT INITIAL.
                LF_ZSPLIT_ID = LF_ZSPLIT_ID + 1.
                LS_SMB_INV-ZSPLIT_ID = LF_ZSPLIT_ID.
              ENDIF.

              LS_SMB_INV-INVOICEAMOUNT = LS_ZSDSFIC031-LIMIT_AMOUNT.
              CONDENSE LS_SMB_INV-INVOICEAMOUNT NO-GAPS.
              APPEND LS_SMB_INV TO LT_SMB_INV.

              LS_PAY_SUM-ZSPLIT_ID = LS_SMB_INV-ZSPLIT_ID.
              LS_PAY_SUM-INVOICEAMOUNT =  LS_SMB_INV-INVOICEAMOUNT.
              LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT.

*              COLLECT LS_PAY_SUM INTO LT_PAY_SUM.       "CH01-
              COLLECT LS_PAY_SUM INTO LT_PAY_SUM_SPLIT.  "CH01+

              LF_INVOICEAMOUNT = LF_INVOICEAMOUNT - LS_ZSDSFIC031-LIMIT_AMOUNT.
              LF_ZSPLIT_ID = LF_ZSPLIT_ID + 1.

              CLEAR: LF_SUM_INV_AMT.
            ELSE.    "LF_INVOICEAMOUNT <= LS_ZSDSFIC031-LIMIT_AMOUNT.

              LF_SUM_INV_AMT = LF_SUM_INV_AMT + LF_INVOICEAMOUNT.

* BOI - 420000241 - CH02 - 15.01.2025
              IF LF_SUM_INV_AMT GT LS_ZSDSFIC031-LIMIT_AMOUNT.
                LF_ZSPLIT_ID = LF_ZSPLIT_ID + 1.
                CLEAR: LF_SUM_INV_AMT.

                LS_SMB_INV-ZSPLIT_ID = LF_ZSPLIT_ID.
                LS_SMB_INV-INVOICEAMOUNT = LF_INVOICEAMOUNT.
                CONDENSE LS_SMB_INV-INVOICEAMOUNT NO-GAPS.
                APPEND LS_SMB_INV TO LT_SMB_INV.

                LS_PAY_SUM-ZSPLIT_ID = LF_ZSPLIT_ID.
                LS_PAY_SUM-INVOICEAMOUNT =  LS_SMB_INV-INVOICEAMOUNT.

                LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT_INV.

                COLLECT LS_PAY_SUM INTO LT_PAY_SUM_SPLIT.

* EOI - 420000241 - CH02 - 15.01.2025
*              IF LF_SUM_INV_AMT GE LS_ZSDSFIC031-LIMIT_AMOUNT.   "CH02+
              ELSEIF LF_SUM_INV_AMT EQ LS_ZSDSFIC031-LIMIT_AMOUNT.   "CH02+

                LS_SMB_INV-INVOICEAMOUNT = LF_INVOICEAMOUNT.
                CONDENSE LS_SMB_INV-INVOICEAMOUNT NO-GAPS.
                APPEND LS_SMB_INV TO LT_SMB_INV.

                LS_PAY_SUM-ZSPLIT_ID = LS_SMB_INV-ZSPLIT_ID.
                LS_PAY_SUM-INVOICEAMOUNT =  LS_SMB_INV-INVOICEAMOUNT.
*                LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT.     "CH02-

                LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT_INV.  "CH02+

*                COLLECT LS_PAY_SUM INTO LT_PAY_SUM.        "CH01-
                COLLECT LS_PAY_SUM INTO LT_PAY_SUM_SPLIT.   "CH01+

                LF_ZSPLIT_ID = LF_ZSPLIT_ID + 1.
                CLEAR: LF_SUM_INV_AMT.

              ELSE.
                LS_SMB_INV-INVOICEAMOUNT = LF_INVOICEAMOUNT.
                CONDENSE LS_SMB_INV-INVOICEAMOUNT NO-GAPS.
                APPEND LS_SMB_INV TO LT_SMB_INV.

                LS_PAY_SUM-ZSPLIT_ID = LS_SMB_INV-ZSPLIT_ID.
                LS_PAY_SUM-INVOICEAMOUNT =  LS_SMB_INV-INVOICEAMOUNT.
*                LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT.     "CH02-

                LS_PAY_SUM-WHTAMOUNT = LS_SMB_INV-WHTAMOUNT_INV.  "CH01+
*                COLLECT LS_PAY_SUM INTO LT_PAY_SUM.       "CH01-
                COLLECT LS_PAY_SUM INTO LT_PAY_SUM_SPLIT.  "CH01+

              ENDIF.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDLOOP.

    ELSE.
*     Payment amount not between ZSDSFIC031-LIMIT_AMOUNT and ZSDSFIC031-MAX_PAYMENT
*     No need to split
      READ TABLE CT_SMB_INV WITH KEY DEFAULTID = LS_SMB_PAY-DEFAULTID
                            TRANSPORTING NO FIELDS.
      IF SY-SUBRC EQ 0.
        LF_TABIX = SY-TABIX.
        LOOP AT CT_SMB_INV ASSIGNING <L_SMB_INV> FROM LF_TABIX.
          IF <L_SMB_INV>-DEFAULTID NE LS_SMB_PAY-DEFAULTID .
            EXIT.
          ENDIF.
          APPEND <L_SMB_INV> TO LT_SMB_INV.

          LS_PAY_SUM-ZSPLIT_ID = <L_SMB_INV>-ZSPLIT_ID.
          LS_PAY_SUM-INVOICEAMOUNT = <L_SMB_INV>-INVOICEAMOUNT.
          LS_PAY_SUM-WHTAMOUNT = <L_SMB_INV>-WHTAMOUNT.

          COLLECT LS_PAY_SUM INTO LT_PAY_SUM.

        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDLOOP.

*-----------------------------------------------------------------------
* Prepare payment document which is no need to split by amount
  LOOP AT LT_PAY_SUM ASSIGNING <L_PAY_SUM>.

    READ TABLE CT_SMB_PAY ASSIGNING <L_SMB_PAY>
                          WITH KEY DEFAULTID = <L_PAY_SUM>-DEFAULTID.
    IF SY-SUBRC EQ 0.

      LS_SMB_PAY = <L_SMB_PAY>.
      LS_SMB_PAY-ZSPLIT_ID = <L_PAY_SUM>-ZSPLIT_ID.
      IF LS_SMB_PAY-ZSPLIT_ID IS NOT INITIAL.
        LS_SMB_PAY-ZNEW_ENV_FLG = GC_TRUE.
      ENDIF.

      LF_PAYMENTAMOUNT = <L_PAY_SUM>-INVOICEAMOUNT -
                         <L_PAY_SUM>-WHTAMOUNT .
      LS_SMB_PAY-PAYMENTAMOUNT = LF_PAYMENTAMOUNT.
      CONDENSE LS_SMB_PAY-PAYMENTAMOUNT NO-GAPS.

      LS_SMB_PAY-WHTAMOUNT = <L_PAY_SUM>-WHTAMOUNT.
      CONDENSE LS_SMB_PAY-WHTAMOUNT NO-GAPS.

      APPEND LS_SMB_PAY TO LT_SMB_PAY.
    ENDIF.
  ENDLOOP.

* BOI - CH01 - 14.11.2024
*-----------------------------------------------------------------------
* Prepare payment document which is split by amount
  LOOP AT LT_PAY_SUM_SPLIT ASSIGNING <L_PAY_SUM>.
    LF_NEXT_RECORD = SY-TABIX + 1.                 "CH03+

    READ TABLE CT_SMB_PAY ASSIGNING <L_SMB_PAY>
                          WITH KEY DEFAULTID = <L_PAY_SUM>-DEFAULTID.
    IF SY-SUBRC EQ 0.

      LS_SMB_PAY = <L_SMB_PAY>.
      LS_SMB_PAY-ZSPLIT_ID = <L_PAY_SUM>-ZSPLIT_ID.
      IF LS_SMB_PAY-ZSPLIT_ID IS NOT INITIAL.
        LS_SMB_PAY-ZNEW_ENV_FLG = GC_TRUE.
      ENDIF.

      LF_PAYMENTAMOUNT = <L_PAY_SUM>-INVOICEAMOUNT -
                         <L_PAY_SUM>-WHTAMOUNT .

      LS_SMB_PAY-INVOICEAMOUNT = <L_PAY_SUM>-INVOICEAMOUNT. "CH01+
      LS_SMB_PAY-PAYMENTAMOUNT = LF_PAYMENTAMOUNT.
      CONDENSE LS_SMB_PAY-PAYMENTAMOUNT NO-GAPS.

      LS_SMB_PAY-WHTAMOUNT = <L_PAY_SUM>-WHTAMOUNT.
      CONDENSE LS_SMB_PAY-WHTAMOUNT NO-GAPS.

* Begin of insertion - CH03 - 420000698 - 23.07.2025
* If INS line is EWHT (WHT_CERTIFICATE = 05),
* Set WHT_CERTIFICATE of the group to 02
* except the last INS of this group, keep it to 05
      IF LS_SMB_PAY-WHT_CERTIFICATE = '05'.
*       Check whether this record is the last of the group or not?
        READ TABLE LT_PAY_SUM_SPLIT INTO DATA(LS_PAY_SUM_SPLIT_NEXT)
                                    INDEX LF_NEXT_RECORD.
        IF SY-SUBRC EQ 0.
          IF LS_PAY_SUM_SPLIT_NEXT-DEFAULTID EQ <L_PAY_SUM>-DEFAULTID.
*           This is not the last INS of this group
            LS_SMB_PAY-WHT_CERTIFICATE = '02'.
          ENDIF.
        ENDIF.
      ENDIF.
* End of insertion - CH03 - 420000698 - 23.07.2025

      APPEND LS_SMB_PAY TO LT_SMB_PAY.
    ENDIF.
  ENDLOOP.
* EOI - CH01 - 14.11.2024

  CT_SMB_PAY[] = LT_SMB_PAY[].
  CT_SMB_INV[] = LT_SMB_INV[].

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_HOUSE_BANK_KEY
*---------------------------------------------------------------------*
* Get SDS House Bank Key
*---------------------------------------------------------------------*
FORM F_GET_HOUSE_BANK_KEY  CHANGING CF_BANKL TYPE T012-BANKL.
  CLEAR: CF_BANKL.

  SELECT SINGLE BANKL
    INTO CF_BANKL
    FROM T012
    WHERE BUKRS EQ P_BUKRS
      AND HBKID EQ P_HBKID.
  IF SY-SUBRC NE 0.
    CLEAR: CF_BANKL.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GENERATE_SMART
*---------------------------------------------------------------------*
* SCB Generate file for option SMART
*---------------------------------------------------------------------*
FORM F_SCB_GENERATE_SMART  USING    UT_RESULT TYPE TT_RESULT
                           CHANGING CT_FILE TYPE TT_FILE.

  DATA: LF_COUNT_DEBIT(6)  TYPE N,
        LF_COUNT_CREDIT(6) TYPE N,
        LF_TOTAL_AMOUNT    TYPE ZSDSDE_PAY_AMT.

* Record type 001: Header record
  PERFORM F_SCB_GEN_HEADER_SMART USING UT_RESULT
                              CHANGING CT_FILE.

* Record type 002: Debit Record
* Record type 003: Credit Record
  PERFORM F_SCB_GEN_DEBIT_SMART USING UT_RESULT
                             CHANGING CT_FILE
                                      LF_COUNT_DEBIT
                                      LF_COUNT_CREDIT
                                      LF_TOTAL_AMOUNT.

* Record type 999: Trailer Record
  PERFORM F_SCB_GEN_TRAILER USING LF_COUNT_DEBIT
                                  LF_COUNT_CREDIT
                                  LF_TOTAL_AMOUNT
                         CHANGING CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GENERATE_DIRECT
*---------------------------------------------------------------------*
* SCB Generate file for option Direct
*---------------------------------------------------------------------*
FORM F_SCB_GENERATE_DIRECT  USING   UT_RESULT TYPE TT_RESULT
                           CHANGING CT_FILE TYPE TT_FILE.

  DATA: LF_COUNT_DEBIT(6)  TYPE N,
        LF_COUNT_CREDIT(6) TYPE N,
        LF_TOTAL_AMOUNT    TYPE ZSDSDE_PAY_AMT.

* Record type 001: Header record
  PERFORM F_SCB_GEN_HEADER_DIRECT USING UT_RESULT
                               CHANGING CT_FILE.

* Record type 002: Debit Record
* Record type 003: Credit Record
  PERFORM F_SCB_GEN_DEBIT_DIRECT USING UT_RESULT
                              CHANGING CT_FILE
                                       LF_COUNT_DEBIT
                                       LF_COUNT_CREDIT
                                       LF_TOTAL_AMOUNT.

* Record type 999: Trailer Record
  PERFORM F_SCB_GEN_TRAILER USING LF_COUNT_DEBIT
                                  LF_COUNT_CREDIT
                                  LF_TOTAL_AMOUNT
                         CHANGING CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_HEADER_direct
*---------------------------------------------------------------------*
* Populate header record to SCB file (Record type 001)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_HEADER_DIRECT  USING UT_RESULT TYPE TT_RESULT
                           CHANGING CT_FILE TYPE TT_FILE.

  DATA: LS_FILE       TYPE TS_FILE,
        LS_SCB_HEADER TYPE TS_SCB_HEADER_DIRECT.

  READ TABLE UT_RESULT INTO DATA(LS_RESULT) INDEX 1.
  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LS_SCB_HEADER-RECORDTYPE = GC_SCB_RECTYPE-HEADER.
  LS_SCB_HEADER-COMPANYID = GS_GENC_SCB-SCB_COMPANY_ID.

  IF LS_RESULT-KTOKK IN GRT_VENGRP_EMP.
    LS_SCB_HEADER-DESCRIPTION = TEXT-T02.   "Employee
  ELSE.
    LS_SCB_HEADER-DESCRIPTION = TEXT-T01.   "Market Place
  ENDIF.

  LS_SCB_HEADER-FILEDATE = SY-DATUM.
  LS_SCB_HEADER-FILETIME = '000000'.   "Fix to 000000
  LS_SCB_HEADER-CHANNELID = GS_GENC_SCB-SCB_CHANNEL_ID.

  LS_SCB_HEADER-BATCHREF = ' '.

  CONCATENATE LS_SCB_HEADER-RECORDTYPE
              LS_SCB_HEADER-COMPANYID
              LS_SCB_HEADER-DESCRIPTION
              LS_SCB_HEADER-FILEDATE
              LS_SCB_HEADER-FILETIME
              LS_SCB_HEADER-CHANNELID
              LS_SCB_HEADER-BATCHREF
   INTO LS_FILE RESPECTING BLANKS.

  CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
    INTO LS_FILE RESPECTING BLANKS.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_DEBIT_DIRECT
*---------------------------------------------------------------------*
* Populate debit record to SCB file (Record type 002)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_DEBIT_DIRECT  USING UT_RESULT TYPE TT_RESULT
                          CHANGING CT_FILE TYPE TT_FILE
                                   CF_COUNT_DEBIT TYPE NUM06
                                   CF_COUNT_CREDIT TYPE NUM06
                                   CF_TOTAL_AMOUNT TYPE ZSDSDE_PAY_AMT.

  DATA: LS_FILE                 TYPE TS_FILE,
        LS_SCB_DEBIT            TYPE TS_SCB_DEBIT_DIRECT,
        LS_SUM_BY_VALUE_DATE    TYPE TS_SUM_BY_VALUE_DATE,
        LS_SUM_CREDIT_BY_VENDOR TYPE TS_SUM_CREDIT_BY_VENDOR,
        LS_PREVIOUS             TYPE TS_RESULT,
        LF_INTERNAL_REF(8)      TYPE N,
        LF_VAT_REGIS_PAYER      TYPE T001-STCEG.

  DATA: LT_SUM_BY_VALUE_DATE TYPE TT_SUM_BY_VALUE_DATE,
        LT_SUM_BY_VENDOR     TYPE TT_SUM_CREDIT_BY_VENDOR,
        LT_RESULT_CREDIT     TYPE TT_RESULT.

  CLEAR: CF_COUNT_DEBIT,
         CF_COUNT_CREDIT,
         CF_TOTAL_AMOUNT,
         LF_VAT_REGIS_PAYER,
         LT_SUM_BY_VALUE_DATE,
         LT_SUM_BY_VENDOR,
         LT_RESULT_CREDIT.

* VAT registration no. of the payer
  SELECT SINGLE STCEG
    FROM T001
    INTO LF_VAT_REGIS_PAYER
    WHERE BUKRS EQ P_BUKRS.
  IF SY-SUBRC NE 0.
    CLEAR: LF_VAT_REGIS_PAYER.
  ELSE.
    LF_VAT_REGIS_PAYER = LF_VAT_REGIS_PAYER+2(13).
  ENDIF.

* Prepare sum of number credit and payment amount
* 1 Output file will have only 1 record type 001 (Debit Detail)
  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>).
    CLEAR: LS_SUM_BY_VALUE_DATE, LS_SUM_CREDIT_BY_VENDOR.

    IF <L_RESULT>-PAYDOC_BELNR EQ LS_PREVIOUS-PAYDOC_BELNR AND
       <L_RESULT>-PAYDOC_GJAHR EQ LS_PREVIOUS-PAYDOC_GJAHR.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING <L_RESULT> TO LS_SUM_CREDIT_BY_VENDOR.

    COLLECT LS_SUM_CREDIT_BY_VENDOR INTO LT_SUM_BY_VENDOR.

    LS_PREVIOUS = <L_RESULT>.

    LS_SUM_BY_VALUE_DATE-UBKNT = <L_RESULT>-UBKNT.
    LS_SUM_BY_VALUE_DATE-VALUT = <L_RESULT>-VALUT.
    LS_SUM_BY_VALUE_DATE-PAY_AMT = <L_RESULT>-PAY_AMT.

    COLLECT LS_SUM_BY_VALUE_DATE INTO LT_SUM_BY_VALUE_DATE.
  ENDLOOP.

  LT_RESULT_CREDIT[] = CORRESPONDING #( LT_SUM_BY_VENDOR[] ).

* Group Data by Company bank (UBKNT) and Value date (VALUT)
  LOOP AT LT_SUM_BY_VALUE_DATE ASSIGNING FIELD-SYMBOL(<L_SUM>).
*   Record type 002 (Debit)
    CLEAR: LS_SCB_DEBIT, LF_INTERNAL_REF.

*   Count total of debit record and sum total payment amount
    CF_COUNT_DEBIT = CF_COUNT_DEBIT + 1.
    CF_TOTAL_AMOUNT = CF_TOTAL_AMOUNT + <L_SUM>-PAY_AMT.

*   Internal reference = Running number for credit/debit record grouping
    LF_INTERNAL_REF = LF_INTERNAL_REF + 1.

    LS_SCB_DEBIT-RECORDTYPE = GC_SCB_RECTYPE-DEBIT.
    LS_SCB_DEBIT-PRODUCTCODE = 'PAY'.
    LS_SCB_DEBIT-VALUEDATE = <L_SUM>-VALUT.
    LS_SCB_DEBIT-DEBITACCOUNT = <L_SUM>-UBKNT.
    CONCATENATE '0' <L_SUM>-UBKNT+3(1) INTO LS_SCB_DEBIT-ACCOUNTTYPE.
    CONCATENATE '0' <L_SUM>-UBKNT+0(3) INTO LS_SCB_DEBIT-DEBITBRANCH.
    LS_SCB_DEBIT-DEBITCURR = GC_THB.

    PERFORM F_WRITE_NUMBER USING <L_SUM>-PAY_AMT
                           CHANGING LS_SCB_DEBIT-DEBITAMOUNT.

    LS_SCB_DEBIT-INTERNALREF = LF_INTERNAL_REF. "GS_GENC_SCB-SCB_INTERNAL_REF.

    LS_SCB_DEBIT-NUMCREDIT = LINES( LT_RESULT_CREDIT ).

    LS_SCB_DEBIT-FEEDEBITACC = <L_SUM>-UBKNT.

    CONCATENATE '0' <L_SUM>-UBKNT+3(1) INTO LS_SCB_DEBIT-ACCOUNTTYPEFEE.
    CONCATENATE '0' <L_SUM>-UBKNT+0(3) INTO LS_SCB_DEBIT-DEBITBRANCHFEE.

    CONCATENATE LS_SCB_DEBIT-RECORDTYPE
                LS_SCB_DEBIT-PRODUCTCODE
                LS_SCB_DEBIT-VALUEDATE
                LS_SCB_DEBIT-DEBITACCOUNT
                LS_SCB_DEBIT-ACCOUNTTYPE
                LS_SCB_DEBIT-DEBITBRANCH
                LS_SCB_DEBIT-DEBITCURR
                LS_SCB_DEBIT-DEBITAMOUNT
                LS_SCB_DEBIT-INTERNALREF
                LS_SCB_DEBIT-NUMCREDIT
                LS_SCB_DEBIT-FEEDEBITACC
                LS_SCB_DEBIT-FILLER
                LS_SCB_DEBIT-MEDIACLEARINGCYCLE
                LS_SCB_DEBIT-ACCOUNTTYPEFEE
                LS_SCB_DEBIT-DEBITBRANCHFEE
       INTO LS_FILE RESPECTING BLANKS.

    CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
      INTO LS_FILE RESPECTING BLANKS.

    APPEND LS_FILE TO CT_FILE.

*   Record type 003 (Credit)
    PERFORM F_SCB_GEN_CREDIT_DIRECT USING <L_SUM>-UBKNT
                                          <L_SUM>-VALUT
                                          LT_RESULT_CREDIT
                                          LS_SCB_DEBIT-INTERNALREF
                                 CHANGING CT_FILE
                                          CF_COUNT_CREDIT.

  ENDLOOP.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_CREDIT_DIRECT
*---------------------------------------------------------------------*
* Populate credit record to SCB file (Record type 003)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_CREDIT_DIRECT USING UF_UBKNT TYPE TS_RESULT-UBKNT
                                   UF_VALUT TYPE TS_RESULT-VALUT
                                   UT_RESULT TYPE TT_RESULT
                                   UF_INTERNAL_REF TYPE CHAR8
                          CHANGING CT_FILE TYPE TT_FILE
                                   CF_COUNT_CREDIT TYPE NUM06.

  DATA: LS_FILE       TYPE TS_FILE,
        LS_SCB_CREDIT TYPE TS_SCB_CREDIT_DIRECT.
  DATA: LF_CREDITSEQNO(6) TYPE N.

  IF UF_UBKNT IS INITIAL OR
     UF_VALUT IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR: LF_CREDITSEQNO.
* Loop invoice document under the same value date
  LOOP AT UT_RESULT ASSIGNING FIELD-SYMBOL(<L_RESULT>)
                    WHERE UBKNT EQ UF_UBKNT
                      AND VALUT EQ UF_VALUT.

    CLEAR: LS_SCB_CREDIT, LS_FILE.

    CF_COUNT_CREDIT = CF_COUNT_CREDIT + 1.
    LF_CREDITSEQNO = LF_CREDITSEQNO + 1.

    LS_SCB_CREDIT-RECORDTYPE = GC_SCB_RECTYPE-CREDIT.
    LS_SCB_CREDIT-CREDITSEQNO = LF_CREDITSEQNO.
    LS_SCB_CREDIT-CREDITACCOUNT = <L_RESULT>-BANKN.

    PERFORM F_WRITE_NUMBER USING <L_RESULT>-PAY_AMT
                           CHANGING LS_SCB_CREDIT-CREDITAMOUNT.

    LS_SCB_CREDIT-CREDITCURR = GC_THB.
    LS_SCB_CREDIT-INTERNALREF = UF_INTERNAL_REF.
    LS_SCB_CREDIT-WHTPRESENT = GS_GENC_SCB-SCB_WHT_PRESENT.
    LS_SCB_CREDIT-INVOICEPRESENT = GS_GENC_SCB-SCB_INV_PRESENT.
    LS_SCB_CREDIT-CREDITADVICEREQ = GS_GENC_SCB-SCB_CREDIT_ADVICE_REQ.
    LS_SCB_CREDIT-DELIVERYMODE = GS_GENC_SCB-SCB_DELIVERY_MODE.

    LS_SCB_CREDIT-TOTALWHTAMOUNT = '0000000000000000'.
    LS_SCB_CREDIT-TOTALINVOICEAMOUNT = '0000000000000000'.
    LS_SCB_CREDIT-RECVBANKCODE = <L_RESULT>-BANKL(3).
    LS_SCB_CREDIT-RECVBANKNAME = GS_GENC_SCB-SCB_RECV_BANK_NAME.
    LS_SCB_CREDIT-RECVBRANCHCODE = <L_RESULT>-BANKL+3(4).
    LS_SCB_CREDIT-BENEFICIARY = GS_GENC_SCB-SCB_BENEFICIARY_NOTI.

    CONCATENATE <L_RESULT>-PAYDOC_BELNR
                <L_RESULT>-PAYDOC_GJAHR
                INTO LS_SCB_CREDIT-CUSTREF.

    LS_SCB_CREDIT-SERVICETYPE = GS_GENC_SCB-SCB_SERVICE_TYPE.

    LS_SCB_CREDIT-REMARK = LS_SCB_CREDIT-CUSTREF.

    LS_SCB_CREDIT-BENEFICIARYCHARGE = ' '.

    CONCATENATE LS_SCB_CREDIT-RECORDTYPE
                LS_SCB_CREDIT-CREDITSEQNO
                LS_SCB_CREDIT-CREDITACCOUNT
                LS_SCB_CREDIT-CREDITAMOUNT
                LS_SCB_CREDIT-CREDITCURR
                LS_SCB_CREDIT-INTERNALREF
                LS_SCB_CREDIT-WHTPRESENT
                LS_SCB_CREDIT-INVOICEPRESENT
                LS_SCB_CREDIT-CREDITADVICEREQ
                LS_SCB_CREDIT-DELIVERYMODE
                LS_SCB_CREDIT-PICKUPLOCATION
                LS_SCB_CREDIT-WHTFORMTYPE
                LS_SCB_CREDIT-WHTRUNNINGNO
                LS_SCB_CREDIT-WHTATTACHNO
                LS_SCB_CREDIT-NUMWHTDETAIL
                LS_SCB_CREDIT-TOTALWHTAMOUNT
                LS_SCB_CREDIT-NUMINVOICEDETAIL
                LS_SCB_CREDIT-TOTALINVOICEAMOUNT
                LS_SCB_CREDIT-WHTPAYTYPE
                LS_SCB_CREDIT-WHTREMARK
                LS_SCB_CREDIT-WHTDEDUCTDATE
                LS_SCB_CREDIT-RECVBANKCODE
                LS_SCB_CREDIT-RECVBANKNAME
                LS_SCB_CREDIT-RECVBRANCHCODE
                LS_SCB_CREDIT-RECVBRANCHNAME
                LS_SCB_CREDIT-WHTSIGNATORY
                LS_SCB_CREDIT-BENEFICIARY
                LS_SCB_CREDIT-CUSTREF
                LS_SCB_CREDIT-CHEQUEREFDOCTYPE
                LS_SCB_CREDIT-PAYMENTTYPECODE
                LS_SCB_CREDIT-SERVICETYPE
                LS_SCB_CREDIT-REMARK
                LS_SCB_CREDIT-SCBREMARK
                LS_SCB_CREDIT-BENEFICIARYCHARGE
      INTO LS_FILE RESPECTING BLANKS.

    CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
      INTO LS_FILE RESPECTING BLANKS.

    APPEND LS_FILE TO CT_FILE.

*   Record type 004 (Payee Details)
    PERFORM F_SCB_GEN_PAYEE_DIRECT  USING <L_RESULT>
                                          LS_SCB_CREDIT-INTERNALREF
                                          LS_SCB_CREDIT-CREDITSEQNO
                                 CHANGING CT_FILE.

  ENDLOOP.
ENDFORM.
*---------------------------------------------------------------------*
* Form F_SCB_GEN_PAYEE_DIRECT
*---------------------------------------------------------------------*
* Populate payee detail to SCB file (Record type 004)
*---------------------------------------------------------------------*
FORM F_SCB_GEN_PAYEE_DIRECT  USING    US_RESULT TYPE TS_RESULT
                                      UF_DEBIT_INTERNALREF TYPE TS_SCB_DEBIT_DIRECT-INTERNALREF
                                      UF_CREDIT_CREDITSEQNO TYPE TS_SCB_CREDIT_DIRECT-CREDITSEQNO
                             CHANGING CT_FILE TYPE TT_FILE.

  DATA: LS_FILE      TYPE TS_FILE,
        LS_SCB_PAYEE TYPE TS_SCB_PAYEE_DETAIL.

  IF US_RESULT IS INITIAL.
    RETURN.
  ENDIF.

  LS_SCB_PAYEE-RECORDTYPE = GC_SCB_RECTYPE-PAYEE.
  LS_SCB_PAYEE-INTERNALREF = UF_DEBIT_INTERNALREF.
  LS_SCB_PAYEE-CREDITSEQNO = UF_CREDIT_CREDITSEQNO.
  LS_SCB_PAYEE-PAYEE1NAMETH = US_RESULT-PAYEE_NAME_EN.
  LS_SCB_PAYEE-PAYEE1EMAIL = US_RESULT-EMAIL.

  LS_SCB_PAYEE-PAYEE2ADDR3 = ' '.

  CONCATENATE LS_SCB_PAYEE-RECORDTYPE
              LS_SCB_PAYEE-INTERNALREF
              LS_SCB_PAYEE-CREDITSEQNO
              LS_SCB_PAYEE-PAYEE1IDCARD
              LS_SCB_PAYEE-PAYEE1NAMETH
              LS_SCB_PAYEE-PAYEE1ADDR1
              LS_SCB_PAYEE-PAYEE1ADDR2
              LS_SCB_PAYEE-PAYEE1ADDR3
              LS_SCB_PAYEE-PAYEE1TAXID
              LS_SCB_PAYEE-PAYEE1NAMEEN
              LS_SCB_PAYEE-PAYEE1FAX
              LS_SCB_PAYEE-PAYEE1MOBILE
              LS_SCB_PAYEE-PAYEE1EMAIL
              LS_SCB_PAYEE-PAYEE2NAMETH
              LS_SCB_PAYEE-PAYEE2ADDR1
              LS_SCB_PAYEE-PAYEE2ADDR2
              LS_SCB_PAYEE-PAYEE2ADDR3
    INTO LS_FILE RESPECTING BLANKS.

  CONCATENATE LS_FILE CL_ABAP_CHAR_UTILITIES=>CR_LF
    INTO LS_FILE RESPECTING BLANKS.

  APPEND LS_FILE TO CT_FILE.

ENDFORM.
*---------------------------------------------------------------------*
* Form F_GET_EWHT_MAPPING
*---------------------------------------------------------------------*
* Get eWHT mapping table for SMBC from ZSDSFIC032
*---------------------------------------------------------------------*
FORM F_GET_EWHT_MAPPING  USING    UF_WITHT TYPE WITH_ITEM-WITHT
                                  UF_WT_WITHCD TYPE WITH_ITEM-WT_WITHCD
                         CHANGING CF_SMBC_WHTTYPE TYPE TS_SMB_INV-WHTTYPE
                                  CF_SMBC_DESCRIPTION TYPE TS_SMB_INV-DESCRIPTION.

  STATICS:
    LS_ZSDSFIC032 TYPE ZSDSFIC032,
    LT_ZSDSFIC032 TYPE SORTED TABLE OF ZSDSFIC032
                  WITH UNIQUE KEY BUKRS WITHT WT_WITHCD.

  CLEAR: CF_SMBC_WHTTYPE, CF_SMBC_DESCRIPTION.

* Check Buffer
  IF LS_ZSDSFIC032-WITHT NE UF_WITHT OR
     LS_ZSDSFIC032-WT_WITHCD NE UF_WT_WITHCD.
*   Validate with Memory
    READ TABLE LT_ZSDSFIC032 INTO LS_ZSDSFIC032
                        WITH KEY BUKRS = P_BUKRS
                                 WITHT = UF_WITHT
                                 WT_WITHCD = UF_WT_WITHCD
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR LS_ZSDSFIC032.

*     Get from ZSDSFIC032
      SELECT SINGLE *
        INTO LS_ZSDSFIC032
        FROM ZSDSFIC032
       WHERE BUKRS EQ P_BUKRS
         AND WITHT EQ UF_WITHT
         AND WT_WITHCD = UF_WT_WITHCD.
      IF SY-SUBRC NE 0.
        CF_SMBC_WHTTYPE = '99'.
        CF_SMBC_DESCRIPTION = 'EWHT'.
        RETURN.
      ENDIF.
      INSERT LS_ZSDSFIC032 INTO TABLE LT_ZSDSFIC032.
    ENDIF.
  ENDIF.

* Assign Output
  CF_SMBC_WHTTYPE = LS_ZSDSFIC032-BANK_WITHTYPE.
  CF_SMBC_DESCRIPTION = LS_ZSDSFIC032-WITHTYPE_TH.

ENDFORM.
* BOI - CH01 - 14.11.2024
*----------------------------------------------------------------------*
*       Form  F_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       ALV TOP-OF-PAGE Event
*----------------------------------------------------------------------*
FORM F_TOP_OF_PAGE_1 USING UREF_DYNDOC_ID  TYPE  REF TO CL_DD_DOCUMENT. "#EC CALLED

  CONSTANTS:
    LC_SIZE_KEY TYPE  SDYDO_VALUE  VALUE '25%',
    LC_SIZE_VAL TYPE  SDYDO_VALUE  VALUE '75%'.

  DATA:
    LF_TEMP1     TYPE  TEXT50,
    LF_TEMP2     TYPE  TEXT50,
    LF_TEXT      TYPE  SDYDO_TEXT_ELEMENT,
    LREF_TABLE   TYPE  REF TO CL_DD_TABLE_ELEMENT,
    LREF_COL_KEY TYPE  REF TO CL_DD_AREA,
    LREF_COL_VAL TYPE  REF TO CL_DD_AREA.

* Create table
  CALL METHOD UREF_DYNDOC_ID->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 2
      BORDER        = '0'
      WIDTH         = '50%'
    IMPORTING
      TABLE         = LREF_TABLE.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_KEY
    IMPORTING
      COLUMN = LREF_COL_KEY.
* Get Column Element
  CALL METHOD LREF_TABLE->ADD_COLUMN
    EXPORTING
      WIDTH  = LC_SIZE_VAL
    IMPORTING
      COLUMN = LREF_COL_VAL.
* Set Key column style
  CALL METHOD LREF_TABLE->SET_COLUMN_STYLE
    EXPORTING
      COL_NO       = 1
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*-----------------------
* Add value in Line1
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h01 : Date/Time:
  LF_TEXT = TEXT-H01.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-DATUM TO LF_TEMP1.
  WRITE GS_PROC-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line2
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h02 : User:
  LF_TEXT = TEXT-H02.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-UNAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line3
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h03 : Filename:
  LF_TEXT = TEXT-H03.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  LF_TEXT = GS_PROC-FILENAME.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.

*-----------------------
* Add value in Line4
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h04 : Total Invoice (THB):
  LF_TEXT = TEXT-H04.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-TOTINVAMT TO LF_TEXT CURRENCY GC_THB.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_TOTAL.

*-----------------------
* Add value in Line5
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h05 : Total WHT (THB):
  LF_TEXT = TEXT-H05.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE GS_PROC-TOTWHTAMT TO LF_TEXT CURRENCY GC_THB.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_HEADING_INT.

*-----------------------
* Add value in Line6
*-----------------------
  CALL METHOD LREF_TABLE->NEW_ROW.
* Text-h06 : Net Paid (THB):
  LF_TEXT = TEXT-H06.
  CALL METHOD LREF_COL_KEY->ADD_TEXT
    EXPORTING
      TEXT = LF_TEXT.
  WRITE  GS_PROC-TOTPAYAMT TO LF_TEXT CURRENCY GC_THB.
  CONDENSE LF_TEXT NO-GAPS.
  CALL METHOD LREF_COL_VAL->ADD_TEXT
    EXPORTING
      TEXT      = LF_TEXT
      SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE.


ENDFORM.
*----------------------------------------------------------------------*
*       Form  F_PRINT_TOP_OF_PAGE_1
*----------------------------------------------------------------------*
*       Print Top of Page
*----------------------------------------------------------------------*
FORM F_PRINT_TOP_OF_PAGE_1.                                 "#EC CALLED

  DATA:
    LF_COL01 TYPE  I VALUE 20,
    LF_COL02 TYPE  I VALUE 40,
    LF_TEXT  TYPE  TEXT50,
    LF_TEMP1 TYPE  TEXT50,
    LF_TEMP2 TYPE  TEXT50.

* Text-h01 : Date/Time:
  WRITE GS_PROC-DATUM TO LF_TEMP1.
  WRITE GS_PROC-UZEIT TO LF_TEMP2.
  LF_TEXT = |{ LF_TEMP1 }/{ LF_TEMP2 }|.
  WRITE AT: /1(LF_COL01)  TEXT-H01 INTENSIFIED ON NO-GAP,
            (LF_COL02)    LF_TEXT NO-GAP.

* Text-h02 : User:
  WRITE AT: /1(LF_COL01)  TEXT-H02 INTENSIFIED ON NO-GAP,
            (LF_COL02)    GS_PROC-UNAME NO-GAP.

* Text-h03 : Filename:
  WRITE AT: /1(LF_COL01)  TEXT-H03 INTENSIFIED ON NO-GAP,
            (150)         GS_PROC-FILENAME NO-GAP ##NUMBER_OK.

* Text-h04 : Total Invoice (THB):
  WRITE GS_PROC-TOTINVAMT TO LF_TEXT CURRENCY GC_THB.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H04 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_HEADING.

* Text-h05 : Total WHT (THB):
  WRITE GS_PROC-TOTWHTAMT TO LF_TEXT CURRENCY GC_THB.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H05 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_HEADING.

* Text-h06 : Net Paid (THB):
  WRITE  GS_PROC-TOTPAYAMT TO LF_TEXT CURRENCY GC_THB.
  CONDENSE LF_TEXT NO-GAPS.
  WRITE AT: /1(LF_COL01)  TEXT-H06 INTENSIFIED ON NO-GAP,
            (10)    LF_TEXT NO-GAP COLOR COL_POSITIVE.

ENDFORM.
* EOI - CH01 - 14.11.2024
* BOI - 420000241 - CH02 - 14.01.2025
*---------------------------------------------------------------------*
* Form F_FI_PERIOD_DETERMINE
*---------------------------------------------------------------------*
* Determine FI fiscal year from posting date
*---------------------------------------------------------------------*
FORM F_FI_PERIOD_DETERMINE  USING    UF_ZALDT TYPE REGUH-ZALDT
                            CHANGING CF_GJAHR TYPE BKPF-GJAHR.

  DATA: LF_GJAHR TYPE BKPF-GJAHR.

  IF UF_ZALDT IS INITIAL.
    RETURN.
  ENDIF.

  CLEAR: CF_GJAHR.

  CALL FUNCTION 'FI_PERIOD_DETERMINE'
    EXPORTING
      I_BUDAT        = UF_ZALDT
      I_BUKRS        = P_BUKRS
    IMPORTING
      E_GJAHR        = LF_GJAHR
    EXCEPTIONS
      FISCAL_YEAR    = 1
      PERIOD         = 2
      PERIOD_VERSION = 3
      POSTING_PERIOD = 4
      SPECIAL_PERIOD = 5
      VERSION        = 6
      POSTING_DATE   = 7
      OTHERS         = 8.
  IF SY-SUBRC EQ 0.
    CF_GJAHR = LF_GJAHR.
  ENDIF.

ENDFORM.
* EOI - 420000241 - CH02 - 14.01.2025
