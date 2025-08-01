*&---------------------------------------------------------------------*
*& Include          ZSDSCMR0140_SELECTION
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* P A R A M E T E R S   &   S E L E C T - O P T I O N S
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : S_PROCTH  FOR CRMS4D_SERV_H-PROCESS_TYPE,
                   S_PROCTI  FOR CRMS4D_SERV_I-PROCESS_TYPE,
                   S_OBJECT  FOR CRMS4D_SERV_H-OBJECT_ID,
                   S_POSING  FOR CRMS4D_SERV_H-POSTING_DATE.
SELECTION-SCREEN END OF BLOCK BLOCK1.
PARAMETERS P_AUTO AS CHECKBOX.
