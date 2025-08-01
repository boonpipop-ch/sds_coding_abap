FUNCTION Z_SDSSD_SALES_RESULT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

*    DATA : LT_DATA TYPE GTY_DATA.
*
*    SELECT *
*      FROM ZSDSVC_SALES_RESULT( P_VKORG = @P_VKORG )
*      WHERE VTWEG IN @S_VTWEG[]
*        AND SPART IN @S_SPART[]
*        AND VKBUR IN @S_VKBUR[]
*        AND VKGRP IN @S_VKGRP[]
*        AND KUNAG IN @S_KUNAG[]
*        AND FKART IN @S_FKART[]
*        AND FKDAT IN @S_FKDAT[]
*        AND VBELN IN @S_VBELN[]
*        AND MATNR IN @S_MATNR[]
*        AND PRODH IN @S_PRODH[]
*        AND PERNR IN @S_PERNR[]
*        AND BSTKD IN @S_BSTKD[]
*        AND KVGR2 IN @S_KVGR2[]
*        AND BNAME IN @S_BNAME[]
*      INTO TABLE @LT_DATA.



ENDFUNCTION.
