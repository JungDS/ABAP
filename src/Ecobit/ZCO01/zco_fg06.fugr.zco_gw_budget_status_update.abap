FUNCTION ZCO_GW_BUDGET_STATUS_UPDATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_GWKEY) TYPE  ZEGWKEY
*"     VALUE(I_STATUS) TYPE  ZEGWSTS
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  CHAR1
*"     VALUE(E_MSG) TYPE  CHAR255
*"----------------------------------------------------------------------

  DATA LV_FNAME TYPE FUNCNAME.
  DATA LV_BELNR TYPE BELNR_D.

  DATA LV_RESULT TYPE C.
  DATA LV_MSG    TYPE CHAR255.

  SELECT SINGLE * FROM ZCOT1190
    INTO @DATA(LS_ZCOT1190)
   WHERE GWKEY = @I_GWKEY
     AND GWSTS <> 'Z9'.

  IF SY-SUBRC <> 0.
    E_RESULT = 'E'.
    MESSAGE S000(ZCO01) WITH TEXT-E04
     INTO E_MSG.
    EXIT.
  ENDIF.

  CASE I_STATUS.

    WHEN 'S0'.  "결재 상신

      IF LS_ZCOT1190-GWSTS <> 'I0'.

        E_RESULT = 'E'.
        MESSAGE S055(ZCO01) INTO E_MSG.
        EXIT.

      ENDIF.

  ENDCASE.

  UPDATE ZCOT1190 SET GWSTS = @I_STATUS
   WHERE GWKEY = @I_GWKEY.

  IF SY-SUBRC = 0 .

    COMMIT WORK AND WAIT.

    E_RESULT = 'S'.
    MESSAGE S007(ZCO01) INTO E_MSG.

    IF I_STATUS = 'A0'.   "최종승인

      CASE LS_ZCOT1190-GWTYP.

        WHEN 'P' OR 'C'.   "예산전용 기간 & 증감액
          LV_FNAME = 'ZCO_GW_BUDGET_POSTING_' &&
                      'P'.
        WHEN 'A'.          "예산전용 계정
          LV_FNAME = 'ZCO_GW_BUDGET_POSTING_' &&
                     LS_ZCOT1190-GWTYP.
      ENDCASE.

      CALL FUNCTION LV_FNAME
        EXPORTING
          I_GWKEY  = I_GWKEY
        IMPORTING
          E_RESULT = LV_RESULT
          E_MSG    = LV_MSG
          E_BELNR  = LV_BELNR.

      CASE LV_RESULT.

        WHEN 'S'.
          UPDATE ZCOT1190 SET RDOCNR = @LV_BELNR,
                              GWSTS  = 'Z0'
           WHERE GWKEY = @I_GWKEY.
          COMMIT WORK.

      ENDCASE.

    ENDIF.

  ELSE.

    ROLLBACK WORK.

    E_RESULT = 'E'.
    MESSAGE S008(ZCO01) INTO E_MSG.

  ENDIF.

ENDFUNCTION.
