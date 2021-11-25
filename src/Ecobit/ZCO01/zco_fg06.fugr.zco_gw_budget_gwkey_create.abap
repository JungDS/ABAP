FUNCTION ZCO_GW_BUDGET_GWKEY_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_GWTYP) TYPE  ZEGWTYP
*"     REFERENCE(I_SPMON) TYPE  SPMON
*"  EXPORTING
*"     REFERENCE(E_GWKEY) TYPE  ZEGWKEY
*"  EXCEPTIONS
*"      INVALID_GWTYP
*"----------------------------------------------------------------------

  DATA LV_SEQ TYPE N LENGTH 7.

  CASE I_GWTYP.
    WHEN 'P' OR 'C' OR 'A'.
    WHEN OTHERS.
      RAISE INVALID_GWTYP.
      EXIT.
  ENDCASE.

  SELECT MAX( GWKEY ) INTO @DATA(LV_GWKEY)
    FROM ZCOT1190
   WHERE GWTYP = @I_GWTYP
     AND SPMON = @I_SPMON.

  LV_SEQ = LV_GWKEY+7(7).
  ADD 1 TO LV_SEQ.

*-- 채번 예산결재 유형 && 년월 && Seq
  E_GWKEY = I_GWTYP && I_SPMON && LV_SEQ.

ENDFUNCTION.
