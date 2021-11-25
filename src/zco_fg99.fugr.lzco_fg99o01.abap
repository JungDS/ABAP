*----------------------------------------------------------------------*
***INCLUDE LZCO_FG99O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module CHECK_INPUT_MODE OUTPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_MODE OUTPUT.

*  IF SY-TCODE EQ 'ZCOV1271'.
*    SET PF-STATUS 'ESLG' EXCLUDING 'AEND'.
*  ENDIF.

  APPEND 'AEND' TO excl_cua_funct.
  APPEND 'ANZG' TO excl_cua_funct.


ENDMODULE.
