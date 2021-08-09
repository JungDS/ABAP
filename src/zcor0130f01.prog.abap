*&---------------------------------------------------------------------*
*& Include          ZCOR0130F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION .

  IF PA_WW010 = 'X'.
    CALL TRANSACTION 'ZCOV1010'.
  ELSEIF PA_WW020 = 'X'.
    CALL TRANSACTION 'ZCOV1020'.
  ELSEIF PA_WW030 = 'X'.
    CALL TRANSACTION 'ZCOV1030'.
  ELSEIF PA_WW040 = 'X'.
    CALL TRANSACTION 'ZCOV1040'.
  ELSEIF PA_WW050 = 'X'.
    CALL TRANSACTION 'ZCOV1040'.
  ELSEIF PA_WW060 = 'X'.
    CALL TRANSACTION 'ZCOV0020'.
  ELSEIF PA_WW070 = 'X'.
    CALL TRANSACTION 'ZCOV1070'.
  ELSEIF PA_WW080 = 'X'.
    CALL TRANSACTION 'ZCOV1120'.
  ELSEIF PA_WW090 = 'X'.
    CALL TRANSACTION 'ZCOV1090'.
  ELSEIF PA_WW100 = 'X'.
    CALL TRANSACTION 'ZCOV1100'.
  ELSEIF PA_WW110 = 'X'.
    CALL TRANSACTION 'ZCOV1110'.
  ELSEIF PA_WW120 = 'X'.
    CALL TRANSACTION 'ZCOV1130'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_INIT_HELP .

  "__ Function Key
  DATA: LS_FUNTXT TYPE SMP_DYNTXT.

  LS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  LS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = LS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND_HELP .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
