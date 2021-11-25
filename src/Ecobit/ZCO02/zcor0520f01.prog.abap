*&---------------------------------------------------------------------*
*& Include          ZCOR0520F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  IF SY-TCODE EQ 'ZCOR0520'.
    GV_MODE = GC_E.
    SY-TITLE = TEXT-T01.  " [CO] 설비WBS 속성 관리
  ELSE.
    GV_MODE = GC_D.
    SY-TITLE = TEXT-T02.  " [CO] 설비WBS 속성 조회
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION .

  CASE ABAP_ON.
    WHEN PA_R01.
      IF GV_MODE EQ GC_E.
        CALL TRANSACTION 'ZCOV1270'.
      ELSE.
        CALL TRANSACTION 'ZCOV1271'.
      ENDIF.

    WHEN PA_R02.
      IF GV_MODE EQ GC_E.
        CALL TRANSACTION 'ZCOV1280'.
      ELSE.
        CALL TRANSACTION 'ZCOV1281'.
      ENDIF.

  ENDCASE.

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
    WHEN 'R02'.
      SUBMIT ZCOR0530 VIA SELECTION-SCREEN AND RETURN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
