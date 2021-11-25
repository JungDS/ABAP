*&--------------------------------------------------------------------*
*& Report ZCOR0010
*&--------------------------------------------------------------------*
*&-------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0010                                            &*
*& Title       : [CO] 코스트센터 생성                                 *
*& Created By  : BSGABAP4                                            &*
*& Created On  : 2019.06.05                                          &*
*& Description :                                                     &*
*---------------------------------------------------------------------*
* MODIFICATION LOG
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*

REPORT ZCOR0010 MESSAGE-ID ZCO01.

INCLUDE ZCOR0010T01. " TOP-Decration
INCLUDE ZCOR0010ALV. " Class ALV OR Others
INCLUDE ZCOR0010SCR. " Selection-Screen
INCLUDE ZCOR0010F01. " Subroutine

LOAD-OF-PROGRAM.

  PA_KOKRS  = '1000'.
*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITAIL.
  PERFORM SCRFIELDS_FUNCTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM LIST_BOX_SET.
  PERFORM SCREEN_SET.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND. "양식 Download

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_FILE.
*---------------------------------------------------------------------*
  PERFORM F4_FILE CHANGING PA_FILE.

**---------------------------------------------------------------------*
*AT SELECTION-SCREEN ON BLOCK B1.
**---------------------------------------------------------------------*
*  PERFORM CHECK_BUKRS_WITH_TKA01 CHANGING TKA02 PA_BUTXT.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  PERFORM EXCEL_UPLOAD_EXEC.
  CHECK GV_EXIT IS INITIAL.
  PERFORM DATA_CONVERT.

  PERFORM SALV_CALL.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*
