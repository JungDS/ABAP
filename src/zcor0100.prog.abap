*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0100                                             &*
*& Title       : [CO] Proj, WBS 마스터 일괄 조회                      &*
*& Created By  : BSGABAP8                                             &*
*& Created On  : 2019.07.12                                           &*
*& Description : [CO] Proj, WBS 마스터 일괄 조회                      &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.07.12  BSGABAP8    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR0100 MESSAGE-ID ZCO01.

INCLUDE ZCOR0100T01.   " TOP-Decration
INCLUDE ZCOR0100ALV.   " Class ALV OR Others
INCLUDE ZCOR0100SCR.   " Selection-Screen
INCLUDE ZCOR0100F01.   " Subroutine

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITAIL.
  PERFORM SET_INIT_HELP.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND_HELP.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM SET_SCREEN.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  PERFORM INTIAL_VALUE_CHECK.
  PERFORM SELECTED_DATA_RTN.
  PERFORM SALV_CALL.
