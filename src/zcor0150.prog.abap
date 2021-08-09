*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0150                                             &*
*& Title       : [CO] 버전별 리포트계정그룹 유지보수                  &*
*& Created By  : BSGABAP8                                             &*
*& Created On  : 2019.07.30                                           &*
*& Description : [CO] 버전별 리포트계정그룹 유지보수                  &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.07.30  BSGABAP8    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR0150 MESSAGE-ID ZCO01.

INCLUDE ZCOR0150T01.    "Top
INCLUDE ZCOR0150ALV.    "Alv
INCLUDE ZCOR0150SCR.    "Screen - Condition screen
INCLUDE ZCOR0150O01.    "Process Befor Output
INCLUDE ZCOR0150I01.    "Process After Input
INCLUDE ZCOR0150F01.    "Form

*---------------------------------------------------------------------*
*INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITAIL.
  PERFORM SET_INIT_HELP.

*---------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM SCR_USER_COMMAND_HELP.

*---------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_SCREEN.

*----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_VERSN.
  PERFORM F4_VERSN CHANGING PA_VERSN.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  "-- 데이터를 조회한다.
  PERFORM SELECTED_DATA_RTN.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF GV_EXIT IS INITIAL.
    " 100번 screen을 호출
    CALL SCREEN 0100.
  ENDIF.
