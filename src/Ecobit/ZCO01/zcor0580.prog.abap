*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0550                                             &*
*& Title       : [CO] 사업 WBS 계획 정산규칙 등록                     &*
*& Created By  : MDP_06                                               &*
*& Created On  : 2021.10.18                                           &*
*& Description : [CO] 사업 WBS 계획 정산규칙 등록                     &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.10.18  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0580 MESSAGE-ID ZCO01.


INCLUDE ZCOR0580T01.   " TOP-Decration
INCLUDE ZCOR0580ALV.   " Class ALV OR Others
INCLUDE ZCOR0580SCR.   " Selection-Screen
INCLUDE ZCOR0580F01.   " Subroutine
INCLUDE ZCOR0580PBO.   " Process Before Output
INCLUDE ZCOR0580PAI.   " Process After  Input


*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITIALIZATION.
  PERFORM SCRFIELDS_FUNCTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM MODIFY_SCR.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND. "양식 Download

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON P_VERSN.
*---------------------------------------------------------------------*
  PERFORM CHECK_VERSN USING P_VERSN.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  PERFORM SELECTED_DATA_RTN.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*
  IF GT_DISPLAY[] IS INITIAL.
    " 조회조건에 만족하는 데이터가 없습니다.
    MESSAGE S004 DISPLAY LIKE GC_W.
  ELSE.
    CALL SCREEN 0100.
  ENDIF.
