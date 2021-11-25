*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0610                                          &*
*& Title          : [CO] 설비 WBS 계획 대비 실적 비교                 &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.10.27                                        &*
*& Description    : [CO] 설비 WBS 계획 대비 실적 비교                 &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 설비 WBS 계획 대비 실적을 비교하는 프로그램       &*
*& Biz. Rule      : 설비 WBS 계획 대비 실적 점검용으로 사용           &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.10.27  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0610 MESSAGE-ID ZCO01.


INCLUDE ZCOR0610T01.   " TOP-Decration
INCLUDE ZCOR0610ALV.   " Class ALV OR Others
INCLUDE ZCOR0610SCR.   " Selection-Screen
INCLUDE ZCOR0610F01.   " Subroutine
INCLUDE ZCOR0610PBO.   " Process Before Output
INCLUDE ZCOR0610PAI.   " Process After  Input


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

AT SELECTION-SCREEN ON P_VERSN.
  PERFORM CHECK_VERSN USING P_VERSN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VERSN.
  PERFORM F4_VERSN USING P_VERSN.

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
