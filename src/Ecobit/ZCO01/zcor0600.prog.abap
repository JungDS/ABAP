*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0600                                          &*
*& Title          : [CO] 설비 WBS 등록 현황                           &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.10.22                                        &*
*& Description    : [CO] 설비 WBS 등록 현황                           &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 설비 WBS 등록현황을 조회하는 프로그램으로         &*
*&                | WBS 의 속성정보와 사용자필드 정보를 조회하고,     &*
*&                | 정산규칙 상세조회를 선택조건으로 반영 처리        &*
*& Biz. Rule      : 설비 WBS 등록 점검용으로 사용                     &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.10.22  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0600 MESSAGE-ID ZCO01.


INCLUDE ZCOR0600T01.   " TOP-Decration
INCLUDE ZCOR0600ALV.   " Class ALV OR Others
INCLUDE ZCOR0600SCR.   " Selection-Screen
INCLUDE ZCOR0600F01.   " Subroutine
INCLUDE ZCOR0600PBO.   " Process Before Output
INCLUDE ZCOR0600PAI.   " Process After  Input


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
