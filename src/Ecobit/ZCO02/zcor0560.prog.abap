*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0560                                          &*
*& Title          : [CO] WBS 정산후 잔고점검                          &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.10.14                                        &*
*& Description    : [CO] WBS 정산후 잔고점검                          &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : WBS 정산후 잔고를 점검하는 프로그램으로           &*
*&                | 사업 WBS 와 설비 WBS 및 계획과 실적에 공통으로    &*
*&                | 사용가능 처리                                     &*
*& Biz. Rule      : 정산작업 후 잔고 점검용으로 사용                  &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.10.14  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0560 MESSAGE-ID ZCO01.


INCLUDE ZCOR0560T01.   " TOP-Decration
INCLUDE ZCOR0560ALV.   " Class ALV OR Others
INCLUDE ZCOR0560SCR.   " Selection-Screen
INCLUDE ZCOR0560F01.   " Subroutine
INCLUDE ZCOR0560PBO.   " Process Before Output
INCLUDE ZCOR0560PAI.   " Process After  Input


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
AT SELECTION-SCREEN ON BLOCK B03.
*---------------------------------------------------------------------*
  PERFORM CHECK_BLOCK_B03.

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
