*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0590                                          &*
*& Title          : [CO] 판관비 배부사이클 조회                       &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.10.19                                        &*
*& Description    : [CO] 판관비 배부사이클 조회                       &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 코스트센터의 판관비를 PA특성으로 배부하는         &*
*&                | 배부사이클을 조회하는 프로그램으로                &*
*&                | 계획과 실적에 공통으로 사용가능 처리              &*
*& Biz. Rule      : 배부작업 후 잔고 점검용으로 사용                  &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.10.19  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0590 MESSAGE-ID ZCO01.


INCLUDE ZCOR0590T01.   " TOP-Decration
INCLUDE ZCOR0590ALV.   " Class ALV OR Others
INCLUDE ZCOR0590SCR.   " Selection-Screen
INCLUDE ZCOR0590F01.   " Subroutine
INCLUDE ZCOR0590PBO.   " Process Before Output
INCLUDE ZCOR0590PAI.   " Process After  Input


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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_CYCLE-LOW.
*---------------------------------------------------------------------*
  PERFORM F4_CYCLE USING S_CYCLE-LOW.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_CYCLE-HIGH.
*---------------------------------------------------------------------*
  PERFORM F4_CYCLE USING S_CYCLE-HIGH.

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
