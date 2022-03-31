*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0670                                          &*
*& Title          : [CO] 사업계획 수행메뉴                            &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2022.03.04                                        &*
*& Description    : [CO] 사업계획 수행메뉴                            &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 사업계획손익 수행을 효율적으로 하기 위하여        &*
*&                  사업계획손익 수행 절차를 세트로 관리하고          &*
*&                  프로그램에서조회하여 해당 t-code 수행으로 연결    &*
*& Biz. Rule      : GS01(GS02) 에서 관리회계 수행 절차를 세트로 관리  &*
*&                  세트: ZCO_P1                                      &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2022.03.04  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0670 MESSAGE-ID ZCO01.


INCLUDE ZCOR0670T01.   " TOP-Decration
INCLUDE ZCOR0670ALV.   " Class ALV OR Others
INCLUDE ZCOR0670SCR.   " Selection-Screen
INCLUDE ZCOR0670F01.   " Subroutine
INCLUDE ZCOR0670PBO.   " Process Before Output
INCLUDE ZCOR0670PAI.   " Process After  Input


*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITIALIZATION.
  PERFORM SCRFIELDS_FUNCTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
*  PERFORM MODIFY_SCR.
  PERFORM SET_SCREEN_TEXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VERSI.
*---------------------------------------------------------------------*
  PERFORM F4_VERSI USING P_VERSI.

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
