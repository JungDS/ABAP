*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0660                                          &*
*& Title          : [CO] 관리결산 수행메뉴                            &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2022.02.14                                        &*
*& Description    : [CO] 관리결산 수행메뉴                            &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 관리결산 수행을 효율적으로 하기 위하여            &*
*&                  관리결산 수행 절차를 세트로 관리하고              &*
*&                  프로그램에서조회하여 해당 t-code 수행으로 연결    &*
*& Biz. Rule      : GS01(GS02) 에서 관리회계 수행 절차를 세트로 관리  &*
*&                  세트: ZCO_C1                                      &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2022.02.14  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0660 MESSAGE-ID ZCO01.


INCLUDE ZCOR0660T01.   " TOP-Decration
INCLUDE ZCOR0660ALV.   " Class ALV OR Others
INCLUDE ZCOR0660SCR.   " Selection-Screen
INCLUDE ZCOR0660F01.   " Subroutine
INCLUDE ZCOR0660PBO.   " Process Before Output
INCLUDE ZCOR0660PAI.   " Process After  Input


*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITIALIZATION.
  PERFORM SCRFIELDS_FUNCTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
*  PERFORM MODIFY_SCR.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND.

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
