*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0650                                          &*
*& Title          : [CO] 회사별 손익추이 레포트(사별권한)             &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2022.02.03                                        &*
*& Description    : [CO] 회사별 손익추이 레포트(사별권한)             &*
*& Biz. Owner     :                                                   &*
*& Pre-requisites : KE35 Report Writer 에 의해 생성된                 &*
*&                | PA-11-D, PA-11-S Report 만을 대상으로 한다.       &*
*&                | 해당 레포트에는 회사코드를 잠금처리하기 위한      &*
*&                | Variant [LOCK_COMPANY] 가 반드시 존재해야 한다.   &*
*& Biz. Rule      : 사별로 PA Report 를 권한관리할 수 있다.           &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2022.02.03  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0650 MESSAGE-ID ZCO01.


INCLUDE ZCOR0650T01.   " TOP-Decration
INCLUDE ZCOR0650SCR.   " Selection-Screen
INCLUDE ZCOR0650F01.   " Subroutine


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
AT SELECTION-SCREEN ON BLOCK BL1.
*---------------------------------------------------------------------*
  PERFORM CHECK_BLOCK_BL1.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND. "User Command: 도움말
  PERFORM CHECK_INPUT_DATA.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  PERFORM AUTHORITY_CHECK.
  PERFORM EXECUTE_REPORT.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*
