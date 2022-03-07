*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0630                                          &*
*& Title          : [CO] 사업 WBS 속성정보 일괄변경                   &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.11.09                                        &*
*& Description    : [CO] 사업 WBS 속성정보 일괄변경                   &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 엑셀로 작성한 자료를 기준으로 사업 WBS 를         &*
*&                | 일괄변경하는 프로그램이다.                        &*
*& Biz. Rule      : 사업 프로젝트와 WBS 일괄변경용으로 사용           &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.11.09  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0630 MESSAGE-ID ZCO01.


INCLUDE ZCOR0630T01.   " TOP-Decration
INCLUDE ZCOR0630ALV.   " Class ALV OR Others
INCLUDE ZCOR0630SCR.   " Selection-Screen
INCLUDE ZCOR0630F01.   " Subroutine
INCLUDE ZCOR0630PBO.   " Process Before Output
INCLUDE ZCOR0630PAI.   " Process After  Input


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
  PERFORM SCR_USER_COMMAND. "양식 Download

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
*---------------------------------------------------------------------*
  PERFORM F4_FILE CHANGING P_FILE.

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
