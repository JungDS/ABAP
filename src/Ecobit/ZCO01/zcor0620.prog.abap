*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0620                                          &*
*& Title          : [CO] 설비 WBS 엑셀 업로드                         &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.11.01                                        &*
*& Description    : [CO] 설비 WBS 엑셀 업로드                         &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 엑셀로 작성한 자료를 기준으로 설비 WBS 를 생성하는&*
*&                | 프로그램으로 프로젝트/WBS를 동시에 등록한다.      &*
*& Biz. Rule      : 설비 프로젝트와 WBS 일괄등록용으로 사용           &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.11.01  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0620 MESSAGE-ID ZCO01.


INCLUDE ZCOR0620T01.   " TOP-Decration
INCLUDE ZCOR0620ALV.   " Class ALV OR Others
INCLUDE ZCOR0620SCR.   " Selection-Screen
INCLUDE ZCOR0620F01.   " Subroutine
INCLUDE ZCOR0620PBO.   " Process Before Output
INCLUDE ZCOR0620PAI.   " Process After  Input


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
