*&--------------------------------------------------------------------&*
*& PROGRAM ID     : ZCOR0640                                          &*
*& Title          : [CO] CO-PA 실적값 업로드                          &*
*& Created By     : MDP_06                                            &*
*& Created On     : 2021.11.08                                        &*
*& Description    : [CO] CO-PA 실적값 업로드                          &*
*& Biz. Owner     : 기획팀                                            &*
*& Pre-requisites : 엑셀로 작성한 자료를 기준으로                     &*
*&                | CO-PA 실적값을 값필드에 등록하는 작업             &*
*& Biz. Rule      : 값 필드를 지정하여 다양한 목적으로 사용           &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2021.11.08  MDP_06      INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0640 MESSAGE-ID ZCO01.


INCLUDE ZCOR0640T01.   " TOP-Decration
INCLUDE ZCOR0640ALV.   " Class ALV OR Others
INCLUDE ZCOR0640SCR.   " Selection-Screen
INCLUDE ZCOR0640F01.   " Subroutine
INCLUDE ZCOR0640PBO.   " Process Before Output
INCLUDE ZCOR0640PAI.   " Process After  Input


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
