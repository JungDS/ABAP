*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR101                                              &*
*& Title       : [CO] 프로젝트, WBS 생성 및 일괄 관리                 &*
*& Created By  : BSG(YJLIM)                                           &*
*& Created On  : 2020.03.11                                           &*
*& Description : [CO] 프로젝트, WBS 생성 및 일괄 관리 프로그램        &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2020.03.11  BSG(YJLIM)  INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR101 MESSAGE-ID ZCO01.


INCLUDE ZCOR101_TOP                             .  " Global Data
INCLUDE ZCOR101_SCR                             .  " Selection Screen
INCLUDE ZCOR101_CLS                             .  " Selection Screen
INCLUDE ZCOR101_O01                             .  " PBO-Modules
INCLUDE ZCOR101_I01                             .  " PAI-Modules
INCLUDE ZCOR101_F01                             .  " FORM-Routines


*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  PERFORM INIT_VALUE.


*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
  PERFORM MODIFY_SCREEN.


*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM DOWNLOAD_FORM.
  ENDCASE.


*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILENM.
*----------------------------------------------------------------------*
  PERFORM GET_FILENM  CHANGING P_FILENM.


*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  CASE 'X'.
    WHEN RB_CRT.  "생성
      PERFORM GET_EXCEL_DATA.
      PERFORM MODIFY_DATA.

    WHEN RB_DISP. "조회/변경
      PERFORM GET_DATA.
      PERFORM MODIFY_DATA.
  ENDCASE.

  IF GT_OUTTAB[] IS NOT INITIAL.
    CALL SCREEN 100.

  ELSE.
    "조회조건에 만족하는 데이터가 없습니다.
    MESSAGE S004 DISPLAY LIKE 'E'.
  ENDIF.
