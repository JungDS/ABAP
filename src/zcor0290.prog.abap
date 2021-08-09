*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0290                                             &*
*& Title       : [CO] 예산가용금액 리포트_월별                        &*
*& Created By  : BSGABAP4                                             &*
*& Created On  : 2019.08.28                                           &*
*& Description : [CO] 예산가용금액 리포트_월별                        &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.08.28  BSGABAP4    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR0290 MESSAGE-ID ZCO01.

INCLUDE ZCOR0290T01.    " TOP-Decration.
INCLUDE ZCOR0290ALV.    " Class ALV OR Others
INCLUDE ZCOR0290SCR.    " Selection-Screen
INCLUDE ZCOR0290F01.    " Subroutine

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITAIL.
  PERFORM SCRFIELDS_FUNCTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM SET_SCREEN.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_PDGR.
*---------------------------------------------------------------------*
  PERFORM F4_PDGR CHANGING PA_PDGR.     "WBS 그룹

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_KAGRU.
*---------------------------------------------------------------------*
  PERFORM F4_KAGRU CHANGING PA_KAGRU.   "원가요소 그룹

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_KSGRU.
*---------------------------------------------------------------------*
  PERFORM F4_KSGRU CHANGING PA_KSGRU.   "코스트센터 그룹

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK B1.
*---------------------------------------------------------------------*
  PERFORM CHECK_B1.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK B2.
*---------------------------------------------------------------------*
  PERFORM CHECK_B2.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  PERFORM AUTHORITY_CHECK.
  PERFORM SET_RANGES_OBJNR.
  PERFORM DATA_GET.
  PERFORM SALV_CALL.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*
