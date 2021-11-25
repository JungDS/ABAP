*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0460                                             &*
*& Title       : [CO] 매출환입 매출원가 차감 관리                     &*
*& Created By  : BSGSM_FCM                                            &*
*& Created On  : 2020.09.18                                           &*
*& Description :                                                      &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2020.09.18 BSGSM_FCM     INITIAL RELEASE
*----------------------------------------------------------------------*

REPORT ZCOR0460  MESSAGE-ID ZCO01.
INCLUDE ZCOR0460TOP.
INCLUDE ZCOR0460TO2.
INCLUDE ZCOR0460ALV.
INCLUDE ZCOR0460SCR.
INCLUDE ZCOR0460SALV.
INCLUDE ZCOR0460F01.
INCLUDE ZCOR0460O01.
INCLUDE ZCOR0460I01.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INIT_DATA.

* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_MONTH.
*---------------------------------------------------------------------*
  PERFORM F4_P_MONTH.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK BL1.
*---------------------------------------------------------------------*
  PERFORM CHECK_BL1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND.



*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM CHECK_AUTH_COND.
  CLEAR GV_EXIT_ZCOR0450.
  PERFORM CHK_ZCOR0450_POST.

  CHECK GV_EXIT IS INITIAL.

  PERFORM SELECTED_DATA_RTN.
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*

END-OF-SELECTION.
  CASE GV_EXIT_ZCOR0450.
    WHEN 'X'.
      MESSAGE I000  WITH TEXT-E98 DISPLAY LIKE gc_e.

    WHEN OTHERS.
      CALL SCREEN 0100.
  ENDCASE.
