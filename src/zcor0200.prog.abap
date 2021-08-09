*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0200                                             &*
*& Title       : [CO] 결산이후 전기된 금액 확인                       &*
*& Created By  : BSGABAP8                                             &*
*& Created On  : 2019.08.13                                           &*
*& Description : [CO] 결산이후 전기된 금액 확인                       &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.08.14  BSGABAP8    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR0200 MESSAGE-ID ZCO01.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZCOR0200T01.    "Top
INCLUDE ZCOR0200ALV.    "Alv
INCLUDE ZCOR0200SCR.    "Screen - Condition screen
INCLUDE ZCOR0200O01.    "Process Befor Output
INCLUDE ZCOR0200I01.    "Process After Input
INCLUDE ZCOR0200F01.    "Form

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM SET_INIT_HELP.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND_HELP.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM SET_SCREEN.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM SELECTED_DATA_RTN.
  CALL SCREEN 0100.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
