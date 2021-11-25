*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0170                                             &*
*& Title       : [CO] WBS 도급내역                                    &*
*& Created By  : BSGABAP8                                             &*
*& Created On  : 2019.08.07                                           &*
*& Description : [CO] WBS 도급내역                                    &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.08.07  BSGABAP8    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR0170 MESSAGE-ID ZCO01.

INCLUDE ZCOR0170T01.    "Top
INCLUDE ZCOR0170ALV.    "Alv
INCLUDE ZCOR0170SCR.    "Screen - Condition screen
INCLUDE ZCOR0170O01.    "Process Befor Output
INCLUDE ZCOR0170I01.    "Process After Input
INCLUDE ZCOR0170F01.    "Form

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  PERFORM INITAIL.
  PERFORM SET_INIT_HELP.
  PERFORM SCRFIELDS_FUNCTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*---------------------------------------------------------------------*
  PERFORM SET_SCREEN.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND.


*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM SELECTED_DATA_RTN.
  CALL SCREEN 0100.
