*&---------------------------------------------------------------------*
*& Include          ZCOR0050O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  DATA FCODE TYPE TABLE OF SY-UCOMM.

  DATA LT_100   TYPE CHAR30.
  DATA LV_TITLE TYPE SYTITLE.

  CLEAR LV_TITLE.
  CLEAR LT_100.

  REFRESH FCODE.

  CASE GV_MODE.

    WHEN 'S'.
      APPEND '&SAV' TO FCODE.
      LV_TITLE = TEXT-T04.

      IF ZCOS0050-GWKEY IS NOT INITIAL.
        APPEND '&SUB' TO FCODE.
      ENDIF.

    WHEN 'E'.
      APPEND '&SUB' TO FCODE.
      LV_TITLE = TEXT-T03.

  ENDCASE.

  CASE ABAP_TRUE.
    WHEN PA_RAD1.
      LT_100 = TEXT-003.
    WHEN PA_RAD2.
      LT_100 = TEXT-004.
  ENDCASE.

  SET PF-STATUS 'PF_0100' EXCLUDING FCODE.
  SET TITLEBAR  'TT_0100' WITH LV_TITLE LT_100.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*& Module GET_LINECOUNT OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_LINECOUNT OUTPUT.

  DESCRIBE TABLE GT_OUTTAB LINES LINECOUNT.
  TC1-LINES    = LINECOUNT.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SHOW_LIST OUTPUT
*&---------------------------------------------------------------------*
MODULE SHOW_LIST OUTPUT.

  MOVE-CORRESPONDING GT_OUTTAB TO ZCOS0051.

  IF ZCOS0051-SAHSTL < 0.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'ZCOS0051-SAHSTL'.
        SCREEN-INTENSIFIED = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.

  IF ZCOS0051-RAHSTL < 0.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'ZCOS0051-RAHSTL'.
        SCREEN-INTENSIFIED = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.

  IF ZCOS0050-GWKEY IS NOT INITIAL.

    LOOP AT SCREEN.
      IF SCREEN-NAME CP 'ZCOS0051*'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module GET_LOOPLINES OUTPUT
*&---------------------------------------------------------------------*
MODULE GET_LOOPLINES OUTPUT.

  LOOPLINES = SY-LOOPC.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_CURSOR OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_CURSOR OUTPUT.
  SET CURSOR FIELD GV_FIELDNAME LINE GV_CURLINE OFFSET 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_SCREEN OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE ABAP_TRUE.

      WHEN PA_RAD1.

        IF SCREEN-GROUP1 = 'W1'.
          SCREEN-ACTIVE = 0.
        ENDIF.

      WHEN PA_RAD2.

        IF SCREEN-GROUP1 = 'C1'.
          SCREEN-ACTIVE = 0.
        ENDIF.

    ENDCASE.

    CASE GV_MODE.
      WHEN 'E'.
        IF SCREEN-GROUP1 = 'T1'.
          SCREEN-ACTIVE = 0.
        ENDIF.
    ENDCASE.

    IF ZCOS0050-GWKEY IS NOT INITIAL.
      SCREEN-INPUT = 0.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.
