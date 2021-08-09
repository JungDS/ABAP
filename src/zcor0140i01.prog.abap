*&---------------------------------------------------------------------*
*& Include          ZCOR0140I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

*-- OK_CODE ACTIONS.
  CASE OK_CODE.
    WHEN '&CNC'.
      LEAVE TO SCREEN 0.
    WHEN '&EXT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER.

  "-- move ok code.
  SAVE_OK = OK_CODE.   CLEAR: OK_CODE.

*-- ALV CHECKED CHANGE DATA
  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).

*-- SAVE_OK ACTIONS.
  CASE SAVE_OK.
    WHEN '&BCK'.
      LEAVE TO SCREEN 0.

    WHEN '&SAV'.

      "-- checked saved data.
      PERFORM CHECKED_SAVED_DATA.

      CHECK GV_EXIT IS INITIAL.

      "-- Popup to confirm
      PERFORM POPUP_TO_CONFIRM.

      CHECK GV_ANSWER EQ '1'.

      "-- Save data
      PERFORM SAVE_DATA_RTN.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHENGE_ZBSUP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHENGE_ZBSUP INPUT.

  DATA: LV_INDEX     TYPE I,
        LV_MONTH     TYPE N LENGTH 2,
        LV_FIELDNAME TYPE FIELDNAME.

  FIELD-SYMBOLS: <FS_WKF>   TYPE ANY,
                 <FS_OUT>   TYPE ANY,
                 <FS_MOD>   TYPE ANY,
                 <FS_TOTAL> TYPE ANY.

  LOOP AT GT_DISPLAY.

    IF ( GT_DISPLAY-INDEX MOD 2 ) = 1.

      LV_MONTH = '01'.

      DO 12 TIMES.

        LV_FIELDNAME = 'GT_DISPLAY-WKF' && LV_MONTH.
        ASSIGN (LV_FIELDNAME) TO <FS_WKF>.

        READ TABLE GT_DISPLAY WITH KEY INDEX = ( GT_DISPLAY-INDEX + 1 ) ASSIGNING FIELD-SYMBOL(<LS_DISPLAY>).

        LV_FIELDNAME = '<LS_DISPLAY>-WKF' && LV_MONTH.
        ASSIGN (LV_FIELDNAME) TO <FS_MOD>.

        <FS_MOD> = <FS_WKF> * ( GV_ZBSUP * 100 ).

        IF LV_MONTH = '01'.
          CLEAR <LS_DISPLAY>-TOTAL.
        ENDIF.

        ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <LS_DISPLAY> TO <FS_TOTAL>.
        <FS_TOTAL> = <FS_TOTAL> + <FS_MOD>.

        ADD 1 TO LV_MONTH.

      ENDDO.

    ENDIF.

    GT_DISPLAY-ZBSUP = GV_ZBSUP.
    MODIFY GT_DISPLAY TRANSPORTING ZBSUP.

  ENDLOOP.


ENDMODULE.
