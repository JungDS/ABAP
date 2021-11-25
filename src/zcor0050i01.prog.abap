*&---------------------------------------------------------------------*
*& Include          ZCOR0050I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.


  CLEAR: gv_exit, gv_answer, gv_change, gv_valid.

*-- OK_CODE ACTIONS.
  CASE ok_code.

    WHEN '&CNC'.

      PERFORM check_change CHANGING gv_valid.

      IF gv_valid IS NOT INITIAL.

        PERFORM popup_to_confirm USING TEXT-pt2
                                       TEXT-qt3.
        CASE gv_answer.

          WHEN '1'.
            LEAVE TO SCREEN 0.
          WHEN OTHERS.
            EXIT.

        ENDCASE.

      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN '&EXT'.

      PERFORM check_change CHANGING gv_valid.

      IF gv_valid IS NOT INITIAL.

        PERFORM popup_to_confirm USING TEXT-pt2
                                       TEXT-qt3.

        CASE gv_answer.

          WHEN '1'.
            LEAVE PROGRAM.
          WHEN OTHERS.
            EXIT.

        ENDCASE.

      ELSE.
        LEAVE PROGRAM.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR: save_ok, gv_exit, gv_answer, gv_change,
         gv_valid.


**ADD BSGSM_FCM
*-- ALV CHECKED CHANGE DATA
  CALL METHOD gr_grid1->check_changed_data( ).
  CALL METHOD gr_grid1->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows[].

**ADD BSGSM_FCM  20210823

  "-- move ok code.
  save_ok = ok_code.   CLEAR: ok_code.


  CASE save_ok.

    WHEN '&BCK'.

*      PERFORM CHECK_CHANGE CHANGING GV_VALID.
*
*      IF GV_VALID IS NOT INITIAL.
*
*        PERFORM POPUP_TO_CONFIRM USING TEXT-PT4
*                                       TEXT-QT4.
*        CASE GV_ANSWER.
*
*          WHEN '1'.
*            PERFORM SAVE_DATA_RTN.
*            LEAVE TO SCREEN 0.
*
*          WHEN '2'.
*            LEAVE TO SCREEN 0.
*
*          WHEN OTHERS.
*            EXIT.
*
*        ENDCASE.
*
*      ELSE.
*        LEAVE TO SCREEN 0.
*      ENDIF.

      PERFORM popup_to_confirm USING TEXT-pt4
                                         TEXT-qt4.

      CASE gv_answer.
        WHEN  '1' OR '2'.
          LEAVE TO SCREEN 0.
        WHEN OTHERS.
          EXIT.
      ENDCASE.

    WHEN '&HELP'.
      PERFORM call_popup_help(zcar9000)
               USING sy-repid sy-dynnr sy-langu ''.

    WHEN '&SAV'.
*      PERFORM checked_saved_data.
**
**      CHECK gv_exit IS INITIAL.
**      PERFORM popup_to_confirm USING TEXT-pt1
**                                     TEXT-qt1.
**
**      CHECK gv_answer = '1'.
**      PERFORM save_data_rtn.


      PERFORM SAVE_select_data_NEW.
      PERFORM refresh_grid_0100.







** MODI  BY  BGSM_FCM   20210824.

    WHEN '&EDIT'.

      PERFORM  edit_select_data.
      PERFORM refresh_grid_0100.


    WHEN '&REFR'.
      PERFORM selected_data_rtn.
      PERFORM refresh_grid_0100.


    WHEN '&ADD'.
      PERFORM add_data_rtn.
      PERFORM refresh_grid_0100.

    WHEN '&DELE'.
      PERFORM dele_select_data.
      PERFORM refresh_grid_0100.



**END BY BSGSM_FCM..



    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
