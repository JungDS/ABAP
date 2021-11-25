*&---------------------------------------------------------------------*
*& Include          ZCOR0480O01
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: LT_EXCLUD TYPE TABLE OF SY-UCOMM.

  CLEAR : LT_EXCLUD[].
  SET PF-STATUS '100' EXCLUDING LT_EXCLUD .
  SET TITLEBAR  '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&------------------------------------------------------

*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO OUTPUT.

  DATA L_SIZE TYPE I.
  L_SIZE = SY-SCOLS * '10'.


  IF G_DOCKING_CONTAINER IS INITIAL.
*
    CREATE OBJECT G_DOCKING_CONTAINER
      EXPORTING
        REPID     = G_REPID_C
        DYNNR     = '0100'
        EXTENSION = L_SIZE
      EXCEPTIONS
        OTHERS    = 1.
*
    IF I_SPLIT EQ SPACE.
      CREATE OBJECT G_SPLIT_CONTAINER
        EXPORTING
          PARENT  = G_DOCKING_CONTAINER
          ROWS    = 1
          COLUMNS = 2.
    ELSE.
      CREATE OBJECT G_SPLIT_CONTAINER
        EXPORTING
          PARENT  = G_DOCKING_CONTAINER
          ROWS    = 1
          COLUMNS = 2.
    ENDIF.
*   ZCOT1260
    PERFORM SET_CONTAINER_ITEM1.
*   SD  MM 펑션의 수량
    PERFORM SET_CONTAINER_ITEM2.

  ENDIF.

ENDMODULE.                 " PBO  OUTPUT
