*&---------------------------------------------------------------------*
*& Include          ZCOR0250SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN: BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-001 FOR FIELD PA_KOKRS.
SELECTION-SCREEN POSITION 33.
PARAMETERS: PA_KOKRS TYPE KOKRS MEMORY ID CAC OBLIGATORY
                                MATCHCODE OBJECT CSH_TKA01.
SELECTION-SCREEN COMMENT 45(20) PA_KTXT MODIF ID MG1.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS SO_KSTAR FOR CSKA-KSTAR.

SELECTION-SCREEN: END OF BLOCK BL1.
