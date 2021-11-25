*&---------------------------------------------------------------------*
*& Include          ZCOR0360SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN: BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-001 FOR FIELD PA_KOKRS.
SELECTION-SCREEN POSITION 33.
PARAMETERS: PA_KOKRS TYPE KOKRS MEMORY ID CAC OBLIGATORY
                                MATCHCODE OBJECT CSH_TKA01
                                MODIF ID MG1 DEFAULT '1000'.
SELECTION-SCREEN COMMENT 45(20) PA_KTXT MODIF ID MG1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-002 FOR FIELD PA_VERSN.
SELECTION-SCREEN POSITION 33.
PARAMETERS  PA_VERSN TYPE VERSN OBLIGATORY MATCHCODE
                                 OBJECT ZH_TKA09
                                 DEFAULT 'P0'.
SELECTION-SCREEN COMMENT 45(20) PA_VTXT.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS SO_PRCTR FOR PRPS-PRCTR.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-003 FOR FIELD PA_GJAHR.
SELECTION-SCREEN POSITION 33.
PARAMETERS  PA_GJAHR TYPE GJAHR DEFAULT SY-DATUM(4) OBLIGATORY
                                MODIF ID G1.

SELECTION-SCREEN COMMENT 40(6) TEXT-004 FOR FIELD PA_PERBL.
SELECTION-SCREEN POSITION 47.
PARAMETERS : PA_PERBL TYPE PERBL  OBLIGATORY.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK BL1.


SELECTION-SCREEN: BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-T03.

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS  PA_RAD2 RADIOBUTTON GROUP RD1  DEFAULT 'X' .

SELECTION-SCREEN COMMENT 2(12) TEXT-005 FOR FIELD PA_RAD2.

SELECTION-SCREEN POSITION 20.

PARAMETERS: PA_RAD1 RADIOBUTTON GROUP RD1.
SELECTION-SCREEN COMMENT 23(12) TEXT-006 FOR FIELD PA_RAD1.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK BL2.

SELECTION-SCREEN: BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-T04.

PARAMETERS PA_CHECK AS CHECKBOX.

SELECTION-SCREEN: END OF BLOCK BL3.
