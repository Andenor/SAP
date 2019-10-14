SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS: s_tkorr FOR <fs_log>-trkorr.
PARAMETERS: p_datef LIKE sy-datum DEFAULT gv_datefrom.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: p_doma TYPE tmsdomnam MODIF ID dis DEFAULT 'DOMAIN_DR0'.
PARAMETERS: p_laya TYPE devlayer MODIF ID dis DEFAULT 'SAP'.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
PARAMETERS: p_rfc TYPE flag,
            p_syst TYPE sysname DEFAULT 'PR0',
            p_dest TYPE rfcdest DEFAULT 'PR0',
            p_domad TYPE tmsdomnam DEFAULT 'DOMAIN_DR0',
            p_last TYPE dats MODIF ID dis.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.

  PERFORM init_last.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'DIS'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  INIT_LAST
*&---------------------------------------------------------------------*
FORM init_last.
  PERFORM start_date_rfc
              CHANGING
                 p_last.

ENDFORM.                    " INIT_LAST
