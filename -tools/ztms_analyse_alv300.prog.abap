*&---------------------------------------------------------------------*
*&  Include           ZACNTRANSP_ALV300
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver300 DEFINITION DEFERRED.

DATA: g_grid300             TYPE REF TO cl_gui_alv_grid,
      g_container300        TYPE scrfname VALUE 'CONT_0300',
      g_container_300 TYPE REF TO cl_gui_custom_container,
      g_event_receiver300   TYPE REF TO lcl_event_receiver300.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver300 DEFINITION.

  PUBLIC SECTION.
    METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    handle_after_user_command
        FOR EVENT after_user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    handle_data_changed_finished
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
          IMPORTING e_modified ,

    handle_after_refresh
        FOR EVENT after_refresh OF cl_gui_alv_grid,

    handle_data_changed
       FOR EVENT data_changed OF cl_gui_alv_grid
           IMPORTING er_data_changed,

    handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
          IMPORTING e_row
                    e_column
                    es_row_no.
  PRIVATE SECTION.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.

* Methods to modularize event handler method HANDLE_DATA_CHANGED:
    METHODS: check_value
     IMPORTING
        ps_regul TYPE lvc_s_modi
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.


ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver300 IMPLEMENTATION.
  METHOD handle_after_refresh.

  ENDMETHOD.                    "handle_after_refresh
  METHOD handle_data_changed_finished .
*    CALL METHOD g_grid->refresh_table_display.
*    break mtresorier.
  ENDMETHOD .                    "handle_data_changed_finished

  METHOD handle_toolbar.
* § 2.In event handler method for event TOOLBAR: Append own functions
*   by using event parameter E_OBJECT.
    DATA: ls_toolbar  TYPE stb_button.
*....................................................................
* E_OBJECT of event TOOLBAR is of type REF TO CL_ALV_EVENT_TOOLBAR_SET.
* This class has got one attribute, namly MT_TOOLBAR, which
* is a table of type TTB_BUTTON. One line of this table is
* defined by the Structure STB_BUTTON (see data deklaration above).
*

* A remark to the flag E_INTERACTIVE:
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*         'e_interactive' is set, if this event is raised due to
*         the call of 'set_toolbar_interactive' by the user.
*         You can distinguish this way if the event was raised
*         by yourself or by ALV
*         (e.g. in method 'refresh_table_display').
*         An application of this feature is still unknown... :-)

* append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ADD' TO ls_toolbar-function.
    MOVE icon_insert_row TO ls_toolbar-icon.
    MOVE 'Ajouter'(121) TO ls_toolbar-quickinfo.
    MOVE 'Ajouter'(121) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'DELETE' TO ls_toolbar-function.
    MOVE icon_delete_row TO ls_toolbar-icon.
    MOVE 'Supprimer'(122) TO ls_toolbar-quickinfo.
    MOVE 'Supprimer'(122) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'CHANGE' TO ls_toolbar-function.
    MOVE icon_change_text TO ls_toolbar-icon.
    MOVE 'Modifier'(123) TO ls_toolbar-quickinfo.
    MOVE 'Modifier'(123) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar
  METHOD handle_after_user_command.
*    PERFORM colorize CHANGING gt_alv.
  ENDMETHOD.                    "handle_after_user_command
*-------------------------------------------------------------------
  METHOD handle_user_command.
* § 3.In event handler method for event USER_COMMAND: Query your
*   function codes defined in step 2 and react accordingly.

*    DATA: lt_rows TYPE lvc_t_row.
*    DATA: ls_outtab LIKE LINE OF gt_outtab,
*          lt_regul TYPE zsdcgst001t,
*          ls_regul LIKE LINE OF lt_regul.
    DATA: lt_rowid TYPE lvc_t_row,
          ls_rowid LIKE LINE OF lt_rowid.
    DATA: lt_trkorr TYPE cts_trkorrs.

    DATA: lt_sel_idx TYPE lvc_t_row,
          ls_sel LIKE LINE OF lt_sel_idx.

    DATA: lt_ot_in TYPE zacn_t_trkorr,
          lt_ot_out TYPE zacn_t_trkorr,
          ls_trkorr LIKE LINE OF lt_ot_in,

          ls_comment LIKE LINE OF gt_comments,

          lt_filter TYPE lvc_t_filt,
          ls_filter LIKE LINE OF lt_filter.

    CALL METHOD g_grid300->get_selected_rows
      IMPORTING
        et_index_rows = lt_sel_idx.

*           et_row_no     =                                                                                        lt_sel_num

    READ TABLE lt_sel_idx INTO ls_sel INDEX 1.

    READ TABLE gt_comments INTO ls_comment INDEX ls_sel-index.
    CASE e_ucomm.
      WHEN 'ADD'.

        gv_textmode = gc_textmode_new.
        CALL SCREEN 0400 STARTING AT 5 5.

        CLEAR lt_trkorr.
        IF ls_comment-trkorr IS INITIAL.
          ls_comment-trkorr = gv_trkorr.
        ENDIF.
        APPEND ls_comment-trkorr TO lt_trkorr.

        IF gv_text IS NOT INITIAL.
          PERFORM add_text USING lt_trkorr gv_text.
        ENDIF.

      WHEN 'DELETE'.
        CHECK lt_sel_idx IS NOT INITIAL.

        PERFORM delete_text USING ls_comment-trkorr
                                  ls_comment-guid.

      WHEN 'CHANGE'.
        CHECK lt_sel_idx IS NOT INITIAL.

        gv_textmode = gc_textmode_edit.
        gv_text = ls_comment-text.
        CALL SCREEN 0400 STARTING AT 5 5.

        CLEAR lt_trkorr.
        APPEND ls_comment-trkorr TO lt_trkorr.

        IF gv_text IS NOT INITIAL.
          PERFORM change_text USING ls_comment-trkorr
                                    ls_comment-guid
                                    gv_text.
        ENDIF.

    ENDCASE.
    CALL METHOD g_grid300->refresh_table_display.
  ENDMETHOD.                           "handle_user_command
*-----------------------------------------------------------------
  METHOD handle_data_changed.

  ENDMETHOD.                    "handle_data_changed

  METHOD check_value.

  ENDMETHOD.                    "check

  METHOD handle_double_click.
    DATA: ls_comment LIKE LINE OF gt_comments,
          lt_trkorr TYPE cts_trkorrs,
          ls_trkorr LIKE LINE OF lt_trkorr,
          lv_syst TYPE ty_list_sys-struct_txt,
          ls_list_sys LIKE LINE OF gt_list_sys.

    READ TABLE gt_comments INTO ls_comment INDEX e_row.
    gv_text = ls_comment-text.
    PERFORM show_text USING ls_comment-guid.

  ENDMETHOD.                    "handle_DOUBLE_CLICK
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_0300
*&---------------------------------------------------------------------*
FORM init_alv_0300  CHANGING ot_comments TYPE tyt_alv_comments.

  DATA: lt_exclude  TYPE ui_functions.
  DATA: ls_layout   TYPE lvc_s_layo.
  DATA: lt_fieldcat TYPE lvc_t_fcat.
  DATA: ls_variant TYPE disvariant.
  CREATE OBJECT g_container_300
    EXPORTING
      container_name = g_container300.
  CREATE OBJECT g_grid300
    EXPORTING
      i_parent = g_container_300.

  PERFORM build_fieldcat300 CHANGING ot_comments
                                  lt_fieldcat.

*§2.Optionally restrict generic functions to 'change only'.
*   (The user shall not be able to add new lines).
  PERFORM exclude_tb_functions CHANGING lt_exclude.

*  ls_layout-cwidth_opt = 'X'.
*  ls_layout-stylefname = 'CELLTAB'.
*  ls_layout-no_totline = 'X'.
*  ls_layout-ctab_fname = 'T_COLOR'.
*  ls_layout-edit       = 'X'.

*  PERFORM colorize CHANGING it_alv.

  ls_variant-report = sy-repid.
  ls_variant-handle = 2.

  CALL METHOD g_grid300->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layout
      it_toolbar_excluding = lt_exclude
      i_save               = 'A'
      is_variant           = ls_variant
    CHANGING
      it_fieldcatalog      = lt_fieldcat
*     it_sort              = gt_sort_lvc
      it_outtab            = ot_comments.

*§3.Optionally register ENTER to raise event DATA_CHANGED.
*   (Per default the user may check data by using the check icon).
  CALL METHOD g_grid300->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CREATE OBJECT g_event_receiver300.
  SET HANDLER g_event_receiver300->handle_user_command FOR g_grid300.
  SET HANDLER g_event_receiver300->handle_after_user_command FOR g_grid300.
  SET HANDLER g_event_receiver300->handle_data_changed FOR g_grid300.
  SET HANDLER g_event_receiver300->handle_toolbar FOR g_grid300.
  SET HANDLER g_event_receiver300->handle_data_changed_finished FOR g_grid300 .
  SET HANDLER g_event_receiver300->handle_after_refresh FOR g_grid300 .
  SET HANDLER g_event_receiver300->handle_double_click FOR g_grid300.
  CALL METHOD g_grid300->set_toolbar_interactive.

ENDFORM.                    " INIT_ALV_0300
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ALV  text
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat300  CHANGING ot_comments TYPE tyt_alv_comments
                                 ot_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fieldcat LIKE LINE OF ot_fieldcat.
  DATA: ls_list_sys LIKE LINE OF gt_list_sys.
  DATA: lt_celltab  TYPE lvc_t_styl,
        ls_celltab  LIKE LINE OF lt_celltab.

  FIELD-SYMBOLS : <fcat> TYPE lvc_s_fcat,
                  <falv> LIKE LINE OF ot_comments.

*TRKORR
*ERDAT
*ERNAM

  ls_fieldcat-col_pos   = 1.
  ls_fieldcat-fieldname = 'TRKORR'.
  ls_fieldcat-scrtext_l = 'Ordre'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-key = 'X'.
  ls_fieldcat-rollname  = ''.

  APPEND ls_fieldcat TO ot_fieldcat.

  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'ERDAT'.
  ls_fieldcat-scrtext_l = 'Date de création'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-key = ' '.
  ls_fieldcat-rollname  = 'ERDAT'.

  APPEND ls_fieldcat TO ot_fieldcat.

  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'ERNAM'.
  ls_fieldcat-scrtext_l = 'Utilisateur'.
  ls_fieldcat-key = ' '.
  ls_fieldcat-rollname  = 'ERNAM'.

  APPEND ls_fieldcat TO ot_fieldcat.

  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'TEXT'.
  ls_fieldcat-scrtext_l = 'Texte'.
  ls_fieldcat-scrtext_s = 'Texte'.
  ls_fieldcat-scrtext_m = 'Texte'.
  ls_fieldcat-key = ' '.
  ls_fieldcat-outputlen = 50.
  ls_fieldcat-rollname  = 'TEXT'.

  APPEND ls_fieldcat TO ot_fieldcat.


ENDFORM.                    " BUILD_FIELDCAT300

*----------------------------------------------------------------------*
*       CLASS user_outbuf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS user_outbuf DEFINITION.

  PUBLIC SECTION.
    INTERFACES if_abap_gzip_text_handler.

    CLASS-DATA : v_xstring TYPE xstring,
                 v_len TYPE ty_trlog-len.

ENDCLASS.                    "user_outbuf DEFINITION

*----------------------------------------------------------------------*
*       CLASS user_outbuf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS user_outbuf IMPLEMENTATION.


  METHOD if_abap_gzip_text_handler~use_out_buf.
    v_len = out_buf_len.
    v_xstring = out_buf.
  ENDMETHOD.                    "if_abap_gzip_text_handler~use_out_buf
ENDCLASS.                    "user_outbuf IMPLEMENTATION

DATA: uref       TYPE REF TO user_outbuf.
DATA: csref      TYPE REF TO cl_abap_gzip_text_stream.
*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
FORM add_text  USING it_trkorr TYPE cts_trkorrs
                     iv_text TYPE string.

  DATA: ls_comm TYPE ty_trlog.
  DATA: ls_comment LIKE LINE OF gt_comments.
  DATA: lt_comm TYPE tyt_trlog.
  DATA: ls_ot LIKE LINE OF it_trkorr.
  DATA: lv_guid TYPE guid.
  DATA: lv_raw TYPE ty_trlog-lraw.
  DATA: cl_guid TYPE REF TO cl_system_uuid.
  DATA: lv_output TYPE xstring.

  CREATE OBJECT cl_guid.

*TRY.
  CALL METHOD cl_guid->if_system_uuid~create_uuid_x16
    RECEIVING
      uuid = lv_guid.
* CATCH cx_uuid_error .
*ENDTRY.

  CREATE OBJECT uref.

  CREATE OBJECT csref
    EXPORTING
      conversion     = 'DEFAULT'
      output_handler = uref.

  TRY.
      CALL METHOD csref->set_out_buf
        IMPORTING
          out_buf = lv_raw.
    CATCH cx_parameter_invalid_range .
      IF 1 = 2.
      ENDIF.
  ENDTRY.

  CALL METHOD csref->compress_text_stream_end
    EXPORTING
      text_in     = iv_text   " some more text
      text_in_len = -1.

  LOOP AT it_trkorr INTO ls_ot.

    ls_comm-trkorr = ls_ot.
    ls_comm-guid = lv_guid.
    ls_comm-erdat = sy-datum.
    ls_comm-ernam = sy-uname.
    ls_comm-len = uref->v_len.
    ls_comm-lraw = uref->v_xstring.

    APPEND ls_comm TO lt_comm.

  ENDLOOP.

  IF lt_comm IS NOT INITIAL.
    INSERT zacntrlog FROM TABLE lt_comm .
  ENDIF.

  LOOP AT lt_comm INTO ls_comm.
    CLEAR ls_comment.
    ls_comment-text = iv_text.
    MOVE-CORRESPONDING ls_comm TO ls_comment.
    APPEND ls_comment TO gt_comments.
    APPEND ls_comm TO gt_trlog.
  ENDLOOP.

  FREE: cl_guid, uref, csref.

ENDFORM.                    " ADD_TEXT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TEXT
*&---------------------------------------------------------------------*
FORM change_text  USING iv_trkorr TYPE ty_trlog-trkorr
                        iv_guid   TYPE ty_trlog-guid
                        iv_text   TYPE string.

  DATA: lt_comments TYPE tyt_trlog.
  DATA: ls_trlog LIKE LINE OF gt_trlog.
  FIELD-SYMBOLS: <comm> LIKE LINE OF lt_comments.

  DATA: lv_count TYPE i.
  DATA: lv_rep TYPE flag.
  DATA: lt_trkorr TYPE cts_trkorrs.
  DATA: ls_comm LIKE LINE OF gt_comments.

  SELECT * FROM zacntrlog INTO TABLE lt_comments
                          WHERE guid EQ iv_guid.

  DESCRIBE TABLE lt_comments LINES lv_count.

  IF lv_count GT 1.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Plusieurs ordres concernés'
        text_question  = 'Plusieurs OT ont le même commmentaire, modifier tous les OT ?'
      IMPORTING
        answer         = lv_rep
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CASE lv_rep.
      WHEN 1.
        LOOP AT lt_comments ASSIGNING <comm>.
          PERFORM calc_text USING iv_text
                         CHANGING <comm>-lraw
                                  <comm>-len.
          CLEAR ls_comm.
          READ TABLE gt_comments INTO ls_comm
                                 WITH KEY trkorr = <comm>-trkorr
                                          guid   = <comm>-guid.
          IF sy-subrc EQ 0.
            ls_comm-text = iv_text.
            MODIFY gt_comments FROM ls_comm INDEX sy-tabix.
          ENDIF.

          CLEAR ls_trlog.
          READ TABLE gt_trlog INTO ls_trlog
                              WITH KEY trkorr = <comm>-trkorr
                                       guid   = <comm>-guid.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING <comm> TO ls_trlog.
            MODIFY gt_trlog FROM ls_trlog INDEX sy-tabix.
          ENDIF.
        ENDLOOP.

        MODIFY zacntrlog FROM TABLE lt_comments.

      WHEN 2.
        DELETE lt_comments WHERE trkorr NE iv_trkorr.
        DELETE gt_comments WHERE trkorr EQ iv_trkorr
                           AND   guid EQ iv_guid.
        DELETE gt_trlog    WHERE trkorr EQ iv_trkorr
                           AND   guid EQ iv_guid.

        DELETE zacntrlog FROM TABLE lt_comments.


        APPEND iv_trkorr TO lt_trkorr.
        PERFORM add_text USING lt_trkorr iv_text.

      WHEN OTHERS.
    ENDCASE.
  ELSE.
    DELETE lt_comments WHERE trkorr NE iv_trkorr.
    DELETE gt_comments WHERE trkorr EQ iv_trkorr
                       AND   guid EQ iv_guid.
    DELETE gt_trlog    WHERE trkorr EQ iv_trkorr
                       AND   guid EQ iv_guid.

    DELETE zacntrlog FROM TABLE lt_comments.


    APPEND iv_trkorr TO lt_trkorr.
    PERFORM add_text USING lt_trkorr iv_text.

  ENDIF.

ENDFORM.                    " CHANGE_TEXT
*&---------------------------------------------------------------------*
*&      Form  CALC_TEXT
*&---------------------------------------------------------------------*
FORM calc_text  USING    iv_text TYPE string
                CHANGING ov_raw TYPE ty_trlog-lraw
                         ov_len TYPE ty_trlog-len.

*  DATA: ls_comm TYPE ty_trlog.
*  DATA: ls_comment LIKE LINE OF gt_comments.
*  DATA: lt_comm TYPE tyt_trlog.
*  DATA: ls_ot LIKE LINE OF it_trkorr.
*  DATA: lv_guid TYPE guid.
  DATA: lv_raw TYPE ty_trlog-lraw.
*  DATA: cl_guid TYPE REF TO cl_system_uuid.
*  DATA: lv_output TYPE xstring.


  CREATE OBJECT uref.

  CREATE OBJECT csref
    EXPORTING
      conversion     = 'DEFAULT'
      output_handler = uref.

  TRY.
      CALL METHOD csref->set_out_buf
        IMPORTING
          out_buf = lv_raw.
    CATCH cx_parameter_invalid_range .
      IF 1 = 2.
      ENDIF.
  ENDTRY.

  CALL METHOD csref->compress_text_stream_end
    EXPORTING
      text_in     = iv_text   " some more text
      text_in_len = -1.

  ov_len = uref->v_len.
  ov_raw = uref->v_xstring.

  FREE: uref, csref.
ENDFORM.                    " CALC_TEXT
*&---------------------------------------------------------------------*
*&      Form  DELETE_TEXT
*&---------------------------------------------------------------------*
FORM delete_text  USING iv_trkorr TYPE ty_trlog-trkorr
                        iv_guid   TYPE ty_trlog-guid.

  DATA: lt_comments TYPE tyt_trlog.
  DATA: ls_trlog LIKE LINE OF gt_trlog.
  FIELD-SYMBOLS: <comm> LIKE LINE OF lt_comments.

  DATA: lv_count TYPE i.
  DATA: lv_rep TYPE flag.
  DATA: lt_trkorr TYPE cts_trkorrs.
  DATA: ls_comm LIKE LINE OF gt_comments.

  SELECT * FROM zacntrlog INTO TABLE lt_comments
                          WHERE guid EQ iv_guid.

  DESCRIBE TABLE lt_comments LINES lv_count.

  IF lv_count GT 1.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Plusieurs ordres concernés'
        text_question  = 'Plusieurs OT ont le même commmentaire, modifier tous les OT ?'
      IMPORTING
        answer         = lv_rep
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CASE lv_rep.
      WHEN 1.

        DELETE zacntrlog FROM TABLE lt_comments.
        DELETE gt_trlog WHERE guid EQ iv_guid.
        DELETE gt_comments WHERE guid EQ iv_guid.

      WHEN 2.

        DELETE lt_comments WHERE trkorr NE iv_trkorr.

        DELETE zacntrlog FROM TABLE lt_comments.
        DELETE gt_trlog WHERE trkorr EQ iv_trkorr
                        AND   guid EQ iv_guid.
        DELETE gt_comments WHERE trkorr EQ iv_trkorr
                           AND   guid EQ iv_guid.

      WHEN OTHERS.
    ENDCASE.
  ELSE.
    DELETE lt_comments WHERE trkorr NE iv_trkorr.

    DELETE zacntrlog FROM TABLE lt_comments.
    DELETE gt_trlog WHERE trkorr EQ iv_trkorr
                    AND   guid EQ iv_guid.
    DELETE gt_comments WHERE trkorr EQ iv_trkorr
                       AND   guid EQ iv_guid.

  ENDIF.

ENDFORM.                    " DELETE_TEXT
