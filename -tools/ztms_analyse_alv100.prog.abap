*&---------------------------------------------------------------------*
*&  Include           ZACNTRANSP_ALV
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_grid             TYPE REF TO cl_gui_alv_grid,
      g_container        TYPE scrfname VALUE 'CONT_0100',
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_event_receiver   TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

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
CLASS lcl_event_receiver IMPLEMENTATION.
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
    MOVE 'REFRESH' TO ls_toolbar-function.
    MOVE icon_refresh TO ls_toolbar-icon.
    MOVE 'Refresh'(111) TO ls_toolbar-quickinfo.
    MOVE ''(111) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'SELECT' TO ls_toolbar-function.
    MOVE icon_modify TO ls_toolbar-icon.
    MOVE 'Sélection dépendance'(111) TO ls_toolbar-quickinfo.
    MOVE 'Sélection dépendance'(111) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'FILTER' TO ls_toolbar-function.
    MOVE icon_filter TO ls_toolbar-icon.
    MOVE 'Filtrer sur sélection'(112) TO ls_toolbar-quickinfo.
    MOVE 'Filtrer sur sélection'(112) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'SHOW' TO ls_toolbar-function.
    MOVE icon_display TO ls_toolbar-icon.
    MOVE 'Afficher contenu'(113) TO ls_toolbar-quickinfo.
    MOVE 'Afficher contenu'(113) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'MULTI' TO ls_toolbar-function.
    MOVE icon_display_more TO ls_toolbar-icon.
    MOVE 'Sélection multiple'(114) TO ls_toolbar-quickinfo.
    MOVE 'Sélection multiple'(114) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ADD' TO ls_toolbar-function.
    MOVE icon_create_text TO ls_toolbar-icon.
    MOVE 'Ajouter commentaire'(115) TO ls_toolbar-quickinfo.
    MOVE 'Ajouter commentaire'(115) TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar
  METHOD handle_after_user_command.
    PERFORM colorize CHANGING gt_alv.
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

    DATA: lt_sel_idx TYPE lvc_t_row,
          ls_sel LIKE LINE OF lt_sel_idx.

    DATA: lt_ot_in TYPE zacn_t_trkorr,
          lt_ot_range TYPE zacn_t_trkorr,
          lt_ot_out TYPE zacn_t_trkorr,
          ls_trkorr LIKE LINE OF lt_ot_in,

          ls_alv LIKE LINE OF gt_alv,

          lt_filter TYPE lvc_t_filt,
          ls_filter LIKE LINE OF lt_filter,
          lr_trkorr TYPE tyr_trkorr,
          ls_otrange LIKE LINE OF lr_trkorr.

    DATA: lt_trkorr TYPE cts_trkorrs.
    DATA: lt_alv TYPE tyt_alv.
    DATA: lv_datefrom TYPE datum.
    DATA: lv_dateto TYPE datum.
    DATA: lt_fieldcat TYPE lvc_t_fcat.

    CASE e_ucomm.
      WHEN 'REFRESH'.

        DATA: lt_entries TYPE lvc_t_fidx.
        DATA: ls_entrie LIKE LINE OF lt_entries.
        CALL METHOD g_grid->get_filtered_entries
          IMPORTING
            et_filtered_entries = lt_entries.

        lt_alv = gt_alv.

        SORT lt_entries DESCENDING.

        LOOP AT lt_entries INTO ls_entrie.
          DELETE lt_alv INDEX ls_entrie.
        ENDLOOP.
        PERFORM date_from_alv USING lt_alv
                           CHANGING lv_datefrom lv_dateto.

        CLEAR lt_alv.
        PERFORM extract_tms USING lv_datefrom lv_dateto
                                  gt_list_sys
                         CHANGING lt_alv.

        PERFORM extract_log CHANGING lt_alv.

        PERFORM refresh_tabalv USING lt_alv
                            CHANGING gt_alv.

        PERFORM colorize CHANGING gt_alv.
*        PERFORM build_fieldcat
*                    CHANGING
*                       lt_alv
*                       lt_fieldcat.

*        gt_alv[] = lt_alv[].
        CALL METHOD g_grid->refresh_table_display.

      WHEN 'ADD'.
        CLEAR lt_rowid.
        DATA: lv_row TYPE i,
              lv_value TYPE c,
              lv_col TYPE i,
              ls_rowid2 TYPE lvc_s_row,
              ls_colid TYPE lvc_s_col,
              ls_rowno TYPE lvc_s_roid.


        CALL METHOD g_grid->get_current_cell
          IMPORTING
            e_row     = lv_row
            e_value   = lv_value
            e_col     = lv_col
            es_row_id = ls_rowid2
            es_col_id = ls_colid
            es_row_no = ls_rowno.

        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rowid.

        CHECK lt_rowid IS NOT INITIAL.
        gv_textmode = gc_textmode_new.
        CALL SCREEN 0400 STARTING AT 5 5.

        CLEAR lt_trkorr.

        LOOP AT lt_rowid INTO ls_rowid.
          CLEAR ls_alv.
          READ TABLE gt_alv INTO ls_alv INDEX ls_rowid-index.
          IF sy-subrc EQ 0.
            ls_trkorr-trkorr = ls_alv-trkorr_exp_src.
            APPEND ls_trkorr TO lt_trkorr.
          ENDIF.
        ENDLOOP.


        IF gv_text IS NOT INITIAL.
          PERFORM add_text USING lt_trkorr gv_text.
        ENDIF.
        PERFORM update_log CHANGING gt_alv.

        CALL METHOD g_grid->refresh_table_display.

        CALL METHOD g_grid->set_current_cell_via_id
          EXPORTING
            is_row_id    = ls_rowid2
            is_column_id = ls_colid
            is_row_no    = ls_rowno.


      WHEN 'MULTI'.
        CALL SCREEN 200 STARTING AT 5 5.

        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rowid.

        LOOP AT gt_alv INTO ls_alv.

          IF s_lib[] IS NOT INITIAL AND
             ls_alv-as4text IN s_lib.
            ls_sel-index = sy-tabix.
            APPEND ls_sel TO lt_rowid.
          ENDIF.

          IF s_trkorr[] IS NOT INITIAL AND
              ls_alv-trkorr_exp_src IN s_trkorr.
            ls_sel-index = sy-tabix.
            APPEND ls_sel TO lt_rowid.
          ENDIF.
        ENDLOOP.

        CALL METHOD g_grid->set_selected_rows
          EXPORTING
            it_index_rows = lt_rowid.

      WHEN 'FILTER'.
        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_sel_idx
*           et_row_no     =                                                                                                                                             lt_sel_num
          .

        LOOP AT lt_sel_idx INTO ls_sel.
          CLEAR ls_alv.
          READ TABLE gt_alv INTO ls_alv INDEX ls_sel-index.
          IF sy-subrc EQ 0.
            ls_filter-fieldname = 'TRKORR_EXP_SRC'.
            ls_filter-sign = 'I'.
            ls_filter-option = 'EQ'.
            ls_filter-low = ls_alv-trkorr_exp_src.
            APPEND ls_filter TO lt_filter.
          ENDIF.

        ENDLOOP.

        CALL METHOD g_grid->set_filter_criteria
          EXPORTING
            it_filter                 = lt_filter
          EXCEPTIONS
            no_fieldcatalog_available = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          CALL METHOD g_grid->refresh_table_display.
        ENDIF.

      WHEN 'SHOW'.

        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_sel_idx
*           et_row_no     =                                                                                                                                             lt_sel_num
          .

        CHECK lt_sel_idx IS NOT INITIAL.

        LOOP AT lt_sel_idx INTO ls_sel.
          CLEAR ls_alv.
          READ TABLE gt_alv INTO ls_alv INDEX ls_sel-index.
          IF sy-subrc EQ 0.
            ls_trkorr-trkorr = ls_alv-trkorr_exp_src.
            APPEND ls_trkorr TO lt_trkorr.
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'TR_DISPLAY_REQUESTS'
          EXPORTING
            it_request_numbers = lt_trkorr.

      WHEN 'SELECT'.

        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_sel_idx
*           et_row_no     =                                                                                                                                             lt_sel_num
          .

        CALL METHOD g_grid->get_filter_criteria
          IMPORTING
            et_filter = lt_filter.    " Filter Criteria

        LOOP AT lt_filter INTO ls_filter.
          MOVE-CORRESPONDING ls_filter TO ls_otrange.
          INSERT ls_otrange INTO TABLE lr_trkorr.
        ENDLOOP.

        IF lr_trkorr IS INITIAL.
          ls_otrange = 'IEQ'.
          LOOP AT gt_alv INTO ls_alv.
            ls_otrange-low = ls_alv-trkorr_exp_src.
            INSERT ls_otrange INTO TABLE lr_trkorr.
          ENDLOOP.
        ENDIF.

        LOOP AT lt_sel_idx INTO ls_sel.
          CLEAR ls_alv.
          READ TABLE gt_alv INTO ls_alv INDEX ls_sel-index.
          IF sy-subrc EQ 0.
            ls_trkorr-trkorr = ls_alv-trkorr_exp_src.
            APPEND ls_trkorr TO lt_ot_in.
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'ZTMS_ANALYSE_TR_DEPENDENCY'
          EXPORTING
            it_trkorr       = lt_ot_in
            it_trkorr_range = lr_trkorr
          IMPORTING
            ot_trkorr       = lt_ot_out.

        LOOP AT lt_ot_out INTO ls_trkorr.
          READ TABLE gt_alv WITH KEY trkorr_exp_src = ls_trkorr-trkorr
                            TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            ls_sel-index = sy-tabix.
            APPEND ls_sel TO lt_rowid.
          ENDIF.
        ENDLOOP.

        CALL METHOD g_grid->set_selected_rows
          EXPORTING
            it_index_rows = lt_rowid.
    ENDCASE.
  ENDMETHOD.                           "handle_user_command
*-----------------------------------------------------------------
  METHOD handle_data_changed.

  ENDMETHOD.                    "handle_data_changed

  METHOD check_value.

  ENDMETHOD.                    "check

  METHOD handle_double_click.
    DATA: ls_alv LIKE LINE OF gt_alv,
          lt_trkorr TYPE cts_trkorrs,
          ls_trkorr LIKE LINE OF lt_trkorr,
          lv_syst TYPE ty_list_sys-struct_txt,
          ls_list_sys LIKE LINE OF gt_list_sys.

    READ TABLE gt_alv INTO ls_alv INDEX e_row.
    CASE e_column.
      WHEN 'COMMENT'.
        DATA: lv_row TYPE i,
              lv_value TYPE c,
              lv_col TYPE i,
              ls_rowid2 TYPE lvc_s_row,
              ls_colid TYPE lvc_s_col,
              ls_rowno TYPE lvc_s_roid.


        CALL METHOD g_grid->get_current_cell
          IMPORTING
            e_row     = lv_row
            e_value   = lv_value
            e_col     = lv_col
            es_row_id = ls_rowid2
            es_col_id = ls_colid
            es_row_no = ls_rowno.

        gv_trkorr = ls_alv-trkorr_exp_src.
        PERFORM popupalv_comment USING ls_alv-trkorr_exp_src.
        PERFORM update_log CHANGING gt_alv.
        CALL METHOD g_grid->refresh_table_display.

        CALL METHOD g_grid->set_current_cell_via_id
          EXPORTING
            is_row_id    = ls_rowid2
            is_column_id = ls_colid
            is_row_no    = ls_rowno.

      WHEN 'TRKORR_EXP_SRC'.
        ls_trkorr-trkorr = ls_alv-trkorr_exp_src.
        APPEND ls_trkorr TO lt_trkorr.

        CALL FUNCTION 'TR_DISPLAY_REQUESTS'
          EXPORTING
            it_request_numbers = lt_trkorr.

      WHEN OTHERS.
        lv_syst = e_column(5).

        CLEAR ls_list_sys.
        READ TABLE gt_list_sys INTO ls_list_sys
                   WITH KEY struct_txt = lv_syst.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'TMS_UI_SHOW_TRANSPORT_LOGS'
            EXPORTING
              iv_request = ls_alv-trkorr_exp_src
              iv_system  = ls_list_sys-system.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "handle_DOUBLE_CLICK
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*      <--P_GT_FIELDCAT  text
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM create_and_init_alv CHANGING it_alv TYPE tyt_alv.

  DATA: lt_exclude  TYPE ui_functions.
  DATA: ls_layout   TYPE lvc_s_layo.
  DATA: lt_fieldcat TYPE lvc_t_fcat.
  DATA: ls_variant TYPE disvariant.
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name = g_container.
  CREATE OBJECT g_grid
    EXPORTING
      i_parent = g_custom_container.

  PERFORM build_fieldcat CHANGING it_alv
                                  lt_fieldcat.

*§2.Optionally restrict generic functions to 'change only'.
*   (The user shall not be able to add new lines).
  PERFORM exclude_tb_functions CHANGING lt_exclude.

  ls_layout-cwidth_opt = 'X'.
  ls_layout-stylefname = 'CELLTAB'.
  ls_layout-no_totline = 'X'.
  ls_layout-ctab_fname = 'T_COLOR'.
  ls_layout-edit       = 'X'.

  SORT it_alv BY syst1-last.
  PERFORM colorize CHANGING it_alv.

*§3.Optionally register ENTER to raise event DATA_CHANGED.
*   (Per default the user may check data by using the check icon).
  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_user_command FOR g_grid.
  SET HANDLER g_event_receiver->handle_after_user_command FOR g_grid.
  SET HANDLER g_event_receiver->handle_data_changed FOR g_grid.
  SET HANDLER g_event_receiver->handle_toolbar FOR g_grid.
  SET HANDLER g_event_receiver->handle_data_changed_finished FOR g_grid .
  SET HANDLER g_event_receiver->handle_after_refresh FOR g_grid .
  SET HANDLER g_event_receiver->handle_double_click FOR g_grid.

  ls_variant-report = sy-repid.

  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layout
      it_toolbar_excluding = lt_exclude
      i_save               = 'A'
      is_variant           = ls_variant
    CHANGING
      it_fieldcatalog      = lt_fieldcat
*     it_sort              = gt_sort_lvc
      it_outtab            = it_alv.


  CALL METHOD g_grid->set_toolbar_interactive.

ENDFORM.                               "CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat CHANGING ot_alv TYPE tyt_alv
                             ot_fieldcat TYPE lvc_t_fcat.

  DATA: ls_fieldcat LIKE LINE OF ot_fieldcat.
  DATA: ls_list_sys LIKE LINE OF gt_list_sys.
  DATA: lt_celltab  TYPE lvc_t_styl,
        ls_celltab  LIKE LINE OF lt_celltab.

  FIELD-SYMBOLS : <fcat> TYPE lvc_s_fcat,
                  <falv> LIKE LINE OF ot_alv.

  ls_fieldcat-col_pos   = 1.
  ls_fieldcat-fieldname = 'COMMENT'.
  ls_fieldcat-scrtext_l = '*'.
  ls_fieldcat-outputlen = 2.
  ls_fieldcat-key = 'X'.
  ls_fieldcat-rollname  = ''.

  APPEND ls_fieldcat TO ot_fieldcat.

  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'TRKORR_EXP_SRC'.
  ls_fieldcat-scrtext_l = 'N° d''ordre'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-key = 'X'.
  ls_fieldcat-rollname  = 'TRKORR'.

  APPEND ls_fieldcat TO ot_fieldcat.

  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'IDX_EXP_SRC'.
  ls_fieldcat-scrtext_l = 'Séquence export NOD'.
  ls_fieldcat-key = ' '.
  ls_fieldcat-rollname  = ' '.

  APPEND ls_fieldcat TO ot_fieldcat.

  LOOP AT gt_list_sys INTO ls_list_sys.
    CHECK ls_list_sys-idx NE 0.

    PERFORM add_syst_fieldcat USING ls_list_sys
                           CHANGING ls_fieldcat-col_pos
                                    ot_fieldcat.
  ENDLOOP.

  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'PROJECT'.
  ls_fieldcat-scrtext_l = 'PROJET'.
  ls_fieldcat-rollname  = ' '.

  APPEND ls_fieldcat TO ot_fieldcat.


  ADD 1 TO ls_fieldcat-col_pos.
  ls_fieldcat-fieldname = 'AS4TEXT'.
  ls_fieldcat-scrtext_l = 'Libellé d''ordre'.
  ls_fieldcat-rollname  = ' '.

  APPEND ls_fieldcat TO ot_fieldcat.

  LOOP AT ot_fieldcat ASSIGNING <fcat>.
    ls_celltab-fieldname = <fcat>-fieldname.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  LOOP AT ot_alv ASSIGNING <falv>.
    <falv>-celltab = lt_celltab.
  ENDLOOP.


ENDFORM.                    "build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.
* Only allow to change data not to create new entries (exclude
* generic functions).

  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_maximum.             APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_minimum.             APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.              APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.                 APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_average.             APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_sum.                 APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_subtot.              APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_views.               APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_view_crystal.        APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_view_excel.          APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_view_grid.           APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_view_lotus.          APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_word_processor.      APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_lystyle_drag_drop_rows. APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_lystyle_no_delete_rows. APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_lystyle_no_insert_rows. APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_ly_drag_drop_rows.      APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_ly_no_delete_rows.      APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_ly_no_insert_rows.      APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_export.              APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_filter.              APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_paste.               APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_subtot.              APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_sum.                 APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_variant.             APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_mb_view.                APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fg_sort.                APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fg_edit.                APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style4_link.            APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style4_link_no.         APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_button.           APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_disabled.         APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_enabled.          APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_f4.               APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_f4_no.            APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_hotspot.          APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_hotspot_no.       APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_style_no_delete_row .   APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print_back.          APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_find.                APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_detail.              APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_print.               APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.               APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.                APPEND ls_exclude TO pt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.        APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.      APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.      APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.      APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.        APPEND ls_exclude TO pt_exclude.

ENDFORM.                               " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  add_syst_fieldcat
*&---------------------------------------------------------------------*
FORM add_syst_fieldcat  USING iv_syst     TYPE ty_list_sys
                     CHANGING ov_pos      TYPE slis_fieldcat_main0-col_pos
                              ot_fieldcat TYPE lvc_t_fcat.


  DATA: ls_fieldcat LIKE LINE OF ot_fieldcat.                .

  DATA: lv_fieldname TYPE slis_fieldname.

*         date TYPE datum,
  ADD 1 TO ov_pos.
  ls_fieldcat-col_pos = ov_pos.
  CONCATENATE 'SYST' iv_syst-idx '-DATE' INTO lv_fieldname.
  ls_fieldcat-fieldname = lv_fieldname.

  CONCATENATE '(' iv_syst-system ') ' 'Date'
  INTO ls_fieldcat-scrtext_l.
  ls_fieldcat-rollname  = ' '.
  APPEND ls_fieldcat TO ot_fieldcat.
*         time TYPE uzeit,
  ADD 1 TO ov_pos.
  ls_fieldcat-col_pos = ov_pos.
  CONCATENATE 'SYST' iv_syst-idx '-TIME' INTO lv_fieldname.
  ls_fieldcat-fieldname = lv_fieldname.
  CONCATENATE '(' iv_syst-system ') ' 'Heure'
  INTO ls_fieldcat-scrtext_l.

  ls_fieldcat-rollname  = ' '.
  APPEND ls_fieldcat TO ot_fieldcat.
*         cnt  TYPE i,
  ADD 1 TO ov_pos.
  ls_fieldcat-col_pos = ov_pos.
  CONCATENATE 'SYST' iv_syst-idx '-CNT' INTO lv_fieldname.
  ls_fieldcat-fieldname = lv_fieldname.
  CONCATENATE '(' iv_syst-system ') ' 'Nb'
  INTO ls_fieldcat-scrtext_l.

  ls_fieldcat-rollname  = ' '.
  APPEND ls_fieldcat TO ot_fieldcat.
*         first TYPE i,
  ADD 1 TO ov_pos.
  ls_fieldcat-col_pos = ov_pos.
  CONCATENATE 'SYST' iv_syst-idx '-FIRST' INTO lv_fieldname.
  ls_fieldcat-fieldname = lv_fieldname.
  CONCATENATE '(' iv_syst-system ') ' 'First'
  INTO ls_fieldcat-scrtext_l.

  ls_fieldcat-rollname  = ' '.
  APPEND ls_fieldcat TO ot_fieldcat.

*         last TYPE i,
  ADD 1 TO ov_pos.
  ls_fieldcat-col_pos = ov_pos.
  CONCATENATE 'SYST' iv_syst-idx '-LAST' INTO lv_fieldname.
  ls_fieldcat-fieldname = lv_fieldname.
  CONCATENATE '(' iv_syst-system ') ' 'Last'
  INTO ls_fieldcat-scrtext_l.

  ls_fieldcat-rollname  = ' '.
  APPEND ls_fieldcat TO ot_fieldcat.

ENDFORM.                    " add_syst_fieldcat
*&---------------------------------------------------------------------*
*&      Form  POPUPALV_COMMENT
*&---------------------------------------------------------------------*
FORM popupalv_comment USING iv_trkorr TYPE trkorr.

  DATA: tabix_log TYPE i.
  DATA: ls_log LIKE LINE OF gt_trlog.
  DATA: ls_comment LIKE LINE OF gt_comments.

  CLEAR gt_comments.

  READ TABLE gt_trlog WITH KEY trkorr = iv_trkorr
                      TRANSPORTING NO FIELDS
                      BINARY SEARCH.
  IF sy-subrc EQ 0.
    tabix_log = sy-tabix.
    LOOP AT gt_trlog INTO ls_log FROM tabix_log.
      IF ls_log-trkorr NE iv_trkorr.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING ls_log TO ls_comment.
      ls_comment-more = icon_zoom_in.

      PERFORM read_text USING ls_comment-len
                              ls_comment-lraw
                     CHANGING ls_comment-text.
      APPEND ls_comment TO gt_comments.
    ENDLOOP.

  ENDIF.

  CALL SCREEN 0300 STARTING AT 5 5.
ENDFORM.                    " POPUPALV_COMMENT
