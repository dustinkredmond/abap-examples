*&---------------------------------------------------------------------*
*& Report  Z_LOCKED_USERS                                              *
*& Created: 05/28/2021                                                 *
*& By: Dustin K. Redmond                                               *
*&---------------------------------------------------------------------*
*& Report with selection screen to get list of locked/unlocked users.  *
*& Report user can click on a user to be taken to SU01 for that user.  *
*&---------------------------------------------------------------------*

REPORT z_locked_users.

TABLES: usr02.

TYPES: BEGIN OF ty_user,
        bname TYPE usr02-bname,
        erdat TYPE usr02-erdat,
        uflag TYPE usr02-uflag,
       END OF ty_user.

DATA: gt_user TYPE STANDARD TABLE OF ty_user,
      gs_user TYPE ty_user.

* local class to work as event handler for click on ALV grid
CLASS lcl_alv_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: on_line_click FOR EVENT link_click OF cl_salv_events_table
              IMPORTING row column.
ENDCLASS.
CLASS lcl_alv_handler IMPLEMENTATION.
  METHOD on_line_click.
    READ TABLE gt_user INTO gs_user INDEX row.
    SET PARAMETER ID 'XUS' FIELD gs_user-bname.
    CALL TRANSACTION 'SU01'.
  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN: BEGIN OF BLOCK one WITH FRAME TITLE text-001.
PARAMETERS: p_lock  RADIOBUTTON GROUP rg1,
            p_nlock RADIOBUTTON GROUP rg1.
SELECTION-SCREEN: END OF BLOCK one.

AT SELECTION-SCREEN.
  PERFORM get_data.
  PERFORM display_data.

*----------------------------------------------------------------------*
*  FORM get_data                                                       *
*----------------------------------------------------------------------*
* Fetches data from usr02 table into gt_user internal table based on   *
* user selection criteria.                                             *
*----------------------------------------------------------------------*
FORM get_data.
  IF p_lock IS NOT INITIAL.
    SELECT bname erdat uflag FROM usr02
      INTO CORRESPONDING FIELDS OF TABLE gt_user
      WHERE uflag NE 0.
  ELSE.
    SELECT bname erdat uflag FROM usr02
      INTO CORRESPONDING FIELDS OF TABLE gt_user
      WHERE uflag EQ 0.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  FORM display_data                                                   *
*----------------------------------------------------------------------*
* Displays the contents of internal table (gt_user) in an ALV by using *
* the ABAP class cl_salv_table.                                        *
*----------------------------------------------------------------------*
FORM display_data.
  DATA: lr_table TYPE REF TO cl_salv_table,
        lr_events TYPE REF TO cl_salv_events_table,
        lr_event_handler TYPE REF TO lcl_alv_handler,
        lr_columns_table TYPE REF TO cl_salv_columns_table,
        lr_column_table  TYPE REF TO cl_salv_column_table.

  TRY.
      CALL METHOD cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = gt_user ).
    CATCH cx_salv_msg.
  ENDTRY.

  DATA: lv_heading TYPE char70.
  DATA: lv_count TYPE i.

  DESCRIBE TABLE gt_user LINES lv_count.

  IF p_lock IS NOT INITIAL.
    lv_heading = lv_count && ` locked users`.
  ELSE.
    lv_heading = lv_count && ` unlocked users`.
  ENDIF.

  lr_table->get_display_settings( )->set_list_header( lv_heading ).

  lr_table->get_columns( RECEIVING value = lr_columns_table ).
  TRY.
      lr_column_table ?= lr_columns_table->get_column( 'BNAME' ).
      lr_column_table->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.
  ENDTRY.

  lr_events = lr_table->get_event( ).
  CREATE OBJECT lr_event_handler.
  SET HANDLER lr_event_handler->on_line_click FOR lr_events.

  lr_table->get_columns( )->set_optimize( abap_true ).
  lr_table->display( ).

ENDFORM.
