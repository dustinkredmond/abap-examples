*&---------------------------------------------------------------------*
*& Report  Z_TCODE_INFO                                                *
*& Created: 05/28/2021                                                 *
*& By: Dustin K. Redmond                                               *
*&---------------------------------------------------------------------*
*& Writes an output file of all custom transactions in the system      *
*& along with the program name, author, and date of creation.          *
*&---------------------------------------------------------------------*

REPORT z_tcode_info.

TABLES: tstc,
        tadir.

INITIALIZATION.
  PERFORM download_file.

FORM download_file.
  DATA: lv_filename TYPE string,
        lv_path TYPE string,
        lv_fullpath TYPE string,
        lv_result TYPE i.

  TYPES: BEGIN OF ty_data,
          row(500) TYPE c,
          END OF ty_data,

         BEGIN OF ty_tstc,
           tcode like tstc-tcode,
           pgmna like tstc-pgmna,
           author like tadir-author,
           created_on like tadir-created_on,
         END OF ty_tstc.

  DATA: lt_data TYPE STANDARD TABLE OF ty_data,
        ls_data TYPE ty_data,
        ls_tstc TYPE ty_tstc.

  ls_data-row = 'sep=;'.
  APPEND ls_data TO lt_data.

  ls_data-row = 'Transaction Code; Program; Author; Created'.
  APPEND ls_data TO lt_data.

  SELECT tcode pgmna FROM tstc INTO ls_tstc
    WHERE tcode LIKE 'Z%' OR tcode LIKE 'Y%'.
    DATA: lv_author like tadir-author,
          lv_created like tadir-created_on.

   SELECT SINGLE author created_on FROM tadir INTO (lv_author, lv_created)
     WHERE object = 'TRAN' AND obj_name = ls_tstc-tcode.

   CONCATENATE ls_tstc-tcode ls_tstc-pgmna lv_author lv_created
    INTO ls_data-row SEPARATED BY ';'.
   APPEND ls_data TO lt_data.
   CLEAR ls_data.
  ENDSELECT.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select a save location...'
      default_extension = 'csv'
      default_file_name = 'transaction_report'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_result.

  IF lv_result EQ 0.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = lv_fullpath
        filetype              = 'ASC'
        write_field_separator = 'X'
      TABLES
        data_tab              = lt_data[]
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        OTHERS                = 3.
  ENDIF.
ENDFORM.
