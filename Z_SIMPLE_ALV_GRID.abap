*&---------------------------------------------------------------------*
*& Report: Z_SIMPLE_ALV_GRID                                           *
*& Created: 04/07/2021                                                 *
*& By: Dustin K. Redmond                                               *
*&---------------------------------------------------------------------*
*&  Simple report to demonstrate the built-in function module          *
*&  REUSE_ALV_GRID_DISPLAY.                                            *
*&                                                                     *
*&  1. Creates a selection screen prompting date and optionally user   *
*&  2. Selects data into an internal table from VBRK (invoice data)    *
*&  3. Builds a field catalog for displaying the data.                 *
*&  4. Displays an ALV report with custom TOP_OF_PAGE data             *
*&  5. Listens for user command, on click of an invoice, takes user    *
*&     to the VF03 transaction to display the billing document.        *
*&---------------------------------------------------------------------*

REPORT Z_SIMPLE_ALV_GRID.

*&---------------------------------------------------------------------*
*& Tables used in program                                              *
*&---------------------------------------------------------------------*
*& 1. VBRK - Billing Document: Header Data                             *
*&---------------------------------------------------------------------*
TABLES: VBRK.

DATA:
  it_listheader TYPE slis_t_listheader,    " ALV list header internal table
  wa_listheader TYPE slis_listheader,      " Work area for list header
  it_fieldcat   TYPE slis_t_fieldcat_alv,  " Field catalog internal table
  wa_fieldcat   TYPE slis_fieldcat_alv,    " Work area for field catalog
  BEGIN OF it_vbrk OCCURS 0,               " Internal table to display in ALV
  VBELN LIKE VBRK-VBELN,
  ERDAT LIKE VBRK-ERDAT,
  ERNAM LIKE VBRK-ERNAM,
  SPART LIKE VBRK-SPART,
  END OF it_vbrk.

*&---------------------------------------------------------------------*
*& SELECTION-SCREEN                                                    *
*&---------------------------------------------------------------------*
*& Parameters:                                                         *
*&   1. Create Date (range, required)                                  *
*&   2. Username (optional)                                            *
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK one WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_cdte FOR SY-DATUM OBLIGATORY.
PARAMETER: p_unam LIKE SY-UNAME.
SELECTION-SCREEN: END OF BLOCK one.


*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN event                                           *
*&---------------------------------------------------------------------*
*& Called when user submits selection screen.                          *
*& Checks what parameters were specified then queries the data into    *
*& it_vbrk (internal table) then calls display_data subroutine.         *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF p_unam IS INITIAL.
    SELECT VBELN ERDAT ERNAM SPART
      FROM VBRK
      INTO CORRESPONDING FIELDS OF TABLE it_vbrk
      WHERE ERDAT IN so_cdte.
  ELSE.
    SELECT VBELN ERDAT ERNAM SPART
      FROM VBRK
      INTO CORRESPONDING FIELDS OF TABLE it_vbrk
      WHERE ERDAT IN so_cdte
            AND ERNAM EQ p_unam.
  ENDIF.

  IF LINES( it_vbrk ) EQ 0.
    MESSAGE 'Query caused no data to be retrieved.' TYPE 'I'.
  ELSE.
    PERFORM display_data.
  ENDIF.

*&---------------------------------------------------------------------*
*& FORM display_data                                                   *
*&---------------------------------------------------------------------*
*& Builds a field catalog to be used by the ALV, then calls the        *
*& REUSE_ALV_GRID_DISPLAY function module.                             *
*&---------------------------------------------------------------------*
FORM display_data.
  wa_fieldcat-fieldname = 'VBELN'.
  wa_fieldcat-seltext_m = 'Invoice Number'.
  wa_fieldcat-datatype  = 'NUMC'.
  wa_fieldcat-outputlen = '14'.
  wa_fieldcat-hotspot   = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'ERDAT'.
  wa_fieldcat-seltext_m = 'Created'.
  wa_fieldcat-outputlen = '10'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'ERNAM'.
  wa_fieldcat-seltext_m = 'Creator'.
  wa_fieldcat-outputlen = '10'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'SPART'.
  wa_fieldcat-seltext_m = 'Division'.
  wa_fieldcat-outputlen = '8'.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

  " Show ALV grid
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = SY-REPID         " Report ID
      I_CALLBACK_USER_COMMAND = 'PROCESS_UCOMM'  " Callback for user commands
      I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'    " Callback for ALV heading
      IT_FIELDCAT             = it_fieldcat      " Field Catalog for ALV
    TABLES
      T_OUTTAB                = it_vbrk          " Internal table for ALV
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    MESSAGE 'Unable to display data. Please contact IT.' TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM process_ucomm                                                  *
*&---------------------------------------------------------------------*
*& Callback subroutine for user commands on ALV grid                   *
*& If user clicks an invoice, send them to VF03 (display invoice)      *
*&---------------------------------------------------------------------*
FORM process_ucomm  USING uc LIKE SY-UCOMM sel TYPE SLIS_SELFIELD.
  IF sel-fieldname = 'VBELN' AND sel-value <> ''.
    SET PARAMETER ID 'VF' FIELD sel-value.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM top_of_page                                                    *
*&---------------------------------------------------------------------*
*& Callback subroutine for setting the ALV headings                    *
*&---------------------------------------------------------------------*
FORM top_of_page.
  " Clear the listheader, if user goes to VF03 then back
  " to the ALV, the listheader will have data appended again.
  CLEAR it_listheader.

  " Start and End dates for display output
  DATA: lv_start_date(10) TYPE c,
        lv_end_date(10) TYPE c.

  " Switch date from yyyymmdd (database value) to mm/dd/yyyy
  CONCATENATE so_cdte-low+4(2) so_cdte-low+6(2) so_cdte-low(4)
    INTO lv_start_date SEPARATED BY '/'.

  " Show report run parameters based on which are specified
  IF so_cdte-high IS INITIAL.
    wa_listheader-typ = 'S'.
    wa_listheader-key = 'Created Date:' .
    wa_listheader-info = lv_start_date.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
  ELSE.
    wa_listheader-typ = 'S'.
    wa_listheader-key = 'Start Date:' .
    wa_listheader-info = lv_start_date.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.

    CONCATENATE so_cdte-high+4(2) so_cdte-high+6(2) so_cdte-high(4)
    INTO lv_end_date SEPARATED BY '/'.
    wa_listheader-typ = 'S'.
    wa_listheader-key = 'End Date:'.
    wa_listheader-info = lv_end_date.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
  ENDIF.

  IF p_unam IS NOT INITIAL.
    wa_listheader-typ = 'S'.
    wa_listheader-key = 'User:'.
    wa_listheader-info = p_unam.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.

    wa_listheader-typ  = 'H'.
    wa_listheader-info ='Invoices by Created Date and User'.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
  ELSE.
    wa_listheader-typ = 'H'.
    wa_listheader-info = 'Invoices By Created Date'.
    APPEND wa_listheader TO it_listheader.
    CLEAR wa_listheader.
  ENDIF.

  " Write the headings to the ALV grid
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_listheader.
ENDFORM.
