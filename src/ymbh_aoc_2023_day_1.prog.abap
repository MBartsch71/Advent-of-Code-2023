REPORT ymbh_aoc_2023_day_1.

CLASS string_parser DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS parse_string IMPORTING input         TYPE string.

    METHODS result RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.

  PRIVATE SECTION.
    TYPES: BEGIN OF number_position,
             pos    TYPE i,
             number TYPE string,
           END OF number_position.
    TYPES number_positions TYPE SORTED TABLE OF number_position WITH UNIQUE KEY primary_key COMPONENTS pos.

    TYPES: BEGIN OF word_digit_item,
             word  TYPE string,
             digit TYPE char1,
           END OF word_digit_item.
    TYPES word_digit_map TYPE SORTED TABLE OF word_digit_item WITH UNIQUE KEY primary_key COMPONENTS word.

    DATA numbers TYPE number_positions.

    METHODS get_first_number RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.

    METHODS get_last_number RETURNING VALUE(result) TYPE REF TO ycl_mbh_string.

    METHODS parse_for_digits IMPORTING input TYPE string.

    METHODS parse_for_number_words IMPORTING input TYPE string.

    METHODS find_numbers_by_word IMPORTING word          TYPE string
                                           input         TYPE string
                                 RETURNING VALUE(result) TYPE match_result_tab.

    METHODS lookup_digit_for_word IMPORTING word          TYPE string
                                  RETURNING VALUE(result) TYPE string.

    METHODS is_number IMPORTING character     TYPE string
                      RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS string_parser IMPLEMENTATION.

  METHOD parse_string.
    parse_for_digits( input ).
    parse_for_number_words( input ).
  ENDMETHOD.

  METHOD result.
    result = get_first_number( )->concatentate_with( get_last_number( )->value( ) ).
  ENDMETHOD.

  METHOD get_first_number.
    result = ycl_mbh_string=>new( numbers[ 1 ]-number ).
  ENDMETHOD.

  METHOD get_last_number.
    result = ycl_mbh_string=>new( numbers[ lines( numbers ) ]-number ).
  ENDMETHOD.

  METHOD parse_for_digits.
    DATA(counter) = 0.
    DATA(iterator) = ycl_mbh_string=>new( input )->get_iterator( ).

    WHILE iterator->has_next( ).
      counter += 1.
      DATA(character) = CAST ycl_mbh_string( iterator->get_next( ) )->value( ).

      IF is_number( character ).
        numbers = VALUE #( BASE numbers ( pos = counter number = character ) ).
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_for_number_words.
    DATA(number_words) = VALUE stringtab( ( |one| ) ( |two| ) ( |three| ) ( |four| )
                                          ( |five| ) ( |six| ) ( |seven| ) ( |eight| )
                                          ( |nine| ) ).
    numbers = VALUE #( BASE numbers
                        FOR word IN number_words
                        LET results = find_numbers_by_word( word = word
                                                            input = input )
                        IN
                        FOR result IN results
                        ( pos    = result-offset + 1
                          number = lookup_digit_for_word( word ) ) ).
  ENDMETHOD.

  METHOD find_numbers_by_word.
    FIND ALL OCCURRENCES OF word IN input RESULTS result.
  ENDMETHOD.

  METHOD lookup_digit_for_word.
    DATA(digit_lookup) = VALUE word_digit_map( ( word = |one|   digit = |1| )
                                               ( word = |two|   digit = |2| )
                                               ( word = |three| digit = |3| )
                                               ( word = |four|  digit = |4| )
                                               ( word = |five|  digit = |5| )
                                               ( word = |six|   digit = |6| )
                                               ( word = |seven| digit = |7| )
                                               ( word = |eight| digit = |8| )
                                               ( word = |nine|  digit = |9| ) ).

    result = digit_lookup[ word = word ]-digit.
  ENDMETHOD.

  METHOD is_number.
    result = xsdbool( sy-abcde NS character ).
  ENDMETHOD.

ENDCLASS.

CLASS trebuchet DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS process_input IMPORTING input_data    TYPE stringtab
                          RETURNING VALUE(result) TYPE i.
  PRIVATE SECTION.
    METHODS parse_line IMPORTING line          TYPE string
                       RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS trebuchet IMPLEMENTATION.

  METHOD process_input.
    result = REDUCE #( INIT sum = 0
                       FOR line IN input_data
                       NEXT sum = sum + parse_line( line ) ).
  ENDMETHOD.

  METHOD parse_line.
    DATA(string_parser) = NEW string_parser( ).
    string_parser->parse_string( line ).
    result = string_parser->result( )->value( ).
  ENDMETHOD.

ENDCLASS.

CLASS test_string_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO string_parser.

    METHODS setup.
    METHODS rslt1_from_1st_and_last_number FOR TESTING.
    METHODS rslt2_from_1st_and_last_number FOR TESTING.
    METHODS rslt3_from_1st_and_last_number FOR TESTING.
    METHODS rslt4_from_1st_and_last_number FOR TESTING.

ENDCLASS.

CLASS test_string_parser IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD rslt1_from_1st_and_last_number.
    cut->parse_string( |1abc2| ).
    cl_abap_unit_assert=>assert_equals( exp = 12 act = cut->result( )->value( ) ).
  ENDMETHOD.

  METHOD rslt2_from_1st_and_last_number.
    cut->parse_string( |a1b2c3d4e5f| ).
    cl_abap_unit_assert=>assert_equals( exp = 15 act = cut->result( )->value( ) ).
  ENDMETHOD.

  METHOD rslt3_from_1st_and_last_number.
    cut->parse_string( |two1nine| ).
    cl_abap_unit_assert=>assert_equals( exp = 29 act = cut->result( )->value( ) ).
  ENDMETHOD.

  METHOD rslt4_from_1st_and_last_number.
    cut->parse_string( |eightwothree| ).
    cl_abap_unit_assert=>assert_equals( exp = 83 act = cut->result( )->value( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS test_trebuchet DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS can_create_object FOR TESTING.
    METHODS result_from_4_lines_is_142 FOR TESTING.
    METHODS result_for_second_part_is_281 FOR TESTING.

ENDCLASS.

CLASS test_trebuchet IMPLEMENTATION.

  METHOD can_create_object.
    DATA(cut) = NEW trebuchet( ).
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD result_from_4_lines_is_142.
    DATA(cut) = NEW trebuchet( ).
    DATA(input_data) = VALUE stringtab( ( |1abc2| )
                                        ( |pqr3stu8vwx| )
                                        ( |a1b2c3d4e5f| )
                                        ( |treb7uchet| ) ).
    cl_abap_unit_assert=>assert_equals( exp = 142 act = cut->process_input( input_data )  ).
  ENDMETHOD.

  METHOD result_for_second_part_is_281.
    DATA(cut) = NEW trebuchet( ).
    DATA(input_data) = VALUE stringtab( ( |two1nine| )
                                        ( |eightwothree| )
                                        ( |abcone2threexyz| )
                                        ( |xtwone3four| )
                                        ( |4nineeightseven2| )
                                        ( |zoneight234| )
                                        ( |7pqrstsixteen| ) ).
    cl_abap_unit_assert=>assert_equals( exp = 281 act = cut->process_input( input_data )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(trebuchet) = NEW trebuchet( ).
  WRITE /: |The result of is: { trebuchet->process_input( input_data ) }|.
