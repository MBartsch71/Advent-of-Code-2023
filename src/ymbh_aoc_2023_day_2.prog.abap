REPORT ymbh_aoc_2023_day_2.

CLASS game_detector DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF draw_detail,
             color  TYPE string,
             amount TYPE i,
           END OF draw_detail.
    TYPES draw_details TYPE STANDARD TABLE OF draw_detail WITH EMPTY KEY.

    TYPES: BEGIN OF draw,
             draw_nr            TYPE i,
             draw_details       TYPE draw_details,
             draw_limit_overrun TYPE abap_bool,
           END OF draw.
    TYPES draws TYPE STANDARD TABLE OF draw WITH EMPTY KEY.

    TYPES: BEGIN OF game_round,
             game                 TYPE string,
             draws                TYPE draws,
             over_limit           TYPE abap_bool,
             minimum_color_amount TYPE draw_details,
             power_of_cubes       TYPE i,
           END OF game_round.
    TYPES game_rounds TYPE STANDARD TABLE OF game_round WITH EMPTY KEY.

    TYPES: BEGIN OF limit,
             color TYPE string,
             limit TYPE i,
           END OF limit.
    TYPES limits TYPE STANDARD TABLE OF limit WITH EMPTY KEY.

    METHODS get_game RETURNING VALUE(result) TYPE string.

    METHODS investigate IMPORTING input_line    TYPE string
                        RETURNING VALUE(result) TYPE game_round.

    METHODS get_game_record RETURNING VALUE(result) TYPE game_round.

    METHODS run IMPORTING input_values TYPE stringtab.

    METHODS get_game_records RETURNING VALUE(result) TYPE game_rounds.

    METHODS set_limits IMPORTING limits TYPE limits.

    METHODS get_sum_of_valid_ids RETURNING VALUE(result) TYPE i.

    METHODS build_minimum_color_stats.

    METHODS get_sum_of_power_of_cubes RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA game_nr TYPE char5.
    DATA game_draws TYPE draws.
    DATA game_record TYPE game_round.
    DATA game_records TYPE game_rounds.
    DATA game_limits TYPE limits.

    METHODS extract_game_number IMPORTING input_line TYPE string.
    METHODS extract_draw_record IMPORTING input_line TYPE string.
    METHODS build_game_record.

    METHODS initialize_color_stats RETURNING VALUE(r_color_stats) TYPE game_detector=>draw_details.

ENDCLASS.

CLASS game_detector IMPLEMENTATION.

  METHOD get_game.
    result = game_nr.
  ENDMETHOD.

  METHOD get_game_record.
    result = game_record.
  ENDMETHOD.

  METHOD investigate.
    extract_game_number( input_line ).
    extract_draw_record( input_line ).
    build_game_record( ).
  ENDMETHOD.

  METHOD extract_game_number.
    SPLIT input_line AT ':' INTO  DATA(game) DATA(draw_record).
    DATA(split_game_expression) = ycl_mbh_string=>new( game )->split_string( 5 ).
    game_nr = split_game_expression[ 2 ].
  ENDMETHOD.

  METHOD extract_draw_record.
    DATA draw_details_tab TYPE draw_details.
    DATA draws_tab TYPE draws.
    DATA counter TYPE i.
    DATA game_limit_overflow TYPE abap_bool.

    SPLIT input_line AT ': ' INTO DATA(game) DATA(draw_record).
    SPLIT draw_record AT '; ' INTO TABLE DATA(temp_draws).

    LOOP AT temp_draws REFERENCE INTO DATA(temp_draw).
      counter += 1.
      SPLIT temp_draw->* AT ', ' INTO TABLE DATA(colors).
      LOOP AT colors REFERENCE INTO DATA(color).
        SPLIT color->* AT space INTO DATA(draw_amount) DATA(draw_color).
        draw_details_tab = VALUE #( BASE draw_details_tab ( color  = draw_color
                                                            amount = draw_amount ) ).
      ENDLOOP.
      LOOP AT draw_details_tab REFERENCE INTO DATA(draw_detail).
        TRY.
            DATA(limit_entry) = VALUE #( game_limits[ color = draw_detail->color ] ).
            IF limit_entry-limit < draw_detail->amount.
              game_limit_overflow = abap_true.
              EXIT.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.
      ENDLOOP.

      draws_tab = VALUE draws( BASE draws_tab ( draw_nr = counter draw_details = draw_details_tab draw_limit_overrun = game_limit_overflow ) ).
      CLEAR draw_details_tab.
      CLEAR game_limit_overflow.
    ENDLOOP.

    game_draws = draws_tab.
  ENDMETHOD.

  METHOD build_game_record.
    DATA game_over_limit TYPE abap_bool.
    IF line_exists( game_draws[ draw_limit_overrun = abap_true ] ).
      game_over_limit = abap_true.
    ENDIF.
    game_record = VALUE #( game = game_nr draws = game_draws over_limit = game_over_limit ).
  ENDMETHOD.

  METHOD run.
    LOOP AT input_values REFERENCE INTO DATA(line).
      investigate( line->* ).
      game_records = VALUE game_rounds( BASE game_records ( game_record ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_game_records.
    result = game_records.
  ENDMETHOD.

  METHOD set_limits.
    game_limits = limits.
  ENDMETHOD.

  METHOD get_sum_of_valid_ids.
    result = REDUCE #( INIT sum = 0
                       FOR line IN game_records
                       NEXT sum = sum + COND #( WHEN line-over_limit IS INITIAL THEN line-game
                                                ELSE 0 ) ).
  ENDMETHOD.


  METHOD build_minimum_color_stats.
    DATA color_stats TYPE game_detector=>draw_details.

    color_stats = initialize_color_stats( ).
    LOOP AT game_records REFERENCE INTO DATA(game_record).
      LOOP AT game_record->draws REFERENCE INTO DATA(draw).
        LOOP AT draw->draw_details REFERENCE INTO DATA(detail).
          ASSIGN color_stats[ color = detail->color ] TO FIELD-SYMBOL(<color_stat>).
          IF sy-subrc = 0.
            <color_stat>-amount = COND #( WHEN <color_stat>-amount < detail->amount
                                            THEN detail->amount
                                            ELSE <color_stat>-amount ).
          ENDIF.
          UNASSIGN <color_stat>.
        ENDLOOP.
      ENDLOOP.
      game_record->minimum_color_amount = color_stats.
      game_record->power_of_cubes = REDUCE #( INIT power = 1
                                              FOR line IN color_stats
                                              NEXT power = power * line-amount ).
      color_stats = initialize_color_stats( ).
    ENDLOOP.

  ENDMETHOD.

  METHOD initialize_color_stats.

    r_color_stats  = VALUE draw_details( ( color = |blue|  amount = 0 )
                                           ( color = |red|   amount = 0 )
                                           ( color = |green| amount = 0 ) ).


  ENDMETHOD.

  METHOD get_sum_of_power_of_cubes.
    result = REDUCE #( INIT sum = 0
                       FOR line IN game_records
                       NEXT sum = sum + line-power_of_cubes ).
  ENDMETHOD.

ENDCLASS.

CLASS test_game_detector DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF draw_detail,
             color  TYPE string,
             amount TYPE i,
           END OF draw_detail.
    TYPES draw_details TYPE STANDARD TABLE OF draw_detail WITH EMPTY KEY.

    TYPES: BEGIN OF draw,
             draw_nr            TYPE i,
             draw_details       TYPE draw_details,
             draw_limit_overrun TYPE abap_bool,
           END OF draw.
    TYPES draws TYPE STANDARD TABLE OF draw WITH EMPTY KEY.

    TYPES: BEGIN OF game_round,
             game                 TYPE string,
             draws                TYPE draws,
             over_limit           TYPE abap_bool,
             minimum_color_amount TYPE draw_details,
             power_of_cubes       TYPE i,
           END OF game_round.
    TYPES game_rounds TYPE STANDARD TABLE OF game_round WITH EMPTY KEY.
    DATA cut TYPE REF TO game_detector.

    METHODS setup.
    METHODS build_input_values RETURNING VALUE(result) TYPE stringtab.
    METHODS build_expected_values RETURNING VALUE(result) TYPE test_game_detector=>game_rounds.

    METHODS get_games_inside_limit FOR TESTING.
    METHODS get_sum_of_valid_ids FOR TESTING.
    METHODS get_sum_of_power_of_cubes FOR TESTING.

ENDCLASS.

CLASS test_game_detector IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD build_expected_values.
    result  = VALUE game_rounds( ( game = |1|
                                   draws = VALUE #( ( draw_nr            = 1
                                                      draw_details       = VALUE #( ( color = |blue| amount = 3 )
                                                                                    ( color = |red|  amount = 4 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 2 draw_details = VALUE #( ( color = |red|  amount = 1 )
                                                                                          ( color = |green| amount = 2 )
                                                                                          ( color = |blue|  amount = 6 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 3 draw_details = VALUE #( ( color = |green| amount = 2 ) ) ) )
                                   over_limit = ||
                                   minimum_color_amount = VALUE #( ( color = |blue|  amount = 6 )
                                                                   ( color = |red|   amount = 4 )
                                                                   ( color = |green| amount = 2 ) )
                                   power_of_cubes = 48 )


                                 ( game = |2|
                                   draws = VALUE #( ( draw_nr = 1 draw_details = VALUE #( ( color = |blue|  amount = 1 )
                                                                                          ( color = |green| amount = 2 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 2 draw_details = VALUE #( ( color = |green| amount = 3 )
                                                                                          ( color = |blue|  amount = 4 )
                                                                                          ( color = |red|   amount = 1 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 3 draw_details = VALUE #( ( color = |green| amount = 1 )
                                                                                          ( color = |blue|  amount = 1 ) )
                                                      draw_limit_overrun = || ) )
                                   over_limit = ||
                                   minimum_color_amount = VALUE #( ( color = |blue|  amount = 4 )
                                                                   ( color = |red|   amount = 1 )
                                                                   ( color = |green| amount = 3 ) )
                                   power_of_cubes = 12 )

                                 ( game = |3|
                                   draws = VALUE #( ( draw_nr = 1 draw_details = VALUE #( ( color = |green| amount = 8 )
                                                                                          ( color = |blue|  amount = 6 )
                                                                                          ( color = |red|   amount = 20 ) )
                                                      draw_limit_overrun = |X| )

                                                    ( draw_nr = 2 draw_details = VALUE #( ( color = |blue|  amount = 5 )
                                                                                          ( color = |red|   amount = 4 )
                                                                                          ( color = |green| amount = 13 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 3 draw_details = VALUE #( ( color = |green| amount = 5 )
                                                                                          ( color = |red|   amount = 1 ) )
                                                      draw_limit_overrun = || ) )
                                   over_limit = |X|
                                   minimum_color_amount = VALUE #( ( color = |blue|  amount = 6 )
                                                                   ( color = |red|   amount = 20 )
                                                                   ( color = |green| amount = 13 ) )
                                   power_of_cubes = 1560 )

                                 ( game = |4|
                                   draws = VALUE #( ( draw_nr = 1 draw_details = VALUE #( ( color = |green| amount = 1 )
                                                                                          ( color = |red|   amount = 3 )
                                                                                          ( color = |blue|  amount = 6 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 2 draw_details = VALUE #( ( color = |green| amount = 3 )
                                                                                          ( color = |red|   amount = 6 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 3 draw_details = VALUE #( ( color = |green| amount = 3 )
                                                                                          ( color = |blue|  amount = 15 )
                                                                                          ( color = |red|   amount = 14 ) )
                                                      draw_limit_overrun = |X| ) )
                                   over_limit = |X|
                                   minimum_color_amount = VALUE #( ( color = |blue|  amount = 15 )
                                                                   ( color = |red|   amount = 14 )
                                                                   ( color = |green| amount = 3 ) )
                                   power_of_cubes = 630 )

                                 ( game = |5|
                                   draws = VALUE #( ( draw_nr = 1 draw_details = VALUE #( ( color = |red|   amount = 6 )
                                                                                          ( color = |blue|  amount = 1 )
                                                                                          ( color = |green| amount = 3 ) )
                                                      draw_limit_overrun = || )

                                                    ( draw_nr = 2 draw_details = VALUE #( ( color = |blue|  amount = 2 )
                                                                                          ( color = |red|   amount = 1 )
                                                                                          ( color = |green| amount = 2 ) )
                                                      draw_limit_overrun = || ) )
                                   over_limit = ||
                                   minimum_color_amount = VALUE #( ( color = |blue|  amount = 2 )
                                                                   ( color = |red|   amount = 6 )
                                                                   ( color = |green| amount = 3 ) )
                                   power_of_cubes = 36 ) ).
  ENDMETHOD.

  METHOD build_input_values.
    result  = VALUE stringtab( ( |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green| )
                               ( |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue| )
                               ( |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red| )
                               ( |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red| )
                               ( |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green| ) ).
  ENDMETHOD.

  METHOD get_games_inside_limit.
    cut->set_limits( VALUE #( ( color = |red|   limit = 12 )
                              ( color = |blue|  limit = 14 )
                              ( color = |green| limit = 13 ) ) ).
    cut->run( build_input_values( ) ).
    cut->build_minimum_color_stats( ).
    DATA(expected_values) = build_expected_values( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_game_records( ) ).
  ENDMETHOD.

  METHOD get_sum_of_valid_ids.
    cut->set_limits( VALUE #( ( color = |red| limit = 12 )
                              ( color = |blue| limit = 14 )
                              ( color = |red|  limit = 13 ) ) ).
    cut->run( build_input_values( ) ).
    cut->build_minimum_color_stats( ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = cut->get_sum_of_valid_ids( ) ).
  ENDMETHOD.



  METHOD get_sum_of_power_of_cubes.
    cut->set_limits( VALUE #( ( color = |red| limit = 12 )
                              ( color = |blue| limit = 14 )
                              ( color = |red|  limit = 13 ) ) ).
    cut->run( build_input_values( ) ).
    cut->build_minimum_color_stats( ).
    cl_abap_unit_assert=>assert_equals( exp = 2286 act = cut->get_sum_of_power_of_cubes( )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(game) = NEW game_detector( ).
  game->set_limits( VALUE #( ( color = |red|   limit = 12 )
                             ( color = |green| limit = 13 )
                             ( color = |blue|  limit = 14 ) ) ).
  game->run( input_data ).
  game->build_minimum_color_stats( ).
  WRITE /: |The result of part 1 is: { game->get_sum_of_valid_ids( ) }|.
  WRITE /: |The result of part 2 is: { game->get_sum_of_power_of_cubes( ) }|.
