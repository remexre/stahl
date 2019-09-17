bits 64

%include "src/amd64/macros.inc"
%define last_defined_word forth_last_builtin

extern no_code_field
extern undefined_word
extern forth_docolon.impl
extern forth_exit.cfa
extern forth_last_builtin

global forth_last_pseudobuiltin

[section .rodata]

defcolon align, "ALIGN"
	word here
	lit 7
	word add
	lit 7
	word invert
	word and
	word here
	word sub
	word allot
endcolon

defcolon comma, ","
	word here
	lit 8
	word allot
	word store
endcolon

defcolon comma_char, "C,"
	word here
	lit 1
	word allot
	word store_char
endcolon

defcolon comma_dword, "D,"
	word here
	lit 4
	word allot
	word store_dword
endcolon

defcolon comma_str, "S,"
	word dup
	wordl comma_char

	word to_r
	lit 0
	word to_r

.loop:
	word from_r_2
	wordl dup2
	wordl not_equal
	wordl if_impl
	dq .end
	word incr
	word to_r_2

	word dup
	word fetch_char
	wordl comma_char
	word incr

	wordl jump_impl
	dq .loop

.end:
	wordl drop2
	wordl drop
endcolon

defcolon comma_word, "W,"
	word here
	lit 2
	word allot
	word store_word
endcolon

defcolon comment, "\", 0x01
	wordl source_rest
	lit forth_is_nl.cfa
	wordl string_find_pred
	word to_in
	word add_store
endcolon

defcolon compile_comma, "COMPILE,"
	word comma
endcolon

defcolon count, "COUNT"
	word dup
	word incr
	word swap
	word fetch_char
endcolon

defcolon create, "CREATE"
	wordl parse_name

	; Next Link
	word here
	wordl latest
	word comma
	wordl dict_head
	word store

	; Documentation
	lit 0
	wordl comma

	; Flags
	lit 0
	wordl comma_char

	; Name
	wordl comma_str

	; Code Field
	lit 0xe9
	wordl comma_char
	word here
	lit no_code_field-4
	word swap
	word sub
	wordl comma_dword
endcolon

defcolon create_noname, "CREATE-NONAME"
	; Next Link
	word here
	wordl latest
	word comma
	wordl dict_head
	word store

	; Documentation
	lit 0
	wordl comma

	; Flags
	lit 0
	wordl comma_char

	; Name
	lit 0
	wordl comma_char

	; Code Field
	word here
	lit 0xe9
	wordl comma_char
	word here
	lit no_code_field-4
	word swap
	word sub
	wordl comma_dword
endcolon

defcolon dict_head, "DICT-HEAD"
	word process_pointer
	lit 0x20
	word add
endcolon

defcolon does_enter, "DOES>ENTER"
	wordl latest
	wordl header_to_cfa
	word incr
	word dup
	lit forth_docolon.impl-4
	word swap
	word sub
	word swap
	wordl store_dword
endcolon

defcolon drop2, "2DROP"
	word drop
	word drop
endcolon

defcolon dup2, "2DUP"
	wordl over
	wordl over
endcolon

defcolon evaluate, "EVALUATE"
	; Save the old source.
	wordl source
	word to_in
	word fetch
	word to_r
	word to_r
	word to_r

	; Set the new source.
	word source_length
	word store
	word source_buffer
	word store

	; Reset the input position to 0.
	lit 0
	word to_in
	word store

	; Interpret the source.
	wordl interpret

	; Restore the old source.
	word from_r
	word source_buffer
	word store
	word from_r
	word source_length
	word store
	word from_r
	word to_in
	word store
endcolon

defcolon find_header, "FIND-HEADER"
	wordl dict_head
	word to_r

.loop:
	word from_r
	word fetch
	word dup
	word to_r

	word if_impl
	dq .end

	wordl dup2

	word r_fetch
	wordl header_to_name
	wordl count

	word streq
	word if_impl
	dq .loop

.end:
	word drop
	word drop
	word from_r
endcolon

defcolon header_to_cfa, "HEADER>CFA"
	wordl header_to_name
	wordl count
	wordl add
endcolon

defcolon header_to_name, "HEADER>NAME"
	lit 17
	word add
endcolon

defcolon immediate, "IMMEDIATE"
	wordl latest
	lit 16
	word add
	word dup
	word fetch_char
	lit 1
	word or
	word swap
	word store_char
endcolon

defcolon interpret, "INTERPRET"
.loop:
	wordl parse_name
	word dup

	word if_impl
	dq .end

	wordl dup2
	wordl find_header
	word dup_nonzero

	word if_impl
	dq .not_found

	word rev_rot
	wordl drop2

	word dup
	wordl is_immediate
	word get_state
	word invert
	word or

	word if_impl
	dq .compile

	wordl header_to_cfa
	word execute
	word jump_impl
	dq .loop

.compile:
	wordl header_to_cfa
	wordl compile_comma
	word jump_impl
	dq .loop

.not_found:
	wordl dup2
	word to_number
	word if_impl
	dq .undefined

	word rev_rot
	wordl drop2

	word get_state
	word if_impl
	dq .loop

	lit forth_literal_impl.cfa
	wordl compile_comma
	wordl comma

	word jump_impl
	dq .loop

.undefined:
	dq undefined_word

.end:
	wordl drop2
endcolon

defcolon is_immediate, "IMMEDIATE?"
	lit 16
	word add
	word fetch_char
	lit 0
	word test_flag
endcolon

defcolon is_nl, "IS-NL?"
	lit `\n`
	word equal
endcolon

defcolon is_space, "IS-SPACE?"
	lit ' '
	word incr
	word u_less
endcolon

defcolon isnt_space, "ISNT-SPACE?"
	lit ' '
	word u_greater
endcolon

defcolon latest, "LATEST"
	wordl dict_head
	word fetch
endcolon

defcolon not_equal, "<>"
	word equal
	word invert
endcolon

defcolon over, "OVER"
	word to_r
	word dup
	word from_r
	word swap
endcolon

defcolon parse_name, "PARSE-NAME"
	wordl source_skip_spaces
	wordl source_rest
	wordl over
	wordl swap
	lit forth_is_space.cfa
	wordl string_find_pred
	word dup
	word to_in
	word add_store
endcolon

defcolon source, "SOURCE"
	word source_buffer
	word fetch
	word source_length
	word fetch
endcolon

defcolon source_rest, "SOURCE-REST"
	wordl source
	word to_in
	word fetch
	word adjust_string
endcolon

defcolon source_skip_spaces, "SOURCE-SKIP-SPACES"
	wordl source_rest
	lit forth_isnt_space.cfa
	wordl string_find_pred
	word to_in
	word add_store
endcolon

defcolon string_find_pred, "STRING-FIND-PRED"
	; ( addr len xt -- pos ), pos == len if none found
	word to_r
	word to_r
	word dup
	word from_r
	word over
	word to_r
	word add
	word from_r

.loop:
	wordl dup2
	wordl not_equal
	word if_impl
	dq .end

	word dup
	word fetch_char

	word r_fetch
	word execute
	word invert
	word if_impl
	dq .end

	word incr

	word jump_impl
	dq .loop

.end:
	word swap
	word from_r
	word drop2
	word swap
	word sub
endcolon

; This is a no-name no-op word, as a marker and safety guard.
defcode last_pseudobuiltin, ""
endcode

; vi: cc=80 ft=nasm
