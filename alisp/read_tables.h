//
//  read_tables.h
//  alisp
//
//  Created by Matt Gadda on 9/21/11.
//  Copyright (c) 2011 Catalpa Labs. All rights reserved.
//

#ifndef alisp_read_tables_h
#define alisp_read_tables_h

enum SyntaxTypes {
	constituent,	
	terminating_macro_character,
	non_terminating_macro_character,
	single_escape,
	multiple_escape,
	invalid,
	whitespace
};


static const int syntax_type_for_char[255] = {
	invalid, //	\000 Null character
	invalid, //	\001 Start of Header
	invalid, //	\002 Start of Text
	invalid, //	\003 End of Text
	invalid, //	\004 End of Transmission
	invalid, //	\005 Enquiry 
	invalid, //	\006 Acknowledgment
	invalid, //	\a Bell
	constituent, //	\b Backspace
	whitespace, //	\t Horizontal Tab
	whitespace, //	\n Newline
	invalid, //	\v Vertical Tab
	whitespace, //	\f Form feed
	whitespace,// \r Carriage Return
	invalid, // \016 Shift Out
	invalid, // \017 Shift In
	invalid, // \020 Data Link Escape
	invalid, // \021 Device Control 1
	invalid, // \022 Device Control 2
	invalid, // \023 Device Control 3
	invalid, // \024 Device Control 4
	invalid, // \025 Negative Acknowledgement
	invalid, // \026 Synchronous idle
	invalid, // \027 End of Transmission Block
	invalid, // \030 Cancel
	invalid, // \031 End of Medium
	invalid, // \032 Substitute
	invalid, // \e Escape
	invalid, // \034 File Separator
	invalid, // \035 Group Separator
	invalid, // \036 Record Separator
	invalid, // \037 Unit Separator
	whitespace, // Space
	constituent, // !
	terminating_macro_character, // "
	non_terminating_macro_character, //  #
	constituent, // $
	constituent, // %
	constituent, // &
	terminating_macro_character, // '
	terminating_macro_character, // (
	terminating_macro_character, // )
	constituent, // *
	constituent, // +
	terminating_macro_character, // ,
	constituent, // -
	constituent, // .
	constituent, // /
	constituent, // 0
	constituent, // 1
	constituent, // 2
	constituent, // 3
	constituent, // 4
	constituent, // 5
	constituent, // 6
	constituent, // 7
	constituent, // 8
	constituent, // 9
	constituent, // :
	terminating_macro_character, // ;
	constituent, // <
	constituent, // =
	constituent, // >
	constituent, // ?
	constituent, // @
	constituent, // A
	constituent, // B
	constituent, // C
	constituent, // D
	constituent, // E
	constituent, // F
	constituent, // G
	constituent, // H
	constituent, // I
	constituent, // J
	constituent, // K
	constituent, // L
	constituent, // M
	constituent, // N
	constituent, // O
	constituent, // P
	constituent, // Q
	constituent, // R
	constituent, // S
	constituent, // T
	constituent, // U
	constituent, // V
	constituent, // W
	constituent, // X
	constituent, // Y
	constituent, // Z
	constituent, // [
	single_escape, // \\
	constituent,  // ]
	constituent, // ^
	constituent, // _
	terminating_macro_character, // `
	constituent, // a
	constituent, // b
	constituent, // c
	constituent, // d
	constituent, // e
	constituent, // f
	constituent, // g
	constituent, // h
	constituent, // i
	constituent, // j
	constituent, // k
	constituent, // l
	constituent, // m
	constituent, // n
	constituent, // o
	constituent, // p
	constituent, // q
	constituent, // r
	constituent, // s
	constituent, // t
	constituent, // u
	constituent, // v
	constituent, // w
	constituent, // x
	constituent, // y
	constituent, // z
	constituent, // {
	multiple_escape, // |
	constituent, // }
	constituent, // ~	
	constituent // Rubout	
};

enum ConstituantCharacterTraits {
	alphabetic = 1,
	alphadigit = 2,
	package_marker = 4,
	plus_sign = 8,
	minus_sign = 16,
	dot = 32,
	decimal_point = 64,
	ratio_marker = 128,
	exponent_marker = 256,
	double_float = 512,
	single_float = 1024
};

static const int character_traits_for_char[255] = {
	invalid, //	\000 Null character
	invalid, //	\001 Start of Header
	invalid, //	\002 Start of Text
	invalid, //	\003 End of Text
	invalid, //	\004 End of Transmission
	invalid, //	\005 Enquiry 
	invalid, //	\006 Acknowledgment
	invalid, //	\a Bell
	invalid, //	\b Backspace
	invalid, //	\t Horizontal Tab
	invalid, //	\n Newline
	invalid, //	\v Vertical Tab
	invalid, //	\f Form feed
	invalid,// \r Carriage Return
	invalid, // \016 Shift Out
	invalid, // \017 Shift In
	invalid, // \020 Data Link Escape
	invalid, // \021 Device Control 1
	invalid, // \022 Device Control 2
	invalid, // \023 Device Control 3
	invalid, // \024 Device Control 4
	invalid, // \025 Negative Acknowledgement
	invalid, // \026 Synchronous idle
	invalid, // \027 End of Transmission Block
	invalid, // \030 Cancel
	invalid, // \031 End of Medium
	invalid, // \032 Substitute
	invalid, // \e Escape
	invalid, // \034 File Separator
	invalid, // \035 Group Separator
	invalid, // \036 Record Separator
	invalid, // \037 Unit Separator
	invalid, // Space
	alphabetic, // !
	alphabetic, // "
	alphabetic, //  #
	alphabetic, // $
	alphabetic, // %
	alphabetic, // &
	alphabetic, // '
	alphabetic, // (
	alphabetic, // )
	alphabetic, // *
	alphabetic | plus_sign, // + 
	alphabetic, // , 
	alphabetic | minus_sign, // - 
	alphabetic | dot | decimal_point, // .
	alphabetic, // /
	alphadigit, // 0
	alphadigit, // 1
	alphadigit, // 2
	alphadigit, // 3
	alphadigit, // 4
	alphadigit, // 5
	alphadigit, // 6
	alphadigit, // 7
	alphadigit, // 8
	alphadigit, // 9
	package_marker, // :
	alphabetic, // ;
	alphabetic, // <
	alphabetic, // =
	alphabetic, // >
	alphabetic, // ?
	alphabetic, // @
	constituent, // A
	constituent, // B
	constituent, // C
	constituent | double_float | exponent_marker, // D
	constituent, // E
	constituent | single_float | exponent_marker, // F
	constituent, // G
	constituent, // H
	constituent, // I
	constituent, // J
	constituent, // K
	constituent, // L
	constituent, // M
	constituent, // N
	constituent, // O
	constituent, // P
	constituent, // Q
	constituent, // R
	constituent, // S
	constituent, // T
	constituent, // U
	constituent, // V
	constituent, // W
	constituent, // X
	constituent, // Y
	constituent, // Z
	alphabetic, // [
	alphabetic | ratio_marker, // \\
	alphabetic,  // ]
	alphabetic, // ^
	alphabetic, // _
	alphabetic, // `
	constituent, // a
	constituent, // b
	constituent, // c
	constituent | double_float | exponent_marker, // d
	constituent, // e
	constituent | single_float | exponent_marker, // f
	constituent, // g
	constituent, // h
	constituent, // i
	constituent, // j
	constituent, // k
	constituent, // l
	constituent, // m
	constituent, // n
	constituent, // o
	constituent, // p
	constituent, // q
	constituent, // r
	constituent, // s
	constituent, // t
	constituent, // u
	constituent, // v
	constituent, // w
	constituent, // x
	constituent, // y
	constituent, // z
	alphabetic, // {
	alphabetic, // |
	alphabetic, // }
	alphabetic, // ~	
	invalid // Rubout	
};

#endif
