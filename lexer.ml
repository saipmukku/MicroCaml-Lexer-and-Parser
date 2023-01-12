open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = let strLength = String.length input in

  let rec tok pos =

    if pos > strLength then [] else

      if Str.string_match (Str.regexp "[0-9]+") input pos then
        let m_string = Str.matched_string input in
        let actual_int = int_of_string m_string in
        
        Tok_Int (actual_int) :: (tok (pos + String.length m_string))

      else if Str.string_match (Str.regexp "(-[0-9]+)") input pos then
        let m_string = Str.matched_string input in
        let actual_int = int_of_string m_string in
        
        Tok_Int (actual_int) :: (tok (pos + String.length m_string))

      else if Str.string_match (Str.regexp "->") input pos then
        Tok_Arrow :: (tok (pos + 2))

      else if Str.string_match (Str.regexp "(") input pos then
        Tok_LParen :: (tok (pos + 1))

      else if Str.string_match (Str.regexp ")") input pos then
        Tok_RParen :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "=") input pos then
        Tok_Equal :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "<>") input pos then
        Tok_NotEqual :: (tok (pos + 2))

      else if Str.string_match (Str.regexp ">") input pos then
        Tok_Greater :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "<") input pos then
        Tok_Less :: (tok (pos + 1))

      else if Str.string_match (Str.regexp ">=") input pos then
        Tok_GreaterEqual :: (tok (pos + 2))

      else if Str.string_match (Str.regexp "<=") input pos then
        Tok_LessEqual :: (tok (pos + 2))
        
      else if Str.string_match (Str.regexp "||") input pos then
        Tok_Or :: (tok (pos + 2))
        
      else if Str.string_match (Str.regexp "&&") input pos then
        Tok_And :: (tok (pos + 2))

      else if Str.string_match (Str.regexp "not") input pos then
        Tok_Not :: (tok (pos + 3))

      else if Str.string_match (Str.regexp "if") input pos then
        Tok_If :: (tok (pos + 2))

      else if Str.string_match (Str.regexp "then") input pos then
        Tok_Then :: (tok (pos + 4))

      else if Str.string_match (Str.regexp "else") input pos then
        Tok_Else :: (tok (pos + 4))

      else if Str.string_match (Str.regexp "\\+") input pos then
        Tok_Add :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "\\-") input pos then
        Tok_Sub :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "\\*") input pos then
        Tok_Mult :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "\\/") input pos then
        Tok_Div :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "\\^") input pos then
        Tok_Concat :: (tok (pos + 1))

      else if Str.string_match (Str.regexp "let") input pos then
        Tok_Let :: (tok (pos + 3))

      else if Str.string_match (Str.regexp "def") input pos then
        Tok_Def :: (tok (pos + 3))

      else if Str.string_match (Str.regexp "in") input pos then
        Tok_In :: (tok (pos + 2))

      else if Str.string_match (Str.regexp "rec") input pos then
        Tok_Rec :: (tok (pos + 3))

      else if Str.string_match (Str.regexp "fun") input pos then
        Tok_Fun :: (tok (pos + 3))

      else if Str.string_match (Str.regexp ";;") input pos then
        Tok_DoubleSemi :: (tok (pos + 2))

      else if Str.string_match (Str.regexp {|true\|false|}) input pos then
        let m_string = Str.matched_string input in
        let bool_string = bool_of_string m_string in
        Tok_Bool (bool_string) :: (tok (pos + String.length m_string))
    
      else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
        let split_string = String.split_on_char '\"' (Str.matched_string input) in
        let string_loc = List.length split_string / 2 in
        let final_string = List.nth split_string string_loc in
        Tok_String (final_string) :: (tok (pos + String.length final_string + 2))
    
      else if Str.string_match (Str.regexp {|[a-zA-Z][a-zA-Z0-9]*|}) input pos then
        let m_string = Str.matched_string input in
        Tok_ID (m_string) :: (tok (pos + String.length m_string))

      else tok (pos + 1)

  in tok 0;;