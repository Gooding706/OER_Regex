# OER_Regex
A simple nfa based, backtracking evaluated regex implementation in a single header file

# FAQ
  Why use this?
  Don't. There are many regex engines implemented in much more a efficient manner. I made this as a tool for myself as well as to learn how these are implemented.

  Why "OER"?
  The header only nature of this is because of how useful I have found tools like stb_image.h. Thus in homage to those files I have used the same convention of starting the file name with my initials.

  What Regex feature does this have?
  '.' Matches any character
	
  'a*' Matches 0 or more of the previous character or group
	
  'a+' Matches 1 or more of the previous character or group
	
  'a?' Matches 0 or 1 of the previous character or group
	
  'a|b' Matches either of the 2 characters or groups
	
  '\[abc]' Matches any of the given characters

 '\[^abc] Matches any character but the given characters

 '\[a-z0-9A-Z]' Matches any of the given characters within the range

  'a{,}' Is equivalent to 'a*'

  'a{3,5}' Matches a number of the previous character or group within a range
  
  'a{3,}' Matches a minimum number of the previous character or group

  'a{,5}' Matches a maximum number of the previous character or group

  Operators are by default greedy but will be made lazy if followed by '?'

  '()' Is used to capture the inside

  '?: Inside of parentheses makes a group non capturing

  '/' Escapes reserved characters

  '/d' Matches any digit

  '/w' Matches any word character

  '/s' Matches any white space

  '/D' Matches anything but digits

  '/W' Matches anything but word characters

  '/D' Matches anything but white space
  
