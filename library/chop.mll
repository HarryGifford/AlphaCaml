rule chop = parse
| ( [ '0'-'9' ]+ "__" )* (_* as suffix) eof
    { suffix }

