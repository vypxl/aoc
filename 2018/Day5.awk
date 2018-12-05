#! /usr/bin/env -S sh -c "awk -f \$0 5.in"
{
    best=f($0)
    print "Solution for part 1:"
    print best

    split("abcdefghijklmnopqrstuvwxyz",a,"")
    for(i in a){
        IGNORECASE=1
        r=f(gensub(a[i],"","g"))
        if(r<best) best=r
    }
    print "Solution for part 2:"
    print best
}

function f(x){
    IGNORECASE=0
    p="aA|Aa|bB|Bb|cC|Cc|dD|Dd|eE|Ee|fF|Ff|gG|Gg|hH|Hh|iI|Ii|jJ|Jj|kK|Kk|lL|Ll|mM|Mm|nN|Nn|oO|Oo|pP|Pp|qQ|Qq|rR|Rr|sS|Ss|tT|Tt|uU|Uu|vV|Vv|wW|Ww|xX|Xx|yY|Yy|zZ|Zz"
    while(x~p) gsub(p,"",x)
    return length(x)
}

# If you want, use this ^^
# $ awk '{b=f($0);print b;split("abcdefghijklmnopqrstuvwxyz",a,"");for(i in a){IGNORECASE=1;r=f(gensub(a[i],"","g"));if(r<b) b=r}print b}function f(x){IGNORECASE=0;p="aA|Aa|bB|Bb|cC|Cc|dD|Dd|eE|Ee|fF|Ff|gG|Gg|hH|Hh|iI|Ii|jJ|Jj|kK|Kk|lL|Ll|mM|Mm|nN|Nn|oO|Oo|pP|Pp|qQ|Qq|rR|Rr|sS|Ss|tT|Tt|uU|Uu|vV|Vv|wW|Ww|xX|Xx|yY|Yy|zZ|Zz";while(x~p)gsub(p,"",x);return length(x)}' < 5.in

# Solution part 1: 9296
# Solution part 2: 5534
