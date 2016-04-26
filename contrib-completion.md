## Quick instructions

1. Initiate completion with one of the standard methods.

1. What you get is the method call with discreet underscore placeholders
for each argument:

    ![Right after completion](http://i.imgur.com/r8qovde.png)

    Whenever cursor is on a placeholder, help text in the echo are (like
for problems) shows that argument in position. Cursor starts on the first
one.

1. Typing over a placeholder erases it:

    ![Type over first arg](http://i.imgur.com/h09TtiR.png)
    
1. Moving to the next placeholder shifts the help text:
    
    ![Help text 2nd arg](http://i.imgur.com/5qQAQbR.png)

1. If you need a more elaborate reminder (might have been a while to write
the first arg, hit `?` or `f1` for javadoc on the method/type that the
placeholder belongs to.

    ![Pull up javadoc](http://i.imgur.com/Enn1w9W.png)

1. Once done with placeholders, there is no trace of them left in the
buffer: no variables or overlay or keybinding; definitely no minor mode.



## Remarks

The placeholders are exactly what they seem to be: underscores.

They happen to be *syntactically* correct everywhere (types or values), so
Eclipse won't get very confused, even if you go do something else before
filling them all. The placeholders also help it distinguish between method
variants when it comes to e.g. offering "new " completion based on the
type of the argument.

The underscores are easy to find and come back to in a typical Java program
(i.e. without underscores). There is I suppose the theoretical chance that
one left in by mistake would conflict (and match type) with a variable
named "_" in some exotic java style, but the advantages of giving them to
Eclipse on occasion to have well formed syntax are too great.

Other than them disappearing the first time they're typed over, there's no
other magic, they're just text with a few properties (slightly shaded, for
example).

In terms of magic, perhaps electric comma would be nice (move to
next argument when hit comma) but I don't think avoiding two forward
strokes is worth any hassle. By no means would I want a keybinding
that's either global or active while writing the arg (including TAB),
although I could easily implement one for the user.

## Also considered

Placeholders that stayed in front of the cursor until deleted, plus TAB
keymapping on the placeholder itself to get rid of the placeholder and jump
to next one. Having a character always on the cursor proved slightly
annoying in use.

Spaces -- Eclipse was too easily confused by the bad syntax (remember that
a partially filled yas has this problem too). Same for the ellipsis
character, which it treats the same as a space.

Nothing at all -- just complete the method name; perhaps use a decorated
space char in front of the cursor for help. Visually lightweight (nothing,
really), but what we lose is the display and knowledge of which method we
selected during completion. The +1 or +2 argument overloads seem to be very
common.

Other colors for the underscores -- that's just the default for a
customization at this point. I like the current `shadow` or also no
colorization at all; any colors proved too distracting.
