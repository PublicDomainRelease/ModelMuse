XYGRAPH Delphi Unit 
Version 2.3, july 2006 
See revised copyright notes in XYGRAPH.PAS

You should find the following 19 files:

- readme.txt    : this file

- xygraph.pas   |
- xygraph3d.pas | the Deplhi source files
- xycommon.pas  |
- xygraph4d.pas |

- xygraph.hlp   |
- xygraph.cnt   | help files for xygraph

- xytest.dpr    |
- xytest.dof    |
- xytest.res    | a sample program showing many 2D features of XYGRAPH
- xyun.pas      |
- xyun.dfm      |

- xytest3d.dpr  |
- xytest3d.dof  |
- xytest3d.res  | a sample program showing many 3D/4D features of XYGRAPH
- xyun3d.pas    |
- xyun3d.dfm    |

- xycopy.pas    | a utility to facilitate copying the graph
- xycopy.dfm    |

INSTALLATION

Copy all files except for the help files into the Deplhi Projects folder.
The helpfiles do not integrate with Delphi Help, so you can copy them
anywere you want.

If everything has been copied correctly the projects XYTEST and XYTEST3D
should compile without problems. Note that the unit XYCOPY can be used in
other programs as well.

COMPATIBILITY

XYGRAPH works on version 4 and later.  
(feedback appreciated).

Note that all the .DFM files are saved in binary format for compatibility
with Delphi version 4. To save in text format (versions >4):
- right-click the form, set: Text DFM

NOTES FOR C++ USERS

1 - Please note that Delphi like is predecessor Pascal is case insensitive.
I have tried to be consistent in the use of lower and upper case but I cannot
guarantee that completely.

2 - Also note that Delphi units may have initialization codes, a piece of code
that is executed at program start-up and that is placed at the very end of the 
unit. I have combined all initialization into a single routine INITYXYGRAPH 
that you must call at program start-up. There is an autodetection that executes 
this routine in case you forget but that may not be always full proof.

SUPPORT

As a matter of principle I will respond to all e-mail.
I do take comments from users very seriously and will try to follow their
suggestions as long as they are within the scope of XYGRAPH (and not too 
complicated). If you have problems with the use of XYGRAPH I will help you
but I am not going to write your application for you.
However keep in mind that I am doing this as a hobby and not for a living
and have other obligations as well, I cannot always respond overnight.

CONTACT

Please resist the temptation of making alterations to XYGRAPH yourself:
- changing someone else's program is very hazardous as you often cannot
  forsee the consequences
- when you download future versions of XYGRAPH you have to do it all
  over again
- you deny your fellow programmers the benefits of your ideas

Contact me for implying improvements or extensions instead. I am happy to
do it for you, in fact many extensions are actually based om comments from
users.

Inform me also of any bugs you find. I use XYGRAPH for my own applications
and as such have tested it extensively. However I cannot test every
possible combination of conditions so some bugs may still be present.

Also contact me if you are just satisfied.
NOTE: For all mails: please mention 'xygraph' clearly in the mail's subject
header, otherwise I might think it is Spam and will delete it unread.

--  wcemmens@solcon.nl

Visit my website for regular upgrades.

--  www.solcon.nl/wcemmens

Also inform me if you want to receive any XYGRAPH mailing.

HISTORY

1.0 sept 2002: first publication with DCU file
1.1 oct  2002: supply of PAS file, typing errors in HLP file corrected
1.2 may  2003: several extensions (drawfunction, setratio, setreduction, 
               print and other)
1.3 aug  2003: more extensions (high-res plotting, errorbar, dashed lines,
               data retrieval and more) 
2.0 dec  2003: implementation of 3D plotting, textangle
2.1 aug  2004: all types of polar plots, red/green stereo, timeaxis
2.2 apr  2005: 4D plotting, 3D labels, missing data, array plot

ACKNOWLEDGEMENTS

Thanks to:
- Alan Sun
- Martin Znidarsic 
- Joe Abramson
- Pieter Jan van Bommel
- Alex Mokrov
- Dominik Schmidt
- Peter Streipardt
- Denny Carlton
- Adrian Ilie
- Neluta Ibris
- Olivier Muet
- Olivier Touzot
- Wim Neimeijer
- Bjorn Liebich
- Christian Schneider
- Dennis Parygin
- Koen Eijsvogels
- Frederic Ott
- Frederic Viricel
- Rafal Treffler
- Nicolas Rousselon
- Klaus Krug
- Thomas Harnisch
for comments and suggestions (or reporting bugs).

Textangle routine by Claus Ziegler (www.zieglersoft.dk).

