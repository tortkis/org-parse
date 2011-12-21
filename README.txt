
org-parse is a set of programs that read emacs org file and convert to
s-expression, convert to format compatible with lml2
(http://www.cliki.net/lml2), and create hunchentoot dispatcher that
displays the contents of the org file.

emacs org mode can export HTML file.  This program allows you to
convert an org file to S-expression, manipilate the data, and create
hunchentoot dispatcher function without generating intermediate HTML
file.  Also it adds expanding/collapsing buttons for TODO items.  (At
this point only the top level items.)
  
[Function]
read-org-file file-name => s-exp-org

  example.org
  * TODO read SICP
  [[./sicp.png][SICP]]
  * DONE write README
  * <2011-08-04 Thu> shop [1/2]
  - [x] olive oil
  - [ ] beer
  
  (defvar org)
  (setf org (read-org "example.org")) =>
  
  (((:* 0 ("TODO" "read SICP")) (:LINE (:LINK "sicp.png" "SICP")))
   ((:* 0 ("DONE" "write README")))
   ((:* 0 (NIL (:DATE "<2011-08-04 Thu>") "shop" (:CHECKLIST (1 2))))
    (:LIST-U ((:- 0 "[x] olive oil")) ((:- 0 "[ ] beer")))))
  
[Function]
org-to-shtml s-exp-org &key inline-image => lml2-shtml
  
  (defvar shtml)
  (setf shtml (org-parse:org-to-shtml org)) =>
  
  ((:DIV (|h2| (:DIV (:B ((:FONT :COLOR "red") " TODO ")) "read SICP"))
    (:DIV ((:A :HREF "sicp.png") "SICP")))
   (:DIV (|h2| (:DIV (:B ((:FONT :COLOR "green") " DONE ")) "write README")))
   (:DIV
    (|h2|
     (:DIV "" ((:FONT :COLOR "#999") " <2011-08-04 Thu> ") "shop"
      ((:FONT :COLOR "red") " [1/2]")))
    (:UL
     (:LI
      ((:INPUT :TYPE "checkbox" :DISABLED "disabled" :CHECKED "checked")
       "olive oil")
      :BR)
     (:LI ((:INPUT :TYPE "checkbox" :DISABLED "disabled") "beer") :BR))))
  
[Function]
org-to-shtml-w-head s-exp-org title &key inline-image => lml2-shtml-with-head
  
  (defvar shtml-w-head)
  (setf shtml-w-head (org-parse:org-to-shtml-w-head org "TODO")) =>
  
  (:HTML
   (:HEAD (:TITLE "TODO")
    ((:STYLE :TYPE "text/css") "<!--" "  pre {"
     "        background-color: #eeeeee;" "        padding: 5pt;"
     "        font-family: courier, monospace;" "        font-size: 90%;"
     "        overflow:auto;" "  }"
     "  h1, h2, h3, h4, h5, h6 { font-family: sans-serif; }" "-->"))
   (:BODY
    (:DIV (|h2| (:DIV (:B ((:FONT :COLOR "red") " TODO ")) "read SICP"))
     (:DIV ((:A :HREF "sicp.png") "SICP")))
    (:DIV (|h2| (:DIV (:B ((:FONT :COLOR "green") " DONE ")) "write README")))
    (:DIV
     (|h2|
      (:DIV "" ((:FONT :COLOR "#999") " <2011-08-04 Thu> ") "shop"
       ((:FONT :COLOR "red") " [1/2]")))
     (:UL
      (:LI
       ((:INPUT :TYPE "checkbox" :DISABLED "disabled" :CHECKED "checked")
        "olive oil")
       :BR)
      (:LI ((:INPUT :TYPE "checkbox" :DISABLED "disabled") "beer") :BR)))))
  
[Function]
export-org-to-html org-fn html-fn &key inline-image => nil
  
  Create HTML file from org file.
  
  (org-parse:export-org-to-html "example.org" "example.html")
  
  example.html
  <HTML><HEAD><TITLE>TODO</TITLE><STYLE TYPE="text/css"><!--  pre {        background-color: #eeeeee;        padding: 5pt;        font-family: courier, monospace;        font-size: 90%;        overflow:auto;  }  h1, h2, h3, h4, h5, h6 { font-family: sans-serif; }--></STYLE></HEAD><BODY><DIV><h2><DIV><B><FONT COLOR="red"> TODO </FONT></B>read SICP</DIV></h2><DIV><A HREF="sicp.png">SICP</A></DIV></DIV><DIV><h2><DIV><B><FONT COLOR="green"> DONE </FONT></B>write README</DIV></h2></DIV><DIV><h2><DIV><FONT COLOR="#999"> <2011-08-04 Thu> </FONT>shop<FONT COLOR="red"> [1/2]</FONT></DIV></h2><UL><LI><INPUT TYPE="checkbox" DISABLED="disabled" CHECKED="checked">olive oil</INPUT><BR />
  </LI><LI><INPUT TYPE="checkbox" DISABLED="disabled">beer</INPUT><BR />
  </LI></UL></DIV></BODY></HTML>
  

[Macro]
define-ht-org-dispatcher uri org-shtml &key (default-expand t) (return-uri nil) (inline-image nil) => symbol

  define a hunchentoot dispatcher function with uri that displays the
  contents of org-shtml, and register it to
  hunchentoot:*dispatch-table*.  The generated html includes buttons
  that returns to return-uri, expand all, and collapse all.

[Macro]
define-ht-org org-file uri &key (title nil) (default-expand t) (return-uri nil) (inline-image nil)

  a wrpper that generates org-shtml from org-file and call
  define-ht-org-dispatcher.
