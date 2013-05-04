
:- use_package([assertions]).

:- doc(filetype, application).

:- doc(title,"(Test of image inclusion in lpdoc documents)").

:- doc(author,"Dummy Author").

:- doc(copyright,"This is the copyright.").

:- doc(summary,"This @bf{module} is very nice and is written in
   @concept{Prolog} and CIAO. You can get it from
   @href{http://localhost/}{our home page}, located at:
   @href{http://localhost/} or contact us at our @email{email
   address}{clip@@clip.dia.fi.upm.es} which is
   @email{clip@@clip.dia.fi.upm.es}. Also, our group logo is
   @image{ciao_figure}, or, alternatively,
   @image{ciaoalt}{30}{40}. Nice, huh?").

:- doc(module,"This @concept{module} is very nice and is written
   in @concept{Prolog} @cite{iso-prolog} and CIAO
   @cite{ciao-assertions-manual-tr,bar,iso-prolog,foo} and @cite{foo}
   @href{http://localhost/}{Hello Dolly}
   uses CLP @cite{survey94}.").


