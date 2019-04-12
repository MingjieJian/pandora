      subroutine PROD
     $(T,X,MODE,ARG,TERM)
C
C     Rudolf Loeser, 1982 Feb 17
C---- Computes stimulated emission factor.
C
C     INPUT:
C     T  is in Kelvins;
C     X  is in Angstroms       when MODE=2,
C           in frequency units when MODE=1.
C
C     OUTPUT:
C     ARG   is (h*nu)/(k*T);
C     TERM  is exp(-ARG).
C     !DASH
      save
C     !DASH
      real*8 ALIM, ARG, T, TERM, X
      integer LUEO, MODE
      character LABEL*6
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external HUNK, MESHED, MASHED, HI, BYE
C
      dimension LABEL(2)
C
      data LABEL  /'Lambda', 'Nu'/
C
      call HI ('PROD')
C     !BEG
      call HUNK     (T, X, MODE, ARG)
C
      if((-ARG).lt.ZLNLARG) then
        TERM = exp(-ARG)
      else
        TERM = ZZLARGE
C
        call MESHED ('PROD', 3)
        write (LUEO,100) T,LABEL(MODE),X,ARG,TERM
  100   format(' ','Trouble in PROD: computing stimulated ',
     $             'emission factor.'/
     $         ' ','T=',1PE16.8,5X,A,'=',E16.8,5X,'-Exponent=',E16.8/
     $         ' ','Result forced =',E16.8)
        call MASHED ('PROD')
      end if
C     !END
      call BYE ('PROD')
C
      return
      end
