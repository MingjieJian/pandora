      subroutine TONY
     $(NW,WTAB,INDXW,IPER,KIPE,NOPAC,WL)
C
C     Rudolf Loeser, 1973 May 18
C---- Makes a contributions plot.
C     (This is version 3 of TONY.)
C     !DASH
      save
C     !DASH
      real*8 SIG, WL, WTAB
      integer INDXW, IPER, KIPE, KOUNT, NO, NOPAC, NW
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
C     !EJECT
      external LOGO, COZUMEL, NITHARD, FARAX, KPRINT, HI, BYE
C
C               IPER(Nopac,Numkon), WTAB(Numkon), INDXW(Numkon),
      dimension IPER(*),            WTAB(*),      INDXW(*),
C
C               WL(Numkon)
     $          WL(*)
C
      call HI ('TONY')
C     !BEG
      SIG = ZL10SMA
C---- Compute logs of abscissa
      call LOGO     (WTAB, NW, 1, SIG, WL)
C---- Initialize plot image
      call COZUMEL  (IMAGE, WL, NW, SIG)
C---- Enter data
      call NITHARD  (IMAGE, WL, NW, SIG, INDXW, IPER, KOUNT)
C---- Print graph, if it seems worthwhile
      if(KOUNT.gt.(2*NOPAC)) then
        call FARAX  (NO, KIPE)
        call KPRINT (IMAGE, NO)
      end if
C     !END
      call BYE ('TONY')
C
      return
      end
