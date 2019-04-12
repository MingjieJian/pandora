      subroutine YEAH
     $(Z,BDI,TAUIJ,INDX,NRAD,IMAGE,BLOG,ZLOG,N,NL,NO,REMARK,LEVELS)
C
C     Rudolf Loeser, 1968 Mar 05
C---- Plots departure coefficients and Tau scales vs. Z.
C     !DASH
      save
C     !DASH
      real*8 BDI, BLOG, ONE, SIG, TAUIJ, Z, ZLOG
      integer IBEG, IEND, INDX, N, NL, NO, NRAD
      character IMAGE*(*), LEVELS*20, REMARK*8, TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external LOGO, ZEBRA, SHRIMP, ABJECT, LINER, KPRINT, SYZYGY,
     $         HI, BYE
C
C               Z(N), BDI(N,NL), TAUIJ(N,NRAD), INDX(NRAD), BLOG(N,NL),
      dimension Z(*), BDI(*),    TAUIJ(*),      INDX(*),    BLOG(*),
C
C               ZLOG(N)
     $          ZLOG(*)
C     !EJECT
C
      call HI ('YEAH')
C     !BEG
      SIG = ZL10SMA
C---- Compute logs of BD
      call LOGO   (BDI, (N*NL), 1, SIG, BLOG)
C---- Initialize plot image
      call ZEBRA  (Z, ZLOG, N, NL, IBEG, IEND, BLOG, SIG, ONE, IMAGE,
     $             TIT, 'YEAH')
C
C---- Enter points into image
      call SHRIMP (ZLOG, N, IBEG, IEND, BLOG, NL, ALPHS, 26, SIG, 2,
     $             IMAGE)
C
C---- Write graph header and image
      call ABJECT (NO)
      write (NO,100) TIT,REMARK,LEVELS
  100 format(' ','Graph of log10(Departure Coefficient) vs. ',A10,9X,
     $           A8,' graph ',A20)
      call LINER  (1, NO)
      call KPRINT (IMAGE, NO)
C
C---- Now plot Tau's vs. Z
      call SYZYGY (N, IBEG, IEND, Z, ZLOG, TAUIJ, INDX, NRAD, NO, 1)
C     !END
      call BYE ('YEAH')
C
      return
      end
