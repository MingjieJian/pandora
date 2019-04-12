      subroutine CROSS
     $(NO,XJNU,LIM,Z,N,ZLOG,REMARK,LEVELS)
C
C     Rudolf Loeser, 1973 Apr 03
C---- Plots Jnu vs. Z.
C     (This is version 2 of CROSS.)
C     !DASH
      save
C     !DASH
      real*8 ONE, SIG, XJNU, Z, ZLOG
      integer IBEG, IEND, LIM, N, NO
      character LEVELS*20, REMARK*8, TIT*10
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
      external LOGO, ZEBRA, SHRIMP, ABJECT, LINER, KPRINT, HI, BYE
C
C               XJNU(N,LIM), Z(N), ZLOG(N)
      dimension XJNU(*),     Z(*), ZLOG(*)
C
      call HI ('CROSS')
C     !BEG
      SIG = ZL10SMA
C---- Convert to logs
      call LOGO   (XJNU, (N*LIM), 1, SIG, XJNU)
C
C---- Initialize plot image
      call ZEBRA  (Z, ZLOG, N, LIM, IBEG, IEND, XJNU, SIG, ONE,
     $             IMAGE, TIT, 'CROSS')
C
C---- Enter points into image
      call SHRIMP (ZLOG, N, IBEG, IEND, XJNU, LIM, ALPHS, 26, SIG,
     $             2, IMAGE)
C
C---- Write graph header and image
      call ABJECT (NO)
      write (NO,100) TIT,REMARK,LEVELS
  100 format(' ','Graph of log10(Jnu at Level''s edge) vs. ',A10,
     $           10X,A8,' graph ',A20)
      call LINER  (1, NO)
      call KPRINT (IMAGE, NO)
C     !END
      call BYE ('CROSS')
C
      return
      end
