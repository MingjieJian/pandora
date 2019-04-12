      subroutine PLINK
     $(NO,N,NRAD,Z,BDIJ,TAUIJ,INDX,DLOG,ZLOG,IMAGE,REMARK)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Plots ratios of departure coefficients,
C     for radiative transitions.
C     (This is version 2 of PLINK.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, DLOG, ONE, TAUIJ, Z, ZLOG
      integer IBEG, IEND, INDX, N, NO, NRAD
      character IMAGE*(*), REMARK*8, TIT*10
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
      external  OBERON, ZEBRA, SHRIMP, ABJECT, LINER, KPRINT, SYZYGY,
     $          HI, BYE
C
C               Z(N), DLOG(N,NRAD), TAUIJ(N,NRAD), INDX(NRAD), ZLOG(N),
      dimension Z(*), DLOG(*),      TAUIJ(*),      INDX(*),    ZLOG(*),
C
C               BDIJ(N,NL)
     $          BDIJ(*)
C     !EJECT
C
      call HI ('PLINK')
C     !BEG
C---- Compute logs of B-ratios
      call OBERON (N,BDIJ,INDX,NRAD,DLOG,ZZLARGE)
C---- Initialize plot image
      call ZEBRA  (Z,ZLOG,N,NRAD,IBEG,IEND,DLOG,ZZLARGE,ONE,IMAGE,
     $             TIT,'PLINK')
C---- Enter points into image
      call SHRIMP (ZLOG,N,IBEG,IEND,DLOG,NRAD,ALPHS,26,ZZLARGE,2,IMAGE)
C---- Print plot
      call ABJECT (NO)
      write (NO,100) TIT, REMARK
  100 format(' ','Graph of log(Ratio of Departure Coefficients)',
     $           ' vs. ', A10,', for radiative transitions, ',A8,
     $           ' graph')
      call LINER  (1,NO)
      call KPRINT (IMAGE,NO)
C---- Now plot Tau's vs. Z
      call SYZYGY (N,IBEG,IEND,Z,ZLOG,TAUIJ,INDX,NRAD,NO,2)
C     !END
      call BYE ('PLINK')
C
      return
      end
