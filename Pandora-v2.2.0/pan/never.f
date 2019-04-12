      subroutine NEVER
     $(Z,XND,IMAGE,ZLOG,N,M,LAST,NO,REMARK,LEVELS)
C
C     Rudolf Loeser, 1969 Jun 10
C---- Plots a bunch of number densities.
C     !DASH
      save
C     !DASH
      real*8 ONE, SIG, XND, Z, ZLOG
      integer IBEG, IEND, M, N, NO
      logical LAST
      character IMAGE*(*), LEVELS*20, REMARK*8, STAR*1, TIT*10
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
      equivalence (SYMBS(45),STAR  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
C     !EJECT
      external  LOGO, ZEBRA, SHRIMP, ABJECT, LINER, KPRINT, HI, BYE
C
C               XND(N,M), Z(N), ZLOG(N)
      dimension XND(N,*), Z(*), ZLOG(*)
C
      call HI ('NEVER')
C     !BEG
      SIG = ZL10SMA
C---- Take logs of number densities
      call LOGO     (XND, (N*M), 1, SIG, XND)
C---- Initialize plot image
      call ZEBRA    (Z, ZLOG, N, M, IBEG, IEND, XND, SIG, ONE, IMAGE,
     $               TIT, 'NEVER')
C
C---- Enter points into image
      if(LAST) then
C       (Special case - continuum number density sits in last slot,
C        needs "*" as plotting character)
        call SHRIMP (ZLOG, N, IBEG, IEND, XND(1,1), (M-1), ALPHS, 26,
     $               SIG, 2, IMAGE)
        call SHRIMP (ZLOG, N, IBEG, IEND, XND(1,M),     1,  STAR,  1,
     $               SIG, 2, IMAGE)
      else
C       (Not special case)
        call SHRIMP (ZLOG, N, IBEG, IEND, XND(1,1),     M, ALPHS, 26,
     $               SIG, 2, IMAGE)
      end if
C
C---- Write graph header and image
      call ABJECT   (NO)
      write (NO,100) TIT,REMARK,LEVELS
  100 format(' ','Graph of log10(Number Density) vs. ',A10,10X,A8,
     $           ' graph ',A20)
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
C     !END
      call BYE ('NEVER')
C
      return
      end
