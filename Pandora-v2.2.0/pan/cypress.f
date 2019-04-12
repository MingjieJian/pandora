      subroutine CYPRESS
     $(HND,HNK,HNI,H2N,ZHEL,PRESS,LIMP,HNKR,HNIR,H2NR,ZHER,F)
C
C     Rudolf Loeser, 1980 Jul 21
C---- Sets up HNKR, H2NR and HNIR, population ratios for H.S.E.
C     (This is version 2 of CYPRESS.)
C     !DASH
      save
C     !DASH
      real*8 F, H2N, H2NR, HND, HNI, HNIR, HNK, HNKR, PRESS, ZHEL, ZHER
      integer J, LIMP, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external ARRDIV, HI, BYE
C
C               HND(N), HNK(N), HNIR(N,LIMP), HNKR(N), H2N(N), H2NR(N),
      dimension HND(*), HNK(*), HNIR(N,*),    HNKR(*), H2N(*), H2NR(*),
C
C               HNI(N,LIMP), ZHER(N), ZHEL(N), PRESS(N), F(N)
     $          HNI(N,*),    ZHER(*), ZHEL(*), PRESS(*), F(*)
C
      call HI ('CYPRESS')
C     !BEG
      do 100 J = 1,LIMP
        call ARRDIV (HNI(1,J), HND, HNIR(1,J), N)
  100 continue
      call ARRDIV   (HNK,      HND, HNKR,      N)
      call ARRDIV   (H2N,      HND, H2NR,      N)
      call ARRDIV   (ZHEL,     HND, ZHER,      N)
      call ARRDIV   (PRESS,    HND, F,         N)
C     !END
      call BYE ('CYPRESS')
C
      return
      end
