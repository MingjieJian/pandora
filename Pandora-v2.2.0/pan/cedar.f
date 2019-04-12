      subroutine CEDAR
     $(HND,HNK,HNI,H2N,ZHEL,LIMP,HNKR,HNIR,H2NR,ZHER)
C
C     Rudolf Loeser, 1969 Aug 04
C---- Adjusts values of Hydrogen populations for H.S.E.
C     (See also CYPRESS.)
C     !DASH
      save
C     !DASH
      real*8 H2N, H2NR, HND, HNI, HNIR, HNK, HNKR, ZHEL, ZHER
      integer J, LIMP, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external ARRMUL, WENDY, HI, BYE
C
C               HND(N), H2NR(N), HNIR(N,LIMP), HNK(N), HNKR(N), H2N(N),
      dimension HND(*), H2NR(*), HNIR(N,*),    HNK(*), HNKR(*), H2N(*),
C
C               HNI(N,LIMP), ZHER(N), ZHEL(N)
     $          HNI(N,*),    ZHER(*), ZHEL(*)
C
      call HI ('CEDAR')
C     !BEG
      do 100 J = 1,LIMP
        call ARRMUL (HND,HNIR(1,J),HNI(1,J),N)
  100 continue
      call ARRMUL   (HND,HNKR     ,HNK     ,N)
      call ARRMUL   (HND,H2NR     ,H2N     ,N)
      call ARRMUL   (HND,ZHER     ,ZHEL    ,N)
C
      call WENDY    (HNI,1,(N*LIMP), 9,'CEDAR')
      call WENDY    (HNK,1,N       ,11,'CEDAR')
      call WENDY    (H2N,1,N       , 7,'CEDAR')
C     !END
      call BYE ('CEDAR')
C
      return
      end
