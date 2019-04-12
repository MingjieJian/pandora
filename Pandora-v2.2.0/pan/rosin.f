      subroutine ROSIN
     $(N,WN,XA,RR,XJNU,XJNUO,KNTMX,KNT,CSFCRIT)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes JNU iteratively, for ARBUTUS.
C     !DASH
      save
C     !DASH
      real*8 CSFCRIT, ONE, RR, SUM, WN, XA, XJNU, XJNUO, ZERO, dummy
      integer I, ITER, ITMX, J, KNT, KNTMX, N, jummy
      logical SAME
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, MOVE1, DIVIDE, BOUNDLO, CONVERD, HI, BYE
C
C               WN(N,N), XA(N), XJNU(N), RR(N), XJNUO(N)
      dimension WN(N,*), XA(*), XJNU(*), RR(*), XJNUO(*)
C     !EJECT
C
      call HI ('ROSIN')
C     !BEG
      call ZERO1      (XJNU,N)
C
      ITMX = KNTMX+1
      KNT  = -1
C
      do 102 ITER = 1,ITMX
        call MOVE1    (XJNU,N,XJNUO)
C
        do 101 I = 1,N
          SUM = ZERO
          do 100 J = 1,N
            SUM = SUM+WN(I,J)*XA(J)*XJNUO(J)
  100     continue
          call DIVIDE ((RR(I)+SUM),(ONE-XA(I)),XJNU(I))
  101   continue
        call BOUNDLO  (N,XJNU,ZERO)
C
        KNT = KNT+1
        call CONVERD  (XJNU,1,N,XJNUO,1,N,CSFCRIT,dummy,jummy,SAME)
        if(SAME) goto 103
C
  102 continue
C
  103 continue
C     !END
      call BYE ('ROSIN')
C
      return
      end
