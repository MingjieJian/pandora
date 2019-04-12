      subroutine DALETH
     $(N,S,BHS,SCAT,XJNU,LAG)
C
C     Rudolf Loeser, 1971 Dec 03
C---- Computes Continuum Source Function.
C     !DASH
      save
C     !DASH
      real*8 BHS, ONE, S, SCAT, XDEN, XJNU, XNUM
      integer I, LAG, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, DIVIDE, HI, BYE
C
C               S(N), BHS(N), SCAT(N), XJNU(N)
      dimension S(*), BHS(*), SCAT(*), XJNU(*)
C
      call HI ('DALETH')
C     !BEG
      if(LAG.le.0) then
        do 100 I = 1,N
          XNUM = BHS(I)+SCAT(I)*XJNU(I)
          XDEN = ONE   +SCAT(I)
          call DIVIDE (XNUM,XDEN,S(I))
  100   continue
      else
        call MOVE1    (BHS,N,S)
      end if
C     !END
      call BYE ('DALETH')
C
      return
      end
