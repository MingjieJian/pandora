      subroutine GRATIAN
     $(N,NL,CHL,CHT,SUM)
C
C     Rudolf Loeser, 1995 May 30
C---- Computes total heating rate.
C     !DASH
      save
C     !DASH
      real*8 CHL, CHT, SUM
      integer J, N, NL, NT
C     !DASH
      external ZERO1, ARRADD, HI, BYE
C
C               SUM(N), CHL(N,NL), CHT(N,NT)
      dimension SUM(*), CHL(N,*),  CHT(N,*)
C
      call HI ('GRATIAN')
C     !BEG
      call ZERO1    (SUM,N)
      NT = NL*(NL-1)/2
C
      do 100 J = 1,NT
        call ARRADD (CHT(1,J),SUM,SUM,N)
  100 continue
C
      do 101 J = 1,NL
        call ARRADD (CHL(1,J),SUM,SUM,N)
  101 continue
C     !END
      call BYE ('GRATIAN')
C
      return
      end
