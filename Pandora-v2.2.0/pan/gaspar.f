      subroutine GASPAR
     $(N,Z,NZE,IND,FME,FNRML,XI,BI,A,W)
C
C     Rudolf Loeser, 1993 Jul 01
C---- Smears rays into beams, for GRIFFIN.
C     !DASH
      save
C     !DASH
      real*8 A, BI, FME, FNRML, W, XI, Z
      integer IND, J, N, NZE
C     !DASH
      external ARRMUL, BUSH, DIVIDE, HI, BYE
C
C               Z(N), FME(N,NZE), FNRML(NZE), XI(N), BI(N), A(N), W(N),
      dimension Z(*), FME(N,*),   FNRML(*),   XI(*), BI(*), A(*), W(*),
C
C               IND(NZE)
     $          IND(*)
C
      call HI ('GASPAR')
C     !BEG
      do 100 J = 1,NZE
        call ARRMUL (XI,FME(1,J),W,N)
        call BUSH   (Z,1,W,1,A,1,N)
        call DIVIDE (A(N),FNRML(J),BI(IND(J)))
  100 continue
C     !END
      call BYE ('GASPAR')
C
      return
      end
