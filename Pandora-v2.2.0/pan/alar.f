      subroutine ALAR
     $(A,N,NL,IS)
C
C     Rudolf Loeser, 1990 Aug 02
C---- Drives ECKE to compute "signals" describing
C     the contents of array A.
C     (This is version 2 of ALAR.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer IS, J, N, NL
C     !DASH
      external ECKE, HI, BYE
C
C               A(N,NL), IS(N,NL)
      dimension A(N,*),  IS(N,*)
C
      call HI ('ALAR')
C     !BEG
      do 100 J = 1,NL
        call ECKE (A(1,J), N, IS(1,J))
  100 continue
C     !END
      call BYE ('ALAR')
C
      return
      end
