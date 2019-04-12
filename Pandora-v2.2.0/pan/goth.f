      subroutine GOTH
     $(N,NL,T,XNU,XNUK,BATAL)
C
C     Rudolf Loeser, 1982 Feb 19
C---- Computes Stimulated Emission factors for each level, normal case.
C     (See also HUN.)
C     !DASH
      save
C     !DASH
      real*8 BATAL, DNU, HNUKT, T, XNU, XNUK
      integer I, J, N, NL
C     !DASH
      external PROD, HI, BYE
C
C               T(N), XNU(NL), BATAL(N,NL)
      dimension T(*), XNU(*),  BATAL(N,*)
C
      call HI ('GOTH')
C     !BEG
      do 101 J = 1,NL
        DNU = XNUK-XNU(J)
        do 100 I = 1,N
          call PROD (T(I), DNU, 1, HNUKT, BATAL(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('GOTH')
C
      return
      end
