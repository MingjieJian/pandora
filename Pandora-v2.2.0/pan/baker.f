      subroutine BAKER
     $(ITAU,N,NL,KRJ,CIJ,PIJ,YBAR,X,IX,Z)
C
C     Rudolf Loeser, 2003 Jun 10
C---- Computes the Z-matrix needed for the basic b-ratios calculation.
C     (See also ZEA.)
C     (This is version 2 of BAKER.)
C     !DASH
      save
C     !DASH
      real*8 CADD, CIJ, PIJ, X, YBAR, Z, ZERO
      integer I, IJ, IL, ITAU, IU, IX, J, KRJ, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXIJ, CADDY, HI, BYE
C
      dimension X(*), IX(*)
C
C               PIJ(N,NL**2), CIJ(N,NL**2), YBAR(N,NT), Z(NL,NL)
      dimension PIJ(N,*),     CIJ(N,*),     YBAR(*),    Z(NL,*)
C
      data IU,IL /0, 0/
C
      call HI ('BAKER')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          if(I.eq.J) then
            Z(I,J) = ZERO
          else
            call INDXIJ (I, J, IJ)
            call CADDY  (ITAU, I, J, KRJ, IU, IL, YBAR, X, IX, CADD)
            Z(I,J) = (CIJ(ITAU,IJ)+CADD)+PIJ(ITAU,IJ)
          end if
  100   continue
  101 continue
C     !END
      call BYE ('BAKER')
C
      return
      end
