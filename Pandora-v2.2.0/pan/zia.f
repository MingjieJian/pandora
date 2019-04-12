      subroutine ZIA
     $(IU,IL,NL,CIJ,PIJ,YIJ,YBRIJ,X,IX,Z)
C
C     Rudolf Loeser, 2003 Nov 17
C---- Computes the Z matrix needed for statistical equilibrium
C     calculations by the VAMOS method.
C
C     See also ZEA & BAKER.
C     !DASH
      save
C     !DASH
      real*8 CIJ, PIJ, T, X, YBRIJ, YIJ, Z, ZERO, ZZ
      integer I, IL, IU, IX, J, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external TELOS, HI, BYE
C
      dimension X(*), IX(*)
C
C               PIJ(NL,NL), CIJ(NL,NL), YIJ(NL,NL), YBRIJ(NL,NL),
      dimension PIJ(NL,*),  CIJ(NL,*),  YIJ(*),     YBRIJ(*),
C
C               Z(NL,NL)
     $          Z(NL,*)
C
      call HI ('ZIA')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,NL
          if(I.eq.J) then
            ZZ = ZERO
          else
            ZZ = CIJ(I,J)+PIJ(I,J)
C
            if(J.gt.I) then
              call TELOS (J, I, IU, IL, NL, YBRIJ, YIJ, X, IX, T)
              ZZ = ZZ+T
            end if
C
          end if
C
          Z(I,J) = ZZ
  100   continue
  101 continue
C     !END
      call BYE ('ZIA')
C
      return
      end
