      subroutine TIMARU
     $(INDX,NOPAC,N,OPAC,RSC,CO)
C
C     Rudolf Loeser, 2004 Aug 31
C---- Computes CO-lines scattering contribution.
C     (This is version 2 of TIMARU.)
C     !DASH
      save
C     !DASH
      real*8 CO, ONE, OPAC, RSC
      integer I, INDX, N, NOPAC
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               CO(Nopac,N), OPAC(N), RSC(N)
      dimension CO(NOPAC,*), OPAC(*), RSC(*)
C
      call HI ('TIMARU')
C     !BEG
      do 100 I = 1,N
        CO(INDX,I) = OPAC(I)*(ONE-RSC(I))
  100 continue
C     !END
      call BYE ('TIMARU')
C
      return
      end
