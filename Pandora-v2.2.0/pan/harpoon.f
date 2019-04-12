      subroutine HARPOON
     $(NDT,ADT,ALBDT,APD)
C
C     Rudolf Loeser, 1983 Mar 30
C---- Computes APD, for QUIVER.
C     (This is version 3 of HARPOON)
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, APD, ONE
      integer I, NDT
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
C               ADT(NDT), ALBDT(NDT), APD(NDT)
      dimension ADT(*),   ALBDT(*),   APD(*)
C
      call HI ('HARPOON')
C     !BEG
      do 100 I = 1,NDT
        APD(I) = ADT(I)*(ONE-ALBDT(I))
  100 continue
C     !END
      call BYE ('HARPOON')
C
      return
      end
