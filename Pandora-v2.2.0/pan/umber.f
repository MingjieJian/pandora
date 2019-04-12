      subroutine UMBER
     $(L,EMU,WMU)
C
C     Rudolf Loeser, 1981 Sep 02
C---- Computes Mu-dependent weights for the integration to obtain Flux
C     from Intensity.
C     (This is version 2 of UMBER.)
C     !DASH
      save
C     !DASH
      real*8 EMU, ONE, WMU
      integer I, L
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
C               EMU(L), WMU(L)
      dimension EMU(*), WMU(*)
C
      call HI ('UMBER')
C     !BEG
      if(L.le.1) then
        WMU(1) = ONE
      else
C
        WMU(1) = EMU(1)*(EMU(1)-EMU(2))
C
        if(L.ge.3) then
          do 100 I = 2,(L-1)
            WMU(I) = EMU(I)*(EMU(I-1)-EMU(I+1))
  100     continue
        end if
C
        WMU(L) = EMU(L)*EMU(L-1)
      end if
C     !END
      call BYE ('UMBER')
C
      return
      end
