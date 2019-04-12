      subroutine DERBY
     $(V,EMU,I,N,VP)
C
C     Rudolf Loeser, 1982 Aug 11
C---- Computes expansion velocity projection along a particular ray;
C     either:
C     1 .le. I .le. N: in the frame comoving with point I, or
C     I .eq. 0       : in the rest frame.
C     !DASH
      save
C     !DASH
      real*8 EMU, REF, V, VP
      integer I, N
C     !DASH
      external ARRMUL, CONSUB, HI, BYE
C
C               V(N), EMU(N), VP(N)
      dimension V(*), EMU(*), VP(*)
C
      call HI ('DERBY')
C     !BEG
      call ARRMUL   (EMU,V,VP,N)
C
      if((I.ge.1).and.(I.le.N)) then
        REF = EMU(I)*V(I)
        call CONSUB (REF,VP,N)
      end if
C     !END
      call BYE ('DERBY')
C
      return
      end
