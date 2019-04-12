      subroutine ZAFRA
     $(CSF,B,N,BC)
C
C     Rudolf Loeser, 1982 Sep 17
C---- Sets up an edited version of BC.
C     (This is version 2 of ZAFRA.)
C     !DASH
      save
C     !DASH
      real*8 B, BC, CSF
      integer I, N
C     !DASH
      external  HI,BYE
      intrinsic min
C
C               CSF(N), B(N), BC(N)
      dimension CSF(*), B(*), BC(*)
C
      call HI ('ZAFRA')
C     !BEG
      do 100 I = 1,N
        BC(I) = min(CSF(I),B(I))
  100 continue
C     !END
      call BYE ('ZAFRA')
C
      return
      end
