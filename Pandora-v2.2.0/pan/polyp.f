      subroutine POLYP
     $(TE,XNE,PE,N)
C
C     Rudolf Loeser, 1982 Jun 21
C---- Computes a table of electron pressure.
C     !DASH
      save
C     !DASH
      real*8 PE, TE, XNE
      integer I, N
C     !DASH
      external ELPH, HI, BYE
C
C               TE(N), XNE(N), PE(N)
      dimension TE(*), XNE(*), PE(*)
C
      call HI ('POLYP')
C     !BEG
      do 100 I = 1,N
        call ELPH (XNE(I),TE(I),PE(I))
  100 continue
C     !END
      call BYE ('POLYP')
C
      return
      end
