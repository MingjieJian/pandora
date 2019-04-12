      subroutine PLONK
     $(N,XLM,TE,B)
C
C     Rudolf Loeser, 2002 Jan 09.
C---- Compute a set of Planck function values for a given temperature.
C     (XLM in Angstroms.)
C---- See also PLUNK.
C     !DASH
      save
C     !DASH
      real*8 B, TE, XLM
      integer I, N
C     !DASH
      external PLUNCK, HI, BYE
C
C               XLM(N), B(N)
      dimension XLM(*), B(*)
C
      call HI ('PLONK')
C     !BEG
      do 100 I = 1,N
        call PLUNCK (XLM(I),TE,B(I))
  100 continue
C     !END
      call BYE ('PLONK')
C
      return
      end
