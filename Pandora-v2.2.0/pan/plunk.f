      subroutine PLUNK
     $(N,XLM,TE,B)
C
C     Rudolf Loeser, 1981 Jul 26.
C---- Compute a set of Planck function values for a given wavelength.
C     (XLM in Angstroms.)
C---- See also PLONK.
C     (This is version 2 of PLUNK.)
C     !DASH
      save
C     !DASH
      real*8 B, TE, XLM
      integer I, N
C     !DASH
      external PLUNCK, HI, BYE
C
C               TE(N), B(N)
      dimension TE(*), B(*)
C
      call HI ('PLUNK')
C     !BEG
      do 100 I = 1,N
        call PLUNCK (XLM,TE(I),B(I))
  100 continue
C     !END
      call BYE ('PLUNK')
C
      return
      end
