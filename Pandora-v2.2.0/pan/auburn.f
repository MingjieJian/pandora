      subroutine AUBURN
     $(N,ANGST,FREQU)
C
C     Rudolf Loeser, 1987 Mar 20
C---- Converts a table from Angstroms to frequency units.
C     (This is version 2 of AUBURN.)
C     !DASH
      save
C     !DASH
      real*8 ANGST, FREQU
      integer I, N
C     !DASH
      external ANGIE, HI, BYE
C
C               ANGST(N), FREQU(N)
      dimension ANGST(*), FREQU(*)
C
      call HI ('AUBURN')
C     !BEG
      do 100 I = 1,N
        call ANGIE (ANGST(I), FREQU(I))
  100 continue
C     !END
      call BYE ('AUBURN')
C
      return
      end
