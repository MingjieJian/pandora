      subroutine BRYMBO
     $(N,RHEAB,HEABD)
C
C     Rudolf Loeser, 1998 Feb 18
C---- Computes depth-dependent He abundance, for diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 ABD, HEABD, RHEAB, dummy
      integer KODE, N
C     !DASH
      external FRANK, SET1, ARRMUL, HI, BYE
C
C               RHEAB(N), HEABD(N)
      dimension RHEAB(*), HEABD(*)
C
      call HI ('BRYMBO')
C     !BEG
      call FRANK  ('HE ',0,ABD,dummy,dummy,dummy,KODE)
      call SET1   (HEABD,N,ABD)
      call ARRMUL (HEABD,RHEAB,HEABD,N)
C     !END
      call BYE ('BRYMBO')
C
      return
      end
