      subroutine LUCAN
     $(X,F,N,TAU,KODE,LAB)
C
C     Rudolf Loeser, 1981 Oct 30
C---- Computes TAU along a ray.
C     (This is version 2 of LUCAN.)
C     !DASH
      save
C     !DASH
      real*8 F, TAU, X
      integer JUNK, KODE, LUEO, N
      character LAB*(*), LABEL*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external TANU, MESHED, PRIVET, MASHED, HI, BYE
C
C               X(N), F(N), TAU(N)
      dimension X(*), F(*), TAU(*)
C
      call HI ('LUCAN')
C     !BEG
      write (LABEL,100) LAB(1:84)
  100 format('Optical Depth: ',A84)
C
      call TANU     (N, X, F, TAU, LABEL, JUNK)
C
      if(JUNK.gt.0) then
        call MESHED ('LUCAN', 3)
        write (LUEO,101) LAB,JUNK
  101   format(' ','Weight Matrix using spherical coordinates.'///
     $         ' ',A/
     $         ' ','is not monotonic at',I3,'. point.')
        call PRIVET (LUEO, TAU, N)
        call MASHED ('LUCAN')
C
        KODE = 0
      else
        KODE = 1
      end if
C     !END
      call BYE ('LUCAN')
C
      return
      end
