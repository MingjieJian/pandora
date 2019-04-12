      subroutine LUNGA
     $(X,F,N,TAU,KODE,LAB,IMG,W)
C
C     Rudolf Loeser, 1981 Oct 30
C---- Computes TAU along the ray "LAB".
C     Returns with KODE=1 if TAU seems ok, =0 if not.
C     !DASH
      save
C     !DASH
      real*8 F, TAU, W, X
      integer IMG, JUNK, KODE, LUEO, N
      logical lummy1, lummy2
      character LAB*(*), LABEL*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external TUNA, MESHED, PRIVET, MASHED, HI, BYE
C
      dimension W(*)
C
C               X(N), F(N), TAU(N), IMG(N)
      dimension X(*), F(*), TAU(*), IMG(*)
C
C
      call HI ('LUNGA')
C     !BEG
      write (LABEL,100) LAB(1:84)
  100 format('Optical Depth: ',A84)
C
      call TUNA     (N, X, F, TAU, LABEL, JUNK, lummy1, lummy2, IMG, W)
C
      if(JUNK.gt.0) then
        call MESHED ('LUNGA', 3)
        write (LUEO,101) LABEL,JUNK
  101   format(' ','Weight Matrix using spherical coordinates.'///
     $         ' ',A/
     $         ' ','is not monotonic at',I3,'. point.')
        call PRIVET (LUEO, TAU, N)
        call MASHED ('LUNGA')
C
        KODE = 0
      else
        KODE = 1
      end if
C     !END
      call BYE ('LUNGA')
C
      return
      end
