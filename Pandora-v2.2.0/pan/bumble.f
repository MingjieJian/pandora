      subroutine BUMBLE
     $(LABEL,JUNK,TAU,N,CALLER,KODE)
C
C     Rudolf Loeser, 1983 Dec 05
C---- Dumps, for optical depth calculations.
C     !DASH
      save
C     !DASH
      real*8 TAU
      integer JUNK, KODE, LUEO, MODE, N
      character CALLER*(*), LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, VECOUT, MASHED, ABORT, HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('BUMBLE')
C     !BEG
      MODE = KODE
      if(MODE.ne.3) then
        MODE = 1
      end if
C
      call MESHED   ('BUMBLE', MODE)
      write (LUEO,100) CALLER,LABEL,JUNK
  100 format(' ','Called from: ',A//
     $       ' ',A//
     $       ' ','Integrand is not monotonic at',I4,'. point.')
      call VECOUT   (LUEO, TAU, N, 'Integrand')
C
      if(MODE.eq.3) then
        call MASHED ('BUMBLE')
      else if(MODE.eq.1) then
        call ABORT
      end if
C     !END
      call BYE ('BUMBLE')
C
      return
      end
