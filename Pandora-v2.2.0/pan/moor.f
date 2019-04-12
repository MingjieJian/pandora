      subroutine MOOR
     $(TAU,EP1,EPS,N,KSMA,KLAR,CALLER)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Dumps for FENN.
C     !DASH
      save
C     !DASH
      real*8 EP1, EPS, TAU
      integer KLAR, KSMA, LUEO, N
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, VECOUT, MASHED, HI, BYE
C
C               TAU(N), EP1(N), EPS(N)
      dimension TAU(*), EP1(*), EPS(*)
C
      call HI ('MOOR')
C     !BEG
      call MESHED  (CALLER, 2)
      write (LUEO,100)
  100 format(' ','Newest EP1 editing procedure')
C
      call VECOUT  (LUEO, TAU, N, 'TAUK'   )
      call VECOUT  (LUEO, EPS, N, 'Old EP1')
C
      if(KSMA.eq.1) then
        call LINER (1, LUEO)
        write (LUEO,101)
  101   format(' ','Small or negative values were edited.')
      end if
      if(KLAR.eq.1) then
        call LINER (1, LUEO)
        write (LUEO,102)
  102   format(' ','Large values were edited')
      end if
C
      call VECOUT  (LUEO, EP1, N, 'New EP1')
      call MASHED  (CALLER)
C     !END
      call BYE ('MOOR')
C
      return
      end
