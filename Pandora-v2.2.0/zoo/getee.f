      subroutine GETEE
     $(D,ED,E,KODE)
C
C     Rudolf Loeser, 1981 Jul 26
C---- Sets up, and computes if necessary, a value of the
C     exponential function.
C     !DASH
      save
C     !DASH
      real*8 D, E, ED, ZERO
      integer KODE
C     !DASH
      external ABORT
C
      data ZERO /0.D0/
C
C     !BEG
      if((KODE.eq.0).or.(KODE.eq.2)) then
        ED = exp(-D)
      else if(KODE.eq.1) then
        if(E.lt.ZERO) then
          write (*,100) E
  100     format(' ','Error in GETEE: E =',1PE20.12)
          call ABORT
        end if
        ED = E
      else
        write (*,101) KODE
  101   format(' ','Error in GETEE: KODE =',I12)
        call ABORT
      end if
C     !END
C
      return
      end
