      subroutine RAOPEN
     $(FILDAT,LUN,LENPHYS,GOOD,IST)
C
C     Rudolf Loeser, 1986 Jul 02
C---- Open an RA scratch file; see remarks in RAFAEL.
C
C     Note: recl is in units of r*4.
C     !DASH
      save
C     !DASH
      real*8 FILDAT, ONE, ZERO
      integer IST, LENGTH, LENPHYS, LUN, NR8S, RETRY
      logical GOOD
C     !COM
C---- RARTRY      as of 1998 Jun 02
      integer     LIMTRY
      common      /RARTRY/ LIMTRY
C     Control parameter for RAFAEL (random access I/O).
C     .
C     !DASH
      external RALENG
C
      dimension FILDAT(11)
C
      data ZERO,ONE /0.D0, 1.D0/
C
C     !BEG
      call RALENG (LENPHYS,LENGTH,NR8S,GOOD)
      if(GOOD) then
        RETRY = 0
  100   continue
C
        open (unit=LUN, access='DIRECT', status='UNKNOWN',
     $        recl=(8*NR8S), iostat=IST)
        if((IST.ne.0).and.(RETRY.lt.LIMTRY)) then
          RETRY = RETRY+1
          goto 100
        end if
C
        GOOD = (IST.eq.0)
        if(GOOD) then
          FILDAT( 1) = LUN
          FILDAT( 2) = ONE
          FILDAT( 3) = LENGTH
          FILDAT( 4) = 2*NR8S-2
          FILDAT( 5) = ZERO
          FILDAT( 6) = ZERO
          FILDAT( 7) = ZERO
          FILDAT( 8) = ZERO
          FILDAT( 9) = ZERO
          FILDAT(10) = ZERO
          FILDAT(11) = ZERO
        end if
      end if
C     !END
C
      return
      end
