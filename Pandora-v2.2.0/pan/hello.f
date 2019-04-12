      subroutine HELLO
     $(KODE,HEAD,LUN)
C
C     Rudolf Loeser, 1996 Jul 19
C---- Resets the HI/BYE/ABORT system.
C     (This is version 2 of HELLO.)
C     !DASH
      save
C     !DASH
      integer KODE, LUN
      logical NOTIFY
      character BLANK*1, HEAD*(*)
C     !COM
C---- EYE         as of 2005 Jan 28
      integer     LSTKSEE,KODESEE,NSTKSEE,LUNSEE,LENHEAD
      logical     DOSEE,DOSTK,DOCHK
      character   HEADSEE*32, STCKSEE*32
      parameter   (LSTKSEE=100)
      dimension   STCKSEE(LSTKSEE)
      common      /EYE1/ DOSEE,DOSTK,DOCHK
      common      /EYE2/ KODESEE,NSTKSEE,LUNSEE,LENHEAD
      common      /EYE3/ HEADSEE,STCKSEE
C     Control parameters for Hi/Bye/Abort System (revised).
C     KODESEE =  0: nothing;
C             =  1: ABORT traceback;
C             =  2: type to screen;
C             =  3: write to file;
C             = 98: check X ?!, to screen
C             = 99: check X ?!, to file
C     HEADSEE = name of highest node for 2 or 3.
C     .
C     !DASH
C     !EJECT
      intrinsic index, max
C
      data BLANK /' '/
C
C     !BEG
      DOCHK = (KODE.eq.98).or.(KODE.eq.99)
      DOSEE = (KODE.eq.2).or.(KODE.eq.3)
      DOSTK = KODE.eq.1
C
      NOTIFY  = (KODE.ne.KODESEE).and.(max(KODE,KODESEE).gt.1)
      KODESEE = KODE
      HEADSEE = HEAD
      LENHEAD = index(HEADSEE,BLANK) -1
      if((KODESEE.eq.3).or.(KODESEE.eq.99)) then
        LUNSEE = LUN
      else
        LUNSEE = 0
      end if
C
      if(NOTIFY) then
        write (*,100) KODE,HEAD,LENHEAD,LUN
  100   format(' Hello: HI/BYE/ABORT System reset; KODE =',I3,
     $         ', HEAD = ',A,'(',I2,'), LUN =',I3)
      end if
C     !END
C
      return
      end
