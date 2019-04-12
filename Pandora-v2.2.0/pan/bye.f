      subroutine BYE
     $(CALLER)
C
C     Rudolf Loeser, 1996 Jul 19
C---- Reacts to module deactivations.
C     (This is version 2 of BYE.)
C     !DASH
      save
C     !DASH
      character CALLER*(*)
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
c
cx      character   NOWD*11, NOWT*8
cx      common      /hinow/ xhi
c
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
      external FLUBB
C
C     !BEG
      if(DOSTK) then
        if(NSTKSEE.gt.0) then
          NSTKSEE = NSTKSEE-1
        end if
      end if
C
      if(DOSEE) then
        call FLUBB (2,CALLER)
      end if
C     !END
C
cx      call cpu_time(xnow)
cx      dx = xnow-xhi
cx      call get_date(nowd)
cx      call get_time(nowt)
cx      print 9000, nowd, nowt, caller, dx
 9000 format('*** ',a,x,a,' - ',a,' says BYE after ',f8.2,' min ***')
c
      return
      end
