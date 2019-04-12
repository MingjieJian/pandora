      subroutine FLABB
     $(MODE,JHB,CALLER)
C
C     Rudolf Loeser, 1996 Jul 23
C---- Types caller information to the screen.
C     MODE = 1: initialize
C          = 2: do it
C          = 3: terminate
C     !DASH
      save
C     !DASH
      integer JHB, MODE, N
      character BLANKS*60, CALLER*(*), LINE*80
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
      data BLANKS /' '/
C
C     !BEG
      if(MODE.eq.2) then
        if(JHB.eq.1) then
          N = N+1
          LINE = BLANKS(:N)//'+'//CALLER
C
        else if(JHB.eq.2) then
          LINE = BLANKS(:N)//'-'//CALLER
          N = N-1
        end if
C
      else if(MODE.eq.1) then
        N = 0
        LINE = ' Hi/Bye display begins'
      else if(MODE.eq.3) then
        LINE = ' Hi/Bye display ends'
      end if
C
      write (*,100) LINE
  100 format(A)
C     !END
C
      return
      end
