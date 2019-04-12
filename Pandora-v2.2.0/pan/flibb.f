      subroutine FLIBB
     $(MODE,JHB,CALLER,LU)
C
C     Rudolf Loeser, 1996 Jul 23
C---- Writes caller information to file LU.
C     MODE = 1: initialize
C          = 2: do it
C          = 3: terminate
C     !DASH
      save
C     !DASH
      integer JC, JHB, LU, M, MODE, N
      character CALLER*(*), LINE*128, PM*2
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
      intrinsic len
C
      dimension PM(2)
C
      data PM /' +', ' -'/
C     !EJECT
C
C     !BEG
      if(MODE.eq.2) then
        JC = len(CALLER) +2
        N  = M+JC
C
        if(N.gt.128) then
          write (LU,102) LINE
  102     format(A)
          M = 0
          N = JC
        end if
C
        LINE = LINE(:M)//PM(JHB)//CALLER
        M = N
C
      else if(MODE.eq.1) then
        write (LU,101) HEADSEE(:LENHEAD)
  101   format(' Hi/Bye System turned on under node ',A,'.')
        M = 0
C
      else if(MODE.eq.3) then
        if(M.gt.0) then
          write (LU,102) LINE
        end if
        write (LU,103)
  103   format(' Hi/Bye System turned off.')
      end if
C     !END
C
      return
      end
