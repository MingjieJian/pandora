      subroutine FLEBB
C
C     Rudolf Loeser, 1996 Jul 22
C---- Types current stack for Hi/Bye/Abort system.
C     !DASH
      save
C     !DASH
      integer I
      character SPACE*100
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
      data SPACE /' '/
C
C     !BEG
      if(DOSTK.and.(NSTKSEE.gt.0)) then
        write (*,100)
  100   format(' ','Current contents of the caller stack:')
        write (*,101) (I,SPACE(:I),STCKSEE(I),I=1,NSTKSEE)
  101   format(' ',I3,A,A)
      end if
C     !END
C
      return
      end
