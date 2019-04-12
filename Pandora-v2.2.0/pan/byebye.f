      subroutine BYEBYE
C
C     Rudolf Loeser, 1996 Jul 19
C---- Final processing for HI/BYE/ABORT system.
C     (This is version 2 of BYEBYE.)
C     !DASH
      save
C     !DASH
      integer jummy
      character qummy*8
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
      external FLABB, FLIBB
C
C     !BEG
      DOSEE = .false.
      DOSTK = .false.
      if(HEADSEE(:7).eq.'PANDORA') then
        if(LUNSEE.gt.0) then
          call FLIBB (3,jummy,qummy,LUNSEE)
        else
          call FLABB (3,jummy,qummy)
        end if
      end if
C     !END
C
      return
      end
