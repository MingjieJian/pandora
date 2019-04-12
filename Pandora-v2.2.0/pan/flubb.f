      subroutine FLUBB
     $(JHB,CALLER)
C
C     Rudolf Loeser, 1996 Jul 23
C---- Hi/Bye/Abort system processing.
C     JHB = 1 for HI, =2 for BYE.
C     (This is version 2 of FLUBB.)
C     !DASH
      save
C     !DASH
      integer JC, JHB, jummy
      logical DO, EQCALL, EQPAND
      character CALLER*(*), qummy*8
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
      external  FLIBB, FLABB, TOAST
      intrinsic len
C
      data DO /.false./
C     !EJECT
C
C     !BEG
      JC = len(CALLER)
      EQCALL = HEADSEE(:JC).eq.CALLER
C
      if(.not.DO) then
        EQPAND = HEADSEE(:7).eq.'PANDORA'
        if((JHB.eq.1).and.(EQCALL.or.EQPAND)) then
          DO = .true.
          if(LUNSEE.gt.0) then
            call FLIBB (1, jummy, qummy, LUNSEE)
          else
            call FLABB (1, jummy, qummy)
          end if
        end if
      end if
C
      if(DO) then
        if(LUNSEE.gt.0) then
          call FLIBB   (2, JHB, CALLER, LUNSEE)
        else
          call FLABB   (2, JHB, CALLER)
        end if
        if((JHB.eq.2).and.EQCALL) then
          DO = .false.
          if(LUNSEE.gt.0) then
            call FLIBB (3, jummy, qummy, LUNSEE)
          else
            call FLABB (3, jummy, qummy)
          end if
        end if
        if((JHB.eq.2).and.DOCHK) then
          call TOAST   (CALLER)
        end if
      end if
C     !END
C
      return
      end
