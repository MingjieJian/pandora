      subroutine EDEKI
     $(NVX)
C
C     Rudolf Loeser, 1989 Oct 24
C---- Adjusts NVX as necessary.
C     !DASH
      save
C     !DASH
      integer IQAMD, IQEXA, IQFLW, IQSFO, IQSFS, IQVLS, NFB, NVX
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 9),NFB)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ(265),IQVLS)
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ(335),IQFLW)
      equivalence (IQQ(  8),IQSFO)
      equivalence (IQQ( 31),IQSFS)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  HALT, HI, BYE
      intrinsic max
C
      call HI ('EDEKI')
C     !BEG
      if(IQAMD.gt.0) then
C----   Make sure that there is room for at least one "additional
C       expansion velocity", if called for (diffusion calculation).
        if((IQVLS.le.0).and.(IQFLW.le.0)) then
          NVX = max(NVX,1)
        end if
      end if
C
      if(IQFLW.gt.0) then
        if(IQEXA.gt.0) then
          write (MSSLIN(1),100) 'EXPAND'
  100     format('FLOWBROAD = on is not allowed when ', A,'= on.')
          call HALT ('EDEKI', 1)
        end if
        if(IQSFO.gt.0) then
          write (MSSLIN(1),100) 'SPHOUT'
          call HALT ('EDEKI', 1)
        end if
        if(IQSFS.gt.0) then
          write (MSSLIN(1),100) 'SPHERE'
          call HALT ('EDEKI', 1)
        end if
C
        if(NVX.ne.0) then
          write (MSSLIN(1),101) NVX
  101     format('Input value NVX =',I12,' not allowed when ',
     $           'FLOWBROAD = on; MUST remain = 0.')
          call HALT ('EDEKI', 1)
        end if
        if(NFB.le.0) then
          write (MSSLIN(1),102) NFB
  102     format('A non-zero value of NFB must be specified when ',
     $           'FLOWBROAD = on.')
          call HALT ('EDEKI', 1)
        end if
C
        NVX = 2*NFB+2
C       (The "+2" insures that storage will be reserved for two
C       CVXF-velocities if needed; NVX will be adjusted later
C       [in BUSKER] to reflect actual need.)
      end if
C     !END
      call BYE ('EDEKI')
C
      return
      end
