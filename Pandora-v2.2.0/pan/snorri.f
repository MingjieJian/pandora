      subroutine SNORRI
     $(LU,KODE)
C
C     Rudolf Loeser, 2003 May 06
C---- Prints a legend on intensity printouts.
C     KODE =0: Line;   =1: Continuum.
C
C     (This is version 2 of SNORRI.)
C     !DASH
      save
C     !DASH
      integer IQAPP, IQEID, IQLID, ISMBD, KODE, LU
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 92),ISMBD)
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
      equivalence (IQQ(261),IQAPP)
      equivalence (IQQ( 58),IQEID)
      equivalence (IQQ( 57),IQLID)
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('SNORRI')
C     !BEG
      if((LU.gt.0).and.(IQAPP.le.0)) then
C
        if(ISMBD.le.0) then
          call LINER (1,LU)
          write (LU,100)
  100     format(' ','A dump of details of every ISMBD''th intensity ',
     $               'integration (summation) can be obtained by ',
     $               'setting ISMBD .gt. 0.')
          if(KODE.eq.1) then
            if(IQEID.le.0) then
              write (LU,101) 'EMINDMP',' also'
  101         format(' ','Option ',A,' can',A,' be used to obtain ',
     $                   'detailed dumps of the intensity calculation.')
            end if
          else
            if(IQLID.le.0) then
              write (LU,101) 'LINTDMP',' also'
             end if
          end if
C
        else
          if(KODE.eq.1) then
            if(IQEID.le.0) then
              write (LU,101) 'EMINDMP',' '
            end if
          else
            if(IQLID.le.0) then
              write (LU,101) 'LINTDMP',' '
             end if
          end if
        end if
C
      end if
C     !END
      call BYE ('SNORRI')
C
      return
      end
