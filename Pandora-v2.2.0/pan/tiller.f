      subroutine TILLER
     $(NO,F,ITS,ITMSS)
C
C     Rudolf Loeser, 2003 May 07
C---- Prints parameters pertaining to CSF printout.
C     !DASH
      save
C     !DASH
      real*8 F, TSM
      integer IQEXA, IQSFS, IQUTM, ITS, NO
      character ITMSS*9, TTSM*12
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
      equivalence (RZQ( 21),TSM  )
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ(321),IQUTM)
C     !DASH
      external LINER, HI, BYE
C
      dimension  TTSM(2)
C     !EJECT
C
      call HI ('TILLER')
C     !BEG
      if(NO.gt.0) then
        write (ITMSS,100) ITS
  100   format('Iter: ',I3)
C
        if((IQSFS.gt.0).or.(IQEXA.gt.0)) then
          write (NO,101) F,ITMSS
  101     format(' ',5X,'Multiplier',1PE12.4,37X,A9)
        else
C
          TTSM(1) = '     TSM    '
          if(IQUTM.gt.0) then
            write (TTSM(2),102) TSM
  102       format(1PE12.4)
          else
            TTSM(2) = '  (not used)'
          end if
          write (NO,103) F,TTSM(1),ITMSS,TTSM(2)
  103     format(' ',5X,'Multiplier',1PE12.4,11X,A12,14X,A9/
     $           ' ',37X,A12)
          call LINER (1, NO)
          if(IQUTM.le.0) then
            write (NO,104) '; ',TSM
  104       format(' ','(Note that neglect of small TAU values ',
     $                 'depends on option USETSM',A,:,'TSM =',
     $                 1PE12.4,'.)')
          else
            write (NO,104) '.)'
          end if
        end if
      end if
C     !END
      call BYE ('TILLER')
C
      return
      end
