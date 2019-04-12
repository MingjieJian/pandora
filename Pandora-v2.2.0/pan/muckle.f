      subroutine MUCKLE
     $(I,MLAB)
C
C     Rudolf Loeser, 2005 Sep 22
C---- Edits IQQ ! ! , and
C                                  encodes option status for printing.
C     !DASH
      save
C     !DASH
      integer I, KIND, KODE
      character LAB*4, LOB*4, MLAB*4
C     !COM
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
C     !DASH
C     !EJECT
      external HI, BYE
C
      dimension LAB(4), LOB(4)
C
      data LAB /' OFF', ' ON ', '*OFF', '*ON '/
      data LOB /' off', ' on ', '*off', '*on '/
C
      call HI ('MUCKLE')
C     !BEG
      KIND = IQQ(I)
      KODE = 1
      if(KIND.ge.10) then
        IQQ(I) = IQQ(I)-10
C       (See JIGGLE !)
        KIND = KIND-10
        KODE = 2
      end if
      if(KIND.eq.IQD(I)) then
        KIND = KIND+2
      end if
      if(KODE.eq.1) then
        MLAB = LOB(KIND+1)
      else
        MLAB = LAB(KIND+1)
      end if
C     !END
      call BYE ('MUCKLE')
C
      return
      end
