      subroutine BERSERK
     $(X,W,IW,NTMX,STAB,IPNT,NT)
C
C     Rudolf Loeser, 2002 Sep 26
C---- Supervises production of a table of standard rates integrations
C     wavelengths.
C     !DASH
      save
C     !DASH
      real*8 STAB, W, X
      integer IPNT, IQPWT, IW, NT, NTMX
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
      equivalence (IQQ( 82),IQPWT)
C     !DASH
      external TRAVIS, SORT, GLEWY, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               STAB(NTMX), IPNT(NTMX)
      dimension STAB(*),    IPNT(*)
C
      call HI ('BERSERK')
C     !BEG
C---- Make table
      call TRAVIS  (X, NTMX, STAB, NT)
      if(IQPWT.gt.0) then
C----   Sort
        call SORT  (STAB, NT, IPNT, 'Standard Rates Wavelengths')
C----   Print
        call GLEWY (STAB, NT)
      end if
C     !END
      call BYE ('BERSERK')
C
      return
      end
