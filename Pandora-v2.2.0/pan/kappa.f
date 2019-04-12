      subroutine KAPPA
     $(NO,NWV,WAVES,YWAVE,WAVMN,WAVMX,NECLP)
C
C     Rudolf Loeser, 2002 Aug 16
C---- Prints additional wavelengths.
C     (This is version 2 of KAPPA.)
C     !DASH
      save
C     !DASH
      real*8 WAVES, WAVMN, WAVMX, YWAVE, ZERO
      integer IQAVK, IQECL, KNEG, NECLP, NO, NWV
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
      equivalence (IQQ(  6),IQECL)
      equivalence (IQQ(303),IQAVK)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LINER, OSMOND, HI, BYE
C
C               WAVES(NWV), YWAVE(NWV)
      dimension WAVES(*),   YWAVE(*)
C     !EJECT
C
      call HI ('KAPPA')
C     !BEG
      if((NO.gt.0).and.(NWV.gt.0)) then
C
        call OSMOND  (NO,NWV,WAVES,YWAVE,KNEG)
C
        if((WAVMN.gt.ZERO).and.(WAVMX.gt.WAVMN)) then
          call LINER (2,NO)
          write (NO,100) WAVMN,WAVMX
  100     format(' ','WAVEMN =',1PE16.8,5X,'WAVEMX =',E16.8)
        end if
C
        if((KNEG.gt.0).and.(IQECL.gt.0).and.(IQAVK.gt.0)) then
          call LINER (1,NO)
          write (NO,101) NECLP
  101     format(' ','NECLIP =',I4)
        end if
      end if
C     !END
      call BYE ('KAPPA')
C
      return
      end
