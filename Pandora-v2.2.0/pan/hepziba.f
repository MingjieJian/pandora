      subroutine HEPZIBA
     $(NO,LDU,XKDST,TDUST,XLMD,DFD,ALD,EPD,DDT,MDTR1,MDTR2,NDT,XLDT,
     $ YLDT,ADT,ALBDT,N,Z,TDT,WTD,YFLUX,TLTR)
C
C     Rudolf Loeser, 1981 May 07
C---- Prints dust parameters.
C     (This is version 2 of HEPZIBA.)
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, ALD, DDT, DFD, EPD, TDT, TDUST, TLTR, WTD,
     $       XKDST, XLDT, XLMD, YFLUX, YLDT, Z, ZERO
      integer IQDT2, LDU, MDTR1, MDTR2, N, NDT, NO
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
      equivalence (IQQ( 99),IQDT2)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external PADMA, LINER, RAMMAN, JATAKA, HI, BYE
C
C               XLMD(LDU), DFD(LDU), XLDT(LDT), EPD(LDU), TDT(N), Z(N),
      dimension XLMD(*),   DFD(*),   XLDT(*),   EPD(*),   TDT(*), Z(*),
C
C               YLDT(LDT), ADT(LDT), ALD(LDU), ALBDT(LDT)
     $          YLDT(*),   ADT(*),   ALD(*),   ALBDT(*)
C     !EJECT
C
      call HI ('HEPZIBA')
C     !BEG
      if((XKDST.gt.ZERO).and.(NO.gt.0)) then
        call PADMA    (NO,'Dust Parameters')
C
        write (NO,100) XKDST
  100   format(' ','KDUST',1PE14.6)
        call LINER    (2,NO)
C
        if(IQDT2.le.0) then
          call RAMMAN (NO,TDUST,XLMD,DFD,ALD,EPD,LDU)
        else
          call JATAKA (NO,XLDT,YLDT,ADT,ALBDT,Z,TDT,NDT,N,DDT,
     $                 MDTR1,MDTR2,WTD,YFLUX,TLTR)
        end if
C
      end if
C     !END
      call BYE ('HEPZIBA')
C
      return
      end
