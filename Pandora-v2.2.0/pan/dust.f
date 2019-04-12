      subroutine DUST
     $(LDU,XLMDUST,DFDUST,ALBDUST,NDT,XLDT,ADT,ALBDT,XFRQ,KILROY,XLM,
     $ COEFF,ALBEDO)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes the wavelength-dependent coefficient and albedo
C     for dust absorption and scattering.
C     (This is version 2 of DUST.)
C     !DASH
      save
C     !DASH
      real*8 AD, ADT, ALBDT, ALBDUST, ALBEDO, COEFF, DFDUST, PAD, TEN,
     $       XFRQ, XKDST, XLDT, XLM, XLMDUST, ZERO
      integer IQDT2, LDU, NDT, jummy
      logical KILROY
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
      equivalence (RZQ( 28),XKDST)
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
      equivalence (IQQ( 99),IQDT2)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external LININT, DAFT, DEFT, MOVE1, RECIPRO, HI, BYE
C
C               XLMDUST(LDU), DFDUST(LDU), ALBDUST(LDU), ALBDT(NDT),
      dimension XLMDUST(*),   DFDUST(*),   ALBDUST(*),   ALBDT(*),
C
C               XLDT(NDT), ADT(NDT), XFRQ(NDT)
     $          XLDT(*),   ADT(*),   XFRQ(*)
C
      call HI ('DUST')
C     !BEG
      if(XKDST.le.ZERO) then
        COEFF  = ZERO
        ALBEDO = ZERO
      else
        if(COEFF.eq.ZERO) then
          if(IQDT2.le.0) then
C----       Type-1
            call LININT    (XLMDUST, 1, ALBDUST, 1, LDU, XLM, ALBEDO,
     $                      1, 1, jummy)
            call DAFT      (XLM, XLMDUST, DFDUST, LDU, PAD)
            AD = TEN**PAD
          else
C----       Type-2
            if(KILROY) then
              KILROY = .false.
              call MOVE1   (XLDT, NDT, XFRQ)
              call RECIPRO (XFRQ, NDT)
            end if
            call DEFT      (XLM, XFRQ, ALBDT, NDT, ALBEDO)
            call DEFT      (XLM, XFRQ, ADT, NDT, AD)
          end if
C
          COEFF = XKDST*AD
        end if
      end if
C     !END
      call BYE ('DUST')
C
      return
      end
