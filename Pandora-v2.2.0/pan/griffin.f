      subroutine GRIFFIN
     $(X,W,NW,WAVES,WVNUM,WTAB,INDX,SCON,OPAC,IJECT,NO)
C
C     Rudolf Loeser, 1993 Jun 15
C---- Computes emergent continuum intensity using spherical coordinates,
C     for selected wavelengths, for selected rays, and
C     prints, and saves them in the Special Spectrum Save file.
C     !DASH
      save
C     !DASH
      real*8 BMWAC, DELTA, OPAC, R1N, SCON, W, WAVES, WTAB, WVNUM, X,
     $       ZERO, dummy
      integer IAI, IAS, IBI, IBS, IDX, IFX, IGX, IJECT, IME, IN, INDX,
     $        INRML, IQESD, IR, IS, ITX, IWI, IWLG, IXI, IXS, J, JJZ,
     $        JJZEC, MOX, N, NO, NW, NZE
      logical BEAMS, DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(56),NZE)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(226),JJZEC)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 23),R1N  )
      equivalence (RZQ(140),BMWAC)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !EJECT
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
      equivalence (IQQ( 40),IQESD)
C     !DASH
      external MAURICE, GRAHAM, GASPAR, RANI, GODFREY, ELECTRA, GARRET,
     $         GORDON, GARY, GREGORY, MESHED, MASHED, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               WAVES(NW), SCON(N,NW), OPAC(N,NW), INDX(NZE), WTAB(NW),
      dimension WAVES(*),  SCON(N,*),  OPAC(N,*),  INDX(*),   WTAB(*),
C
C               WVNUM(NW)
     $          WVNUM(*)
C
      dimension IN(15)
      equivalence
     $(IN( 1),IR    ),(IN( 2),IAS   ),(IN( 3),IXI   ),(IN( 4),IDX   ),
     $(IN( 5),IFX   ),(IN( 6),IGX   ),(IN( 7),ITX   ),(IN( 8),IXS   ),
     $(IN( 9),IBS   ),(IN(10),IBI   ),(IN(11),IAI   ),(IN(12),IWLG  ),
     $(IN(13),IME   ),(IN(14),INRML ),(IN(15),IWI   )
C
      call HI ('GRIFFIN')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAURICE  (IN, IS, MOX, 'GRIFFIN', NW)
C
      BEAMS = BMWAC.gt.ZERO
      DUMP  = IQESD.gt.0
      if(DUMP) then
        call MESHED ('GRIFFIN', 2)
      end if
C     !EJECT
C---- Compute integration weights
      call ELECTRA     (N, R1N, X(JJZ), W(IR), W(IAS))
C---- Compute selection indices
      call GARY        (X(JJZ), N, X(JJZEC), INDX, NZE)
      if(BEAMS) then
C----   Compute Gaussians and their integrals
        call GARRET    (N, X(JJZ), NZE, INDX, BMWAC, DELTA, W(IME),
     $                  W(INRML), W(IAI))
      end if
C---- Loop over all wavelengths
      do 100 J = 1,NW
C----   Compute ray intensity
        call RANI      (N, R1N, X(JJZ), OPAC(1,J), SCON(1,J), W(IXI),
     $                  W(IDX), W(IFX), W(IGX), W(ITX), WAVES(J), 1,
     $                  DUMP)
C----   Extract and save selected values
        call GODFREY   (J, NW, W(IXI), INDX, NZE, W(IXS))
        if(BEAMS) then
C----     Compute beam intensity
          call GASPAR  (N, X(JJZ), NZE, INDX, W(IME), W(INRML),
     $                  W(IXI), W(IBI), W(IAI), W(IWI))
C----     Extract and save selected values
          call GODFREY (J, NW, W(IBI), INDX, NZE, W(IBS))
        end if
  100 continue
C
      if(NO.gt.0) then
C----   Print ray intensity
        call GREGORY   (NW, NZE, WTAB, W(IXS), INDX, X(JJZ), X(JJZEC),
     $                  1, dummy, dummy, IJECT, NO)
C----   Plot ray intensity
        call GORDON    (NW, NZE, WTAB, W(IWLG), W(IXS), NO)
        if(BEAMS) then
C----     Print beam intensity
          call GREGORY (NW, NZE, WTAB, W(IBS), INDX, dummy, dummy,
     $                  2, BMWAC, DELTA, IJECT, NO)
C----     Plot beam intensity
          call GORDON  (NW, NZE, WTAB, W(IWLG), W(IBS), NO)
        end if
      end if
C---- Save beam intensity
      call GRAHAM    (NW, NZE, WAVES, WVNUM, W(IBS), INDX, X(JJZ),
     $                BMWAC)
C
      if(DUMP) then
        call MASHED  ('GRIFFIN')
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'GRIFFIN')
C     !END
      call BYE ('GRIFFIN')
C
      return
      end
