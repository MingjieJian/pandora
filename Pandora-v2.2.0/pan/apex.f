      subroutine APEX
     $(NO,NW,WAVES,WVNUM,WTAB,XMULT,LTYP,SPHERE,TF,SF,XLTIT,IJECT,LFB,
     $ LINFLX,LINK,XLB3,W)
C
C     Rudolf Loeser, 1981 Mar 17
C---- Continuum FLUX Summary printout, saving and plots.
C     (This is version 3 of APEX.)
C     !DASH
      save
C     !DASH
      real*8 ADMAS, ADS, R1N, SF, TF, W, WAVES, WREF, WTAB, WVNUM, XLB3,
     $       XLTIT, XMULT
      integer IFA, IFHZ, IFMM, IFR, IJECT, IN, IS, ITB, IXLA, IXMG, IZ,
     $        LFB, LINK, LTYP, MOX, NO, NW
      logical LINFLX, SPHERE
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
      equivalence (RZQ( 23),R1N  )
      equivalence (RZQ(  1),ADS  )
      equivalence (RZQ( 95),ADMAS)
C     !DASH
C     !EJECT
      external IVIRI, KLING, MONSTER, SNIPE, FEATHER, KLUNG, NARKE,
     $         BOLO, WGIVE, HI, BYE
C
      dimension W(*)
C
C               WAVES(Nmkuse), XMULT(Nmkuse), LTYP(Nmkuse), TF(Nmkuse),
      dimension WAVES(*),      XMULT(*),      LTYP(*),      TF(*),
C
C               XLTIT(Nmkuse), WTAB(Nmkuse), WVNUM(Nmkuse), SF(Nmkuse),
     $          XLTIT(*),      WTAB(*),      WVNUM(*),      SF(*),
C
C               XLB3(Li3len)
     $          XLB3(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IFHZ  ),(IN( 2),IFA   ),(IN( 3),IXLA  ),(IN( 4),ITB   ),
     $(IN( 5),IFMM  ),(IN( 6),IXMG  ),(IN( 7),IFR   ),(IN( 8),IZ    )
C
      call HI ('APEX')
C     !BEG
C     (Get, and allocate, W allotment)
      call IVIRI   (IN, IS, MOX, 'APEX', NW)
C
C---- Compute derived quantities
      call KLING   (SPHERE, ADS, R1N, NW, WAVES, TF, W(IFR), W(IFHZ),
     $              W(IFA), W(IXLA), W(ITB), W(IFMM), W(IXMG), WREF,
     $              W(IZ))
C---- Print header
      call BOLO    (NO, ADS, ADMAS, R1N, SPHERE, LFB, LINK, IJECT)
C---- Print results
      call SNIPE   (NO, NW, SPHERE, XMULT, LTYP, XLTIT, WAVES, WTAB,
     $              W(IFHZ), W(IFA), W(ITB), TF, SF)
      if(LINK.eq.3) then
C----   Save for line profiles calculation
        call NARKE (NW, WAVES, LTYP, XLTIT, W(ITB), W(IFHZ), W(IFA),
     $              XLB3, LFB)
      end if
C---- Save results in special Spectrum Save file
      call FEATHER (NW, WAVES, TF, SF, XMULT, LTYP, LFB, WVNUM,
     $              LINFLX)
C---- Save checksum
      call MONSTER (TF, NW, LFB, LINFLX, LINK)
C---- Plot FHZ and FMM
      call KLUNG   (NO, NW, W(IFR), WTAB, W(IFHZ), W(IFMM), LFB,
     $              LINFLX)
C
C     (Give back W allotment)
      call WGIVE   (W, 'APEX')
C     !END
      call BYE ('APEX')
C
      return
      end
