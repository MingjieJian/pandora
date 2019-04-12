      subroutine RAJA
     $(X,W,NW,WAVES,SCON,OPAC,IPRNT,IJECT,NO)
C
C     Rudolf Loeser, 1980 Jun 19
C---- Computes Intensity and Flux using spherical coordinates,
C     for selected continuum wavelengths.
C     (This is version 3 of RAJA.)
C     !DASH
      save
C     !DASH
      real*8 DSK, OPAC, R1N, SCON, SHL, T, W, WAVES, X, ZERO
      integer IAD, IAS, ICS, IDP, IDR, IDX, IEM, IESAVS, IFD, IFF,
     $        IFSAVS, IFX, IGX, IIFSD, IISAVD, IISAVS, IJECT, IN, IPRNT,
     $        IQAVK, IQESD, IR, IS, ITX, IXI, J, JJFRR, JJZ, MOX, MRR,
     $        N, NECLP, NO, NW
      logical DISK, DUMP
      character TIT*9
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(135),JJFRR)
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
      equivalence (KZQ(139),NECLP)
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
      equivalence (IQQ(303),IQAVK)
C     !DASH
      external BERTRAM, SARACEN, GALLETA, ELECTRA, ACAMAS, BRUNO, RANI,
     $         LUPINE, MESQUIT, DEXTER, MARCUS, TOBOSA, PYXIE, MELILOT,
     $         VINCI, ADAPA, MESHED, MASHED, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               WAVES(NW), SCON(N,NW), OPAC(N,NW), IPRNT(NW)
      dimension WAVES(*),  SCON(N,*),  OPAC(N,*),  IPRNT(*)
C
      dimension IN(19)
      equivalence
     $(IN( 1),IDX   ),(IN( 2),IFX   ),(IN( 3),IGX   ),(IN( 4),IAS   ),
     $(IN( 5),IR    ),(IN( 6),IFF   ),(IN( 7),IXI   ),(IN( 8),ICS   ),
     $(IN( 9),IDR   ),(IN(10),IAD   ),(IN(11),IEM   ),(IN(12),IDP   ),
     $(IN(13),IFD   ),(IN(14),IIFSD ),(IN(15),ITX   ),(IN(16),IISAVS),
     $(IN(17),IESAVS),(IN(18),IFSAVS),(IN(19),IISAVD)
C
      data TIT /'"Eclipse"'/
C
      call HI ('RAJA')
C     !BEG
C     (Get, and allocate, W allotment)
      call MARCUS (IN, IS, MOX, 'RAJA')
C
      DISK = MRR.gt.0
      DUMP = IQESD.gt.0
C     !EJECT
C---- Write general heading
      call GALLETA      (NO, IJECT)
C---- Compute Shell integration weights
      call ELECTRA      (N, R1N, X(JJZ), W(IR), W(IAS))
C---- Compute Disk integration weights
      if(DISK) then
        call MELILOT    (MRR, X(JJFRR), R1N, W(IDP), W(IDR), W(IAD))
      end if
C---- Set up printing selections
      call DEXTER       (IPRNT, NW, NO, NECLP)
      if(DUMP) then
        call MESHED     ('RAJA', 2)
        call ACAMAS     (N, X(JJZ), R1N, MRR, X(JJFRR), W(IR), W(IAS),
     $                   W(IDP), W(IDR), W(IAD), TIT)
      end if
C---- Loop over all wavelengths
      do 100 J = 1,NW
        if(DUMP) then
          call ADAPA   (WAVES(J), OPAC(1,J), SCON(1,J), N, TIT)
        end if
C----   Compute Shell intensity
        call RANI      (N, R1N, X(JJZ), OPAC(1,J), SCON(1,J), W(IXI),
     $                  W(IDX), W(IFX), W(IGX), W(ITX), WAVES(J), 1,
     $                  DUMP)
C----   Computed integrated Shell intensity
        call SARACEN   (N, X(JJZ), W(IXI), W(ICS))
C----   Compute Shell flux
        call TOBOSA    (N, W(IAS), W(IXI), W(IFF), SHL)
C----   Print
        call MESQUIT   (IPRNT(J), WAVES(J), R1N, N, X(JJZ), OPAC(1,J),
     $                  SCON(1,J), W(IXI), W(ICS), W(IFF), SHL)
        if(DISK) then
C----     Compute Disk intensity
          call PYXIE   (X(JJFRR), MRR, X(JJZ), OPAC(1,J), SCON(1,J),
     $                  N, W(IDX), W(IFX), W(IGX), W(ITX), W(IEM),
     $                  WAVES(J), TIT, W, DUMP)
C----     Compute Disk flux
          call TOBOSA  (MRR, W(IAD), W(IEM), W(IFD), DSK)
C----     Print
          call LUPINE  (IPRNT(J), MRR, X(JJFRR), W(IDP), W(IDR),
     $                  W(IAD), W(IEM), DSK)
        else
          DSK = ZERO
        end if
C----   Save for "average continuum"
        if(IQAVK.gt.0) then
          call BERTRAM (J, N, DISK, MRR, W(IXI), W(ICS), W(IFF),
     $                  W(IEM), W(IISAVS), W(IESAVS), W(IFSAVS),
     $                  W(IISAVD))
        end if
C----   Compute and print total flux
        call VINCI     (IPRNT(J), SHL, DSK, R1N, T)
  100 continue
C     !EJECT
C---- Compute and print "average continuum"
      if(IQAVK.gt.0) then
        call BRUNO  (NW, N, DISK, MRR, R1N, WAVES, W(IISAVS),
     $               W(IESAVS), W(IFSAVS), W(IISAVD), W(IIFSD),
     $               X(JJZ), X(JJFRR), NO)
      end if
C
      if(DUMP) then
        call MASHED ('RAJA')
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'RAJA')
C     !END
      call BYE ('RAJA')
C
      return
      end
