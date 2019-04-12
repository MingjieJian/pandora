      subroutine SNAKE
     $(EMU,EMUF,VXN,CVX,ZECL,ISSV,HNDF,FNH)
C
C     Rudolf Loeser, 1986 Jan 23
C---- Controls the reading of the 3. batch of input statements -
C     Spectrum Calculations input.
C     (This is version 3 of SNAKE.)
C     !DASH
      save
C     !DASH
      real*8 CORMN, CORMX, CVX, CVXF, CVXM, EMU, EMUF, FBVMX, FNH, HNDF,
     $       SCPS, SCVA, SCVB, SCVS, VXN, WFB, ZECL, dummy
      integer IPNT, ISSV, KERR, KIND, L, LF, LL, LOOK, LPMLR, LPVEL,
     $        LUEO, LV, MODE, N, NFH, NVX, NZE, jummy
      character QLIST*8, QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 7),L  )
      equivalence (JZQ(19),LF )
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
      equivalence (JZQ(56),NZE)
      equivalence (JZQ(31),NFH)
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
      equivalence (RZQ(145),CORMN)
      equivalence (RZQ(146),CORMX)
      equivalence (RZQ(170),SCVA )
      equivalence (RZQ(171),SCVS )
      equivalence (RZQ(174),SCVB )
      equivalence (RZQ(175),SCPS )
      equivalence (RZQ( 65),CVXM )
      equivalence (RZQ( 67),CVXF )
      equivalence (RZQ( 68),WFB  )
      equivalence (RZQ(110),FBVMX)
      equivalence (KZQ(217),LPVEL)
      equivalence (KZQ(218),LPMLR)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C     !DASH
      external CARMEN, LOOKUC, CHIRON, KIWI, BASIL, UNMIX, WITCH, MINT,
     $         SINGC, LINER, CHLOE, ABORT, CENTAUR, CILENTO, HI, BYE
C
C               VXN(N,NVX), HNDF(NFH), ISSV(NVX), EMUF(LF), ZECL(NZE),
      dimension VXN(N,*),   HNDF(*),   ISSV(*),   EMUF(*),  ZECL(*),
C
C               EMU(L), CVX(NVX), FNH(NFH)
     $          EMU(*), CVX(*),   FNH(*)
C
      parameter (LL=23)
      dimension QLIST(LL), IPNT(LL)
C
      data QLIST /'USE', 'MU', 'MUF', 'VX', 'CVX', 'FILE', 'ZECL',
     $            'CORMIN', 'CORMAX', 'ISSV', 'SCVA', 'SCVS', 'SCVB',
     $            'SCPS', 'HNDF', 'FNH', 'CVXM', 'CVXF', 'WFB',
     $            'FBVMX', 'LPVEL', 'LPMLR', 'GO'/
C
      call HI ('SNAKE')
C     !BEG
      KERR = 0
C---- Read next control field
  100 continue
      call KIWI    (MODE, dummy, jummy, QNAME, jummy)
      if(MODE.ne.2) goto 201
      call UNMIX   (QNAME)
C
C---- Identify this control field
      call LOOKUC  (QLIST, LL, QNAME, KIND, LOOK)
      if(LOOK.ne.1) goto 202
      goto (101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
     $      113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 199), KIND
C     !EJECT
  101 continue
C----   "USE" - Input file specifier
        call CHIRON
        if(ESCARGO) goto 199
        goto 100
  102 continue
C----   "EMU" - Look angles for line profiles
        call BASIL (EMU, L, QNAME)
        if(ESCARGO) goto 199
        goto 100
  103 continue
C----   "EMUF" - Look angles for flux profile integral
        call BASIL (EMUF, LF, QNAME)
        if(ESCARGO) goto 199
        goto 100
  104 continue
C----   "VX" - Additional expansion velocities
        call MINT  (QNAME, LV)
        if((LV.lt.1).or.(LV.gt.NVX)) then
          call ABORT
        end if
        call BASIL (VXN(1,LV), N, QNAME)
        if(ESCARGO) goto 199
        goto 100
  105 continue
C----   "CVX" - velocity generating parameters
        call BASIL (CVX, NVX, QNAME)
        if(ESCARGO) goto 199
        goto 100
  106 continue
C----   "FILE" - filespec of "general" input file
        call CENTAUR
        if(ESCARGO) goto 199
        goto 100
  107 continue
C----   "ZECL" - selected Z's for Continuum Eclipse
        call BASIL   (ZECL, NZE, QNAME)
        if(ESCARGO) goto 199
        goto 100
  108 continue
C----   "CORMIN" - limit for ORIGINS / CONTRIBUTORS
        call BASIL   (CORMN, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  109 continue
C----   "CORMAX" - limit for ORIGINS / CONTRIBUTORS
        call BASIL   (CORMX, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  110 continue
C----   "ISSV" - shock velocity depth index
        call CILENTO (ISSV, NVX, QNAME)
        if(ESCARGO) goto 199
        goto 100
  111 continue
C----   "SCVA" - shock velocity amplitude
        call BASIL   (SCVA, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  112 continue
C----   "SCVS" - shock velocity scale height
        call BASIL   (SCVS, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  113 continue
C----   "SCVB" - shock velocity amplitude
        call BASIL   (SCVB, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  114 continue
C----   "SCPS" - shock velocity scale height
        call BASIL   (SCPS, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  115 continue
C----   "HNDF" - hydrogen density table for FNH
        call BASIL   (HNDF, NFH, QNAME)
        if(ESCARGO) goto 199
        goto 100
  116 continue
C----   "FNH" - standard table of flow velocity
        call BASIL   (FNH, NFH, QNAME)
        if(ESCARGO) goto 199
        goto 100
  117 continue
C----   "CVXM" - flow broadening velocity parameter
        call BASIL   (CVXM, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  118 continue
C----   "CVXF" - flow broadening velocity parameter
        call BASIL   (CVXF, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  119 continue
C----   "WFB" - weight for flow broadening
        call BASIL   (WFB, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  120 continue
C----   "FBVMX" - velocity limit for flow broadening
        call BASIL   (FBVMX, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  121 continue
C----   "LPVEL" - profile velocities print switch
        call CILENTO (LPVEL, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
  122 continue
C----   "LPMLR" - mass loss rates print switch
        call CILENTO (LPMLR, 1, QNAME)
        if(ESCARGO) goto 199
        goto 100
C     !EJECT
C---- Error messages
  202 KERR = KERR+1
  201 KERR = KERR+1
C
      call SINGC (QLIST, LL, jummy, IPNT)
C
      call LINER (3, LUEO)
      write (LUEO,200) QLIST
  200 format(' ','Trouble in SNAKE: reading  PART F  input.'//
     $       ' ','List of valid control fields:'//
     $      (' ',12A10))
C
      call CHLOE (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
      if(ESCARGO) goto 199
      goto 100
C
C---- Go home
  199 continue
C     !END
      call BYE ('SNAKE')
C
      return
      end
