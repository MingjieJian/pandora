      subroutine DEANNA
     $(X,W,IW,XCBL,KODE,KTRU,MOVING,SPHERE,INCDNT,JNUMTH,TIN,TOTIME)
C
C     Rudolf Loeser, 1973 Feb 07
C---- Provides Continuum Data printouts.
C
C     KODE = 3 for current line (PRD, FDB)
C     KODE = 2 for "Lyman"
C     KODE = 1 everything else
C
C     XCBL is the buffer for the Continuum Data Block.
C
C     See also DION and SHAKE.
C     !DASH
      save
C     !DASH
      real*8 TIN, TOTIME, TOUT, W, X, XCBL, XLM
      integer IBHSL, IBL, ICL, ICSB, ICSO, IFLAGB, IFLAGO, IISWA, IISWE,
     $        IJBHS, IJOP, IJOPAC, IL, IN, IP, IS, ISL, ISS1, ISSR, ITS,
     $        ITS1, ITSR, IU, IW, IWS, IYNUL, IZL, JJFRS, JJTE, JJZ, JN,
     $        JNUMTH, KISLV, KJCNXP, KJFD, KJKONT, KJOPAC, KJSCON,
     $        KJTAUK, KKACTB, KKACTO, KKALBD, KKB, KKBHS, KKBHSD,
     $        KKBHSN, KKBULT, KKCAPP, KKCB, KKCO, KKDAMP, KKISLV,
     $        KKISWA, KKISWE, KKITS, KKJNU, KKLAMD, KKLTIT, KKMULT,
     $        KKRESN, KKS1, KKSCAT, KKSIGM, KKSIGS, KKSR, KKT1, KKTR,
     $        KODE, KRESN, KTRU, KTYPE, LU, LUS, LUT, MOX, MUX, N
      logical DOIT, INCDNT, LINE, MOVING, PRNT, SPHERE, lummy
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(138),JJFRS)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 1),KKLTIT)
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK( 4),KKDAMP)
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK( 7),KKSCAT)
      equivalence (KKK( 8),KKBHSN)
      equivalence (KKK( 9),KKBHSD)
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(13),KKJNU )
      equivalence (KKK(15),KKB   )
      equivalence (KKK(18),KKACTO)
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(20),KKACTB)
      equivalence (KKK(21),KKCB  )
      equivalence (KKK(32),KKRESN)
      equivalence (KKK(22),KKT1  )
      equivalence (KKK(23),KKTR  )
      equivalence (KKK(24),KKS1  )
      equivalence (KKK(25),KKSR  )
      equivalence (KKK(33),KKISLV)
      equivalence (KKK(39),KKBULT)
      equivalence (KKK(31),KKALBD)
      equivalence (KKK(42),KKITS )
      equivalence (KKK(43),KKISWA)
      equivalence (KKK(44),KKISWE)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK(30),KKSIGM)
      equivalence (KKK(45),KKSIGS)
C     !DASH
C     !EJECT
      external  NICKY, MANSUR, CONRAD, CONTDI, MUGWORT, SHARON, PADDLE,
     $          HEN, SHORE, RING, SECOND, IGIVE, WGIVE, HI, BYE
      intrinsic abs
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension LUT(6)
C
      dimension IN(15)
      equivalence
     $(IN( 1),IP    ),(IN( 2),ICSO  ),(IN (3),ICSB  ),(IN( 4),IZL   ),
     $(IN( 5),ISL   ),(IN( 6),IBL   ),(IN( 7),IBHSL ),(IN( 8),IYNUL ),
     $(IN( 9),ITS1  ),(IN(10),ITSR  ),(IN(11),ISS1  ),(IN(12),ISSR  ),
     $(IN(13),ICL   ),(IN(14),IJOPAC),(IN(15),IJBHS )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IISWA ),(JN( 2),IISWE ),(JN( 3),IJOP  )
C
      data KTYPE /0/
C
      call HI ('DEANNA')
C     !BEG
C---- Get LU and LUTs, and find out whether printing should occur
      call SHORE       (LU, LUT, PRNT)
      call HEN         (XCBL(KKLTIT), LUS, IU, IL)
      DOIT = PRNT.or.(LUS.gt.0)
C
      if(.not.DOIT) then
        call SECOND    (TOUT)
        TOTIME = TOUT-TIN
      else
C     !EJECT
C       (Get, and allocate, W & IW allotments)
        call NICKY     (IN, IS , MOX, 'DEANNA')
        call MANSUR    (JN, IWS, MUX, 'DEANNA')
C
        XLM = abs(XCBL(KKLAMD))
C
C----   Get B block indices depending on KODE.
        call CONRAD    (KTRU, KJTAUK, KJSCON, KJOPAC, KJCNXP, KJFD,
     $                  KJKONT)
        if(PRNT) then
C----     Convert switches back to integer form
          IFLAGO = XCBL(KKACTO)
          IFLAGB = XCBL(KKACTB)
          KISLV  = XCBL(KKISLV)
          KRESN  = XCBL(KKRESN)
          ITS    = XCBL(KKITS)
          call CONTDI  (XCBL(KKISWA), 1, NOPAC, IW(IISWA), 1, NOPAC)
          call CONTDI  (XCBL(KKISWE), 1, NOPAC, IW(IISWE), 1, NOPAC)
          call CONTDI  (XCBL(KJKONT), 1, NOPAC, IW(IJOP ), 1, NOPAC)
C----     Edit COPAC and CBHS, to set contributors =0 whose KOPAC =0
          call MUGWORT (XCBL(KKCO), IW(IJOP), N, NOPAC, W(IJOPAC))
          call MUGWORT (XCBL(KKCB), IW(IJOP), N, NOPAC, W(IJBHS ))
C----     Get LINE switch
          call PADDLE  (KTYPE, LINE, lummy)
C----     Regular printout
          call SHARON  (XLM, LU, LUT, N, NOPAC, XCBL(KKMULT),
     $                  XCBL(KKLTIT), XCBL(KKDAMP), XCBL(KKCAPP),
     $                  XCBL(KJOPAC), XCBL(KKSIGS), XCBL(KKSCAT),
     $                  XCBL(KKBHSN), XCBL(KKBHSD), XCBL(KKBHS),
     $                  X(JJZ), XCBL(KJTAUK), XCBL(KKJNU),
     $                  XCBL(KJSCON), XCBL(KKB), X(JJTE), XCBL(KJFD),
     $                  IFLAGO, W(IJOPAC), IFLAGB, W(IJBHS), W(IP),
     $                  W(ICSO), W(ICSB), W(IZL), W(ISL), W(IBL),
     $                  W(IBHSL), W(IYNUL), KRESN, XCBL(KKT1),
     $                  XCBL(KKTR), XCBL(KKS1), XCBL(KKSR), W(ITS1),
     $                  W(ITSR), W(ISS1), W(ISSR), KISLV, ITS,
     $                  IW(IISWA), IW(IISWE), XCBL(KJCNXP), TIN,
     $                  MOVING, SPHERE, INCDNT, JNUMTH, X(JJFRS),
     $                  W(ICL), XCBL(KKALBD), XCBL(KKBULT), TOTIME,
     $                  KODE, KTRU, LINE)
        end if
        if((LUS.gt.0).and.(KTRU.eq.0)) then
C----     Special printout of line-center data (if needed)
          call RING    (N, XLM, LUS, IU, IL, XCBL(KKMULT),
     $                  XCBL(KKCAPP), XCBL(KKSIGM), XCBL(KKSCAT),
     $                  XCBL(KJOPAC), XCBL(KKBHS), XCBL(KKJNU),
     $                  XCBL(KJSCON))
        end if
C
C       (Give back W & IW allotments)
        call WGIVE     (W,  'DEANNA')
        call IGIVE     (IW, 'DEANNA')
      end if
C     !END
      call BYE ('DEANNA')
C
      return
      end
