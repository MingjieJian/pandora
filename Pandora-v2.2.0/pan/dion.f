      subroutine DION
     $(X,W,IW,XCBL,MOVING,TIN,TOTIME)
C
C     Rudolf Loeser, 2005 Mar 23
C---- Drives TERMES, to provide a printout of absorbers and emitters
C     for ATTIS.
C     (This is version 3 of DION.)
C
C     XCBL is the buffer for the Continuum Data Block.
C
C     See also DEANNA and SHAKE.
C     !DASH
      save
C     !DASH
      real*8 TIN, TOTIME, TOUT, W, X, XCBL, XLM
      integer IBL, ICL, ICSB, ICSO, IISWA, IISWE, IJBHS, IJOP, IJOPAC,
     $        IN, IS, ISLV, ITS, IW, IWS, IZL, JJZ, JN, KJKONT, KJOPAC,
     $        KJTAUK, KKACTB, KKACTO, KKALBD, KKB, KKBHS, KKBHSD,
     $        KKBHSN, KKBULT, KKCAPP, KKCB, KKCO, KKISWA, KKISWE,
     $        KKLAMD, KKLTIT, KKMULT, KKRESN, KKSIGS, KTRU, KTYPE, LU,
     $        LUT, MOX, MUX, N, NRES, jummy
      logical LINE, MOVING, PRNT, lummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK( 8),KKBHSN)
      equivalence (KKK( 9),KKBHSD)
      equivalence (KKK(39),KKBULT)
      equivalence (KKK(31),KKALBD)
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(15),KKB   )
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(21),KKCB  )
      equivalence (KKK(43),KKISWA)
      equivalence (KKK(44),KKISWE)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK(45),KKSIGS)
C     !EJECT
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
C
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
C     !DASH
      external  JOLLY, MANSUR, CONRAD, CONTDI, MUGWORT, TERMES, PADDLE,
     $          SHORE, SECOND, IGIVE, WGIVE, HI, BYE
      intrinsic abs
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension LUT(6)
C
      dimension IN(6)
      equivalence
     $(IN( 1),ICSO  ),(IN( 2),ICSB  ),(IN( 3),IZL   ),(IN( 4),ICL  ),
     $(IN( 5),IJOPAC),(IN( 6),IJBHS )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IISWA ),(JN( 2),IISWE ),(JN( 3),IJOP  )
C
      data KTYPE,KTRU /0, 0/
C     !EJECT
C
      call HI ('DION')
C     !BEG
C---- Get LU and LUTs, and find out whether printing should occur
      call SHORE     (LU, LUT, PRNT)
      if(.not.PRNT) then
        call SECOND    (TOUT)
        TOTIME = TOUT-TIN
      else
C       (Get, and allocate, W & IW allotments)
        call JOLLY   (IN, IS,  MOX, 'DION')
        call MANSUR  (JN, IWS, MUX, 'DION')
C
        XLM = abs(XCBL(KKLAMD))
C
C----   Get XCBL block indices depending on KTRU.
        call CONRAD  (KTRU, KJTAUK, jummy, KJOPAC, jummy, jummy,
     $                KJKONT)
C----   Convert switches back to integer form
        call CONTDI  (XCBL(KKISWA), 1, NOPAC, IW(IISWA), 1, NOPAC)
        call CONTDI  (XCBL(KKISWE), 1, NOPAC, IW(IISWE), 1, NOPAC)
        call CONTDI  (XCBL(KJKONT), 1, NOPAC, IW(IJOP ), 1, NOPAC)
C----   Edit COPAC and CBHS, to set contributors =0 whose KOPAC =0
        call MUGWORT (XCBL(KKCO), IW(IJOP), N, NOPAC, W(IJOPAC))
        call MUGWORT (XCBL(KKCB), IW(IJOP), N, NOPAC, W(IJBHS ))
C
C----   Get LINE switch (for proper header printout)
        call PADDLE  (KTYPE, LINE, lummy)
C
C----   Now do it
        call TERMES  (XLM, LU, LUT, N, NOPAC, XCBL(KKMULT),
     $                XCBL(KKLTIT), XCBL(KKCAPP), XCBL(KKSIGS),
     $                XCBL(KJOPAC), XCBL(KKBHSN), XCBL(KKBHSD),
     $                XCBL(KKBHS), X(JJZ), XCBL(KJTAUK), XCBL(KKB),
     $                W(IJOPAC), W(IJBHS), W(ICSO), W(ICSB), W(IZL),
     $                IW(IISWA), IW(IISWE), TIN, MOVING, W(ICL),
     $                XCBL(KKALBD), XCBL(KKBULT), TOTIME, KTRU, LINE)
C
C       (Give back W & IW allotments)
        call WGIVE   (W,  'DION')
        call IGIVE   (IW, 'DION')
      end if
C     !END
      call BYE ('DION')
C
      return
      end
