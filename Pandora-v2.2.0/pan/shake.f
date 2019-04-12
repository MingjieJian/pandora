      subroutine SHAKE
     $(X,W,IW,XCBL,MOVING,INCDNT,JNUMTH,TIN,TOTIME)
C
C     Rudolf Loeser, 2005 Mar 28
C---- Drives CLAM, to provide printout of JNU and CSF for DEMETER.
C     (This is version 3 of SHAKE.)
C
C     XCBL is the buffer for the Continuum Data Block.
C
C     See also DEANNA and DION.
C     !DASH
      save
C     !DASH
      real*8 TIN, TOTIME, TOUT, W, X, XCBL, XLM
      integer IBHSL, IBL, IFLAGB, IISWA, IISWE, IJOPAC, IN, IP, IS, ISL,
     $        ITS, IW, IYNUL, IZL, JJFRS, JJTE, JJZ, JNUMTH, KJCNXP,
     $        KJFD, KJOPAC, KJSCON, KJTAUK, KKACTB, KKB, KKBHS, KKCB,
     $        KKCO, KKDAMP, KKITS, KKJNU, KKLAMD, KKLTIT, KKMULT,
     $        KKSCAT, KTRU, LU, LUT, MOX, N, jummy
      logical INCDNT, LINE, MOVING, PRNT, SPHERE, lummy
C     !COM
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
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(13),KKJNU )
      equivalence (KKK(15),KKB   )
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(20),KKACTB)
      equivalence (KKK(21),KKCB  )
      equivalence (KKK(42),KKITS )
C     !DASH
C     !EJECT
      external  HUTIA, CONRAD, CLAM, SHORE, SECOND, WGIVE, HI, BYE
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
     $(IN( 1),IP    ),(IN( 2),IZL   ),(IN( 3),ISL   ),(IN( 4),IBL   ),
     $(IN( 5),IBHSL ),(IN( 6),IYNUL )
C
      data KTRU /0/
C
      call HI ('SHAKE')
C     !BEG
C---- Get LU and LUTs, and find out whether printing should occur
      call SHORE    (LU, LUT, PRNT)
      if(.not.PRNT) then
        call SECOND (TOUT)
        TOTIME = TOUT-TIN
      else
C       (Get, and allocate, W & IW allotments)
        call HUTIA  (IN, IS , MOX, 'SHAKE')
C
        XLM = abs(XCBL(KKLAMD))
C
C----   Get XCBL block indices depending on KTRU.
        call CONRAD (KTRU, KJTAUK, KJSCON, KJOPAC, KJCNXP, KJFD,
     $                jummy)
C----   Convert switches back to integer form
        IFLAGB = XCBL(KKACTB)
        ITS    = XCBL(KKITS)
C
        call CLAM   (XLM, LU, LUT, N, XCBL(KKMULT), XCBL(KKLTIT),
     $               XCBL(KKDAMP), XCBL(KJOPAC), XCBL(KKSCAT),
     $               XCBL(KKBHS), X(JJZ), XCBL(KJTAUK), XCBL(KKJNU),
     $               XCBL(KJSCON), XCBL(KKB), X(JJTE), XCBL(KJFD),
     $               IFLAGB, W(IP), W(IZL), W(ISL), W(IBL), W(IBHSL),
     $               W(IYNUL), ITS, XCBL(KJCNXP), TIN, MOVING, INCDNT,
     $               JNUMTH, X(JJFRS), TOTIME, KTRU)
C
C       (Give back W allotment)
        call WGIVE  (W, 'SHAKE')
      end if
C     !END
      call BYE ('SHAKE')
C
      return
      end
