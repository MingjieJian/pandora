      subroutine DIMPLE
     $(X,W,IW,INCDNT,JNUMTH,XCBL,KODE,KTRU)
C
C     Rudolf Loeser, 1976 Mar 08
C---- Calculates the Continuum Source Function and Jnu.
C
C     XCBL is the buffer for the Continuum Data Block.
C     (See also "AVALON".)
C
C     (This is version 2 of DIMPLE.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL, XLM
      integer IEXT, IIFLAG, IIMG, IN, IS, ITS, IW, IWS, JN, JNUMTH,
     $        KJCNXP, KJFD, KJOPAC, KJSCON, KJTAUK, KKACTB, KKBHS,
     $        KKBHSN, KKCNDT, KKDAMP, KKITS, KKJNU, KKLAMD, KKLTIT,
     $        KKSCAT, KKSIGM, KODE, KTRU, MOX, MUX, N, jummy
      logical INCDNT
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
      equivalence (KKK(13),KKJNU )
      equivalence (KKK( 7),KKSCAT)
      equivalence (KKK( 4),KKDAMP)
      equivalence (KKK(42),KKITS )
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(38),KKCNDT)
      equivalence (KKK(20),KKACTB)
      equivalence (KKK(30),KKSIGM)
      equivalence (KKK( 8),KKBHSN)
C     !DASH
C     !EJECT
      external  MUMU, CONRAD, LILITH, POUR, IMMAKE, IGIVE, WGIVE,
     $          HI, BYE
      intrinsic abs
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IEXT  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      call HI ('DIMPLE')
C     !BEG
C     (Get W and IW allotments)
      call MUMU   (IN, IS , MOX, 'DIMPLE')
      call IMMAKE (JN, IWS, MUX, 'DIMPLE')
C
      call CONRAD (KTRU, KJTAUK, KJSCON, KJOPAC, KJCNXP, KJFD, jummy)
      XLM = abs(XCBL(KKLAMD))
      call LILITH (X, W, IW, XLM, INCDNT, JNUMTH, N, XCBL(KKJNU),
     $             XCBL(KJOPAC), XCBL(KJSCON), XCBL(KKSCAT),
     $             XCBL(KKDAMP), XCBL(KJTAUK), ITS, XCBL(KJFD),
     $             XCBL(KKBHS), XCBL(KKCNDT), IW(IIMG), IIFLAG,
     $             XCBL(KJCNXP), XCBL(KKSIGM), XCBL(KKBHSN), W(IEXT))
      if(KTRU.ne.1) then
        XCBL(KKACTB) = IIFLAG
        XCBL(KKITS)  = ITS
      end if
      call POUR   (XLM, XCBL(KJTAUK), XCBL(KJOPAC), XCBL(KKBHS),
     $             XCBL(KJSCON), XCBL(KKJNU), KODE, KTRU,
     $             XCBL(KKLTIT))
C
C     (Give back W & IW allotments)
      call WGIVE  (W , 'DIMPLE')
      call IGIVE  (IW, 'DIMPLE')
C     !END
      call BYE ('DIMPLE')
C
      return
      end
