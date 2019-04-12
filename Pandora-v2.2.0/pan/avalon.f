      subroutine AVALON
     $(X,IX,W,IW,ILFLX,MPROM,MOVING,SPHERE,INCDNT,JNUMTH,XCBL,
     $ XLB1,XLB2)
C
C     Rudolf Loeser, 1983 Jan 24
C---- Calculates the Continuum Source Function and Jnu.
C
C     XLB1 is the buffer for the Line Intensity Data Block, Part 1;
C     XLB2 is the buffer for the Line Intensity Data Block, Part 2;
C     XCBL is the buffer for the Continuum Data Block.
C
C     (Special version of "DIMPLE", for P.R.D. Jnu calculation.)
C                                       ==========
C
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL, XLB1, XLB2, XLM
      integer IEXT, IIFLAG, IIMG, ILFLX, IN, IS, ITS, IW, IWS, IX, JN,
     $        JNUMTH, KKACTB, KKBHS, KKBHSN, KKCNDT, KKCNXP, KKDAMP,
     $        KKFD, KKITS, KKJNU, KKLAMD, KKLTIT, KKOPAC, KKSCAT,
     $        KKSCON, KKSIGM, KKTAUK, KODE, KTRU, LG, MOX, MPROM, MRR,
     $        MUX, N
      logical INCDNT, MOVING, SPHERE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(34),LG )
      equivalence (JZQ(15),MRR)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK(13),KKJNU )
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(14),KKSCON)
      equivalence (KKK( 7),KKSCAT)
      equivalence (KKK( 4),KKDAMP)
      equivalence (KKK(12),KKTAUK)
      equivalence (KKK(42),KKITS )
      equivalence (KKK(17),KKFD  )
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(38),KKCNDT)
      equivalence (KKK(20),KKACTB)
      equivalence (KKK(11),KKCNXP)
      equivalence (KKK(30),KKSIGM)
      equivalence (KKK( 8),KKBHSN)
      equivalence (KKK( 1),KKLTIT)
C     !DASH
C     !EJECT
      external  KAKADU, VENUS, POUR, IMMAKE, WGIVE, IGIVE, HI, BYE
      intrinsic abs
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), XLB1(Li1len), XLB2(Li2len)
      dimension XCBL(*),      XLB1(*),      XLB2(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IEXT  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      data KODE,KTRU /1, 0/
C
      call HI ('AVALON')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call KAKADU (IN, IS,  MOX, 'AVALON')
      call IMMAKE (JN, IWS, MUX, 'AVALON')
C
C---- Get wavelength value
      XLM = abs(XCBL(KKLAMD))
C---- Compute
      call VENUS  (X, W, IW, XCBL, XLM, N, LG, MRR, XCBL(KKJNU),
     $             XCBL(KKOPAC), XCBL(KKSCON), XCBL(KKSCAT),
     $             XCBL(KKDAMP), XCBL(KKTAUK), ITS, XCBL(KKFD),
     $             XCBL(KKBHS), XCBL(KKCNDT), IW(IIMG), IIFLAG,
     $             XCBL(KKCNXP), XCBL(KKSIGM), XCBL(KKBHSN), W(IEXT),
     $             XCBL(KKLTIT), MOVING, SPHERE, INCDNT, JNUMTH,
     $             ILFLX, MPROM, XLB1, XLB2)
C---- (Store flags in data block in flpt format)
      XCBL(KKACTB) = IIFLAG
      XCBL(KKITS)  = ITS
C---- Debug checksums
      call POUR   (XLM, XCBL(KKTAUK), XCBL(KKOPAC), XCBL(KKBHS),
     $             XCBL(KKSCON), XCBL(KKJNU), KODE, KTRU,
     $             XCBL(KKLTIT))
C
C     (Give back W & IW allotments)
      call WGIVE  (W,  'AVALON')
      call IGIVE  (IW, 'AVALON')
C     !END
      call BYE ('AVALON')
C
      return
      end
