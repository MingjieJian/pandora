      subroutine KARDA
     $(W,IW,BANDL,BANDU,L,EMU,NW,LTYPE,WAVES,EMINT,YSTAR,BRIGHT,LU,
     $ IJECT,LFB)
C
C     Rudolf Loeser, 2000 Nov 07
C---- Drives HEART, to compute and print composite opacity band
C     averages.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, BRIGHT, EMINT, EMU, W, WAVES, YSTAR
      integer IBRTB, IEMNB, IIET, IIST, IJECT, IN, INBT, IS, IW, IWAVB,
     $        IWS, IWVNB, IYSTB, JN, L, LFB, LTYPE, LU, MOX, MUX, NAB,
     $        NCP, NW
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(44),NCP)
C     !DASH
C     !EJECT
      external GORAN, AGNOR, HEART, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               BRIGHT(Nmkuse,L), EMU(L), LTYPE(Nmkuse), WAVES(Nmkuse),
      dimension BRIGHT(*),        EMU(*), LTYPE(*),      WAVES(*),
C
C               EMINT(Nmkuse,L), YSTAR(Nmkuse,L), BANDL(NAB), BANDU(NAB)
     $          EMINT(*),        YSTAR(*),        BANDL(*),   BANDU(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IWAVB ),(IN( 2),IEMNB ),(IN( 3),IYSTB ),(IN( 4),IBRTB),
     $(IN( 5),IWVNB )
C
      dimension JN(3)
      equivalence
     $(JN( 1),INBT  ),(JN( 2),IIST  ),(JN( 3),IIET  )
C
      call HI ('KARDA')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call GORAN (IN, IS,  MOX, 'KARDA')
      call AGNOR (JN, IWS, MUX, 'KARDA')
C
      call HEART (NAB, BANDL, BANDU, L, EMU, NW, LTYPE, WAVES, EMINT,
     $            YSTAR, BRIGHT, NCP, W(IWAVB), W(IEMNB), W(IYSTB),
     $            W(IBRTB), IW(INBT), IW(IIST), IW(IIET), LU, IJECT,
     $            LFB, W(IWVNB))
C
C     (Give back W & IW allotments)
      call WGIVE (W,  'KARDA')
      call IGIVE (IW, 'KARDA')
C     !END
      call BYE ('KARDA')
C
      return
      end
