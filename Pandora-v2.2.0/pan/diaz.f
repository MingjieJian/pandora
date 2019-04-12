      subroutine DIAZ
     $(YL,YCOL,YRATS,YCONT,NT,YWAVE,NWV,YHM,MHM,YLYM,KK,YCR,NCR,YKRJ,
     $ MLS,YLDT,NDT,BANDY,NAB)
C
C     Rudolf Loeser, 1970 May 13
C---- Initializes damping parameters, which, of course, mainly function
C     as method control parameters.
C     !DASH
      save
C     !DASH
      real*8 BANDY, ONE, Y, YCOL, YCONT, YCR, YHM, YKRJ, YL, YLDT, YLYM,
     $       YRATS, YWAVE
      integer KK, MHM, MLS, NAB, NCR, NDT, NT, NWV
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SET1, BLIND, HI, BYE
C
C               YCONT(NT), YWAVE(NWV), YHM(MHM), YLDT(NDT), BANDY(NAB),
      dimension YCONT(*),  YWAVE(*),   YHM(*),   YLDT(*),   BANDY(*),
C
C               YKRJ(MLS), YCR(NCR), YLYM(KK)
     $          YKRJ(*),   YCR(*),   YLYM(*)
C
      call HI ('DIAZ')
C     !BEG
      call BLIND (-ONE,Y)
C
      YL    = Y
      YCOL  = Y
      YRATS = Y
C
      call SET1  (YCONT,NT ,Y)
      call SET1  (YWAVE,NWV,Y)
      call SET1  (YHM  ,MHM,Y)
      call SET1  (YLYM ,KK ,Y)
      call SET1  (YCR  ,NCR,Y)
      call SET1  (YKRJ ,MLS,Y)
      call SET1  (YLDT ,NDT,Y)
      call SET1  (BANDY,NAB,Y)
C     !END
      call BYE ('DIAZ')
C
      return
      end
