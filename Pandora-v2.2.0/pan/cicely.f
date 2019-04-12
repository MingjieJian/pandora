      subroutine CICELY
     $(QNAME,KIND,X,LZA,ZAUX,W)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Reads floating arrays with one index.
C     !DASH
      save
C     !DASH
      real*8 W, X, ZAUX
      integer I, JJBDI, JJCII, JJCKA, JJR1W, JJRK, JJRL, JJTR, JJXND,
     $        JJZ, KIND, LZA, N, NL, NSL, NTE
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(20),NTE)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(108),JJR1W)
      equivalence (IZOQ(208),JJCKA)
C     !DASH
      external MINT, THYME, CREAM, HI, BYE
C
      dimension X(*), W(*)
C
C               ZAUX(LZM,NZM), LZA(50)
      dimension ZAUX(*),       LZA(*)
C     !EJECT
C
      call HI ('CICELY')
C     !BEG
      call MINT    (QNAME,I)
C
      goto (
C
C       RKI      RLI      TRN               CKADD    CII      XND
     $  101,     102,     103,     104,     105,     106,     107,
C
C       BDI      RKW
     $  108,     109
C
     $  ), KIND
C
  101 continue
        call THYME (I,2,NSL,X(JJRK) ,N  ,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  102 continue
        call THYME (I,2,NSL,X(JJRL) ,N  ,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  103 continue
        call THYME (I,2,NSL,X(JJTR) ,N  ,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  104 continue
        goto 400
  105 continue
        call THYME (I,2,NSL,X(JJCKA),N  ,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  106 continue
        call THYME (I,1,NSL,X(JJCII),NTE,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  107 continue
        call THYME (I,2,NL ,X(JJXND),N  ,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  108 continue
        call THYME (I,2,NL ,X(JJBDI),N  ,QNAME,LZA,ZAUX,X(JJZ),W)
        goto 400
  109 continue
        call CREAM (X(JJR1W),QNAME,I,0,LZA,ZAUX,X(JJZ),W)
        goto 400
  400 continue
C     !END
      call BYE ('CICELY')
C
      return
      end
