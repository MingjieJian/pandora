      subroutine PAPRIKA
     $(QNAME,KIND,X,LZA,ZAUX,W)
C
C     Rudolf Loeser, 1973 Jun 01
C---- Reads floating point arrays with two level indices.
C     !DASH
      save
C     !DASH
      real*8 W, X, ZAUX, dummy
      integer IL, IU, JJACE, JJAIJ, JJAW, JJCEI, JJCIA, JJFCE, JJMCE,
     $        JJOLL, JJOML, JJPCE, JJQHI, JJRHO, JJYBR, JJYCO, JJZ,
     $        KIND, LZA, N, NL, NTE, jummy
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(20),NTE)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 30),JJCEI)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ(  3),JJOML)
      equivalence (IZOQ(182),JJOLL)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(207),JJCIA)
      equivalence (IZOQ(106),JJMCE)
      equivalence (IZOQ(107),JJACE)
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(242),JJAW )
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(249),JJPCE)
C     !DASH
      external MINT, ANATH, JUNIPER, PALMYRA, HI, BYE
C
      dimension X(*), W(*)
C
C               ZAUX(LZM,NZM), LZA(50)
      dimension ZAUX(*),       LZA(*)
C
      call HI ('PAPRIKA')
C     !BEG
      call MINT      (QNAME, IU)
      call MINT      (QNAME, IL)
C     !EJECT
      goto (
C
C       A        CE       RHO      YCONT    OML      JBAR     OLL
     $  101,     102,     103,     104,     105,     106,     107,
C
C       CIJADD   MCE      ACE      QHI      AW       FCE      PCE
     $  108,     109,     110,     111,     112,     113,     114
C
     $  ), KIND
C
  101 continue
        call ANATH   (NL, QNAME, X(JJAIJ), IU, IL)
        goto 400
  102 continue
        call JUNIPER (1, 'UL', 1, IU, IL, X(JJCEI), jummy, NTE, QNAME,
     $                jummy, dummy, dummy, dummy)
        goto 400
  103 continue
        call JUNIPER (1, 'NT', 2, IU, IL, X(JJRHO), jummy, N  , QNAME,
     $                LZA, ZAUX, X(JJZ), W)
        goto 400
  104 continue
        call JUNIPER (1, 'NT', 1, IU, IL, X(JJYCO), jummy, 1  , QNAME,
     $                jummy, dummy, dummy, dummy)
        goto 400
  105 continue
        call JUNIPER (1, 'NT', 1, IU, IL, X(JJOML), jummy, 1  , QNAME,
     $                jummy, dummy, dummy, dummy)
        call PALMYRA (IU, IL, X(JJOML))
        goto 400
  106 continue
        call JUNIPER (1, 'NT', 2, IU, IL, X(JJYBR), jummy, N  , QNAME,
     $                LZA, ZAUX, X(JJZ), W)
        goto 400
  107 continue
        call JUNIPER (1, 'NT', 1, IU, IL, X(JJOLL), jummy, 1  , QNAME,
     $                jummy, dummy, dummy, dummy)
        goto 400
  108 continue
        call JUNIPER (1, 'IJ', 2, IU, IL, X(JJCIA), jummy, N  , QNAME,
     $                LZA, ZAUX, X(JJZ), W)
        goto 400
  109 continue
        call JUNIPER (1, 'UL', 1, IU, IL, X(JJMCE), jummy, 1  , QNAME,
     $                jummy, dummy, dummy, dummy)
        goto 400
  110 continue
        call JUNIPER (1, 'UL', 1, IU, IL, X(JJACE), jummy, 1  , QNAME,
     $                jummy, dummy, dummy, dummy)
        goto 400
C     !EJECT
  111 continue
        call JUNIPER (1, 'NT', 2, IU, IL, X(JJQHI), jummy, N  , QNAME,
     $                LZA, ZAUX, X(JJZ), W)
        goto 400
  112 continue
        call JUNIPER (1, 'NT', 2, IU, IL, X(JJAW) , jummy, N  , QNAME,
     $                LZA, ZAUX, X(JJZ), W)
        goto 400
  113 continue
        call JUNIPER (1, 'NT', 2, IU, IL, X(JJFCE), jummy, N  , QNAME,
     $                LZA, ZAUX, X(JJZ), W)
        goto 400
  114 continue
        call JUNIPER (1, 'NT', 1, IU, IL, X(JJPCE), jummy, 1  , QNAME,
     $                jummy, dummy, dummy, dummy)
        goto 400
  400 continue
C     !END
      call BYE ('PAPRIKA')
C
      return
      end
