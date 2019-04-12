      subroutine JIB
     $(X,IX,P,LZA,NLPAIR)
C
C     Rudolf Loeser, 1995 Jul 27
C---- Moves things from (temporary) P to (permanent) X & IX; and
C     provides integer versions of some things from P.
C     (This is version 3 of JIB.)
C     !DASH
      save
C     !DASH
      real*8 P, X
      integer IIIBNE, IIIBNL, IIIBNU, IIILR, IIILZA, IIIMR, IIINLP, IX,
     $        JJBNL, JJBNU, JJIBE, JJLRJ, JJMRJ, LZA, NAB, NL, NLPAIR,
     $        NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(40),NSL)
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  7),JJIBE)
      equivalence (JZOQ(  1),JJMRJ)
      equivalence (JZOQ(  3),JJLRJ)
C
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
C     .
      equivalence
     $(ISHF( 1),IIIMR ),(ISHF( 2),IIILR ),(ISHF( 3),IIILZA),
     $(ISHF( 4),IIIBNL),(ISHF( 5),IIIBNU),(ISHF( 6),IIIBNE),
     $(ISHF( 7),IIINLP)
C     .
C     !DASH
C     !EJECT
      external LEYTE, MOVE1, CONTDI, HI, BYE
C
      dimension X(*), IX(*)
C
C               P(LSHF), LZA(50), NLPAIR(2,NL)
      dimension P(*),    LZA(*),  NLPAIR(*)
C
      call HI ('JIB')
C     !BEG
      call LEYTE  (P,LSHF,LADR)
C
      call MOVE1  (P(IIIBNL),NAB,X(JJBNL))
      call MOVE1  (P(IIIBNU),NAB,X(JJBNU))
C
      call CONTDI (P(IIIBNE),1,NAB    ,IX(JJIBE),1,NAB    )
      call CONTDI (P(IIIMR ),1,(NSL+1),IX(JJMRJ),1,(NSL+1))
      call CONTDI (P(IIILR ),1,NL     ,IX(JJLRJ),1,NL     )
C
      call CONTDI (P(IIILZA),1,50     ,LZA      ,1,50     )
      call CONTDI (P(IIINLP),1,(2*NL) ,NLPAIR   ,1,(2*NL) )
C     !END
      call BYE ('JIB')
C
      return
      end
