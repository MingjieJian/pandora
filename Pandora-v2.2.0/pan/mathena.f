      subroutine MATHENA
     $(QNAME,J,IX)
C
C     Rudolf Loeser, 1997 Jul 24
C---- Reads integer vectors.
C     !DASH
      save
C     !DASH
      integer IX, J, JJLCH, JJNLE, JJRKS, JJRLS, NSL
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  4),JJRKS)
      equivalence (JZOQ(  5),JJRLS)
      equivalence (JZOQ( 12),JJNLE)
      equivalence (JZOQ( 17),JJLCH)
C     !DASH
C     !EJECT
      external CILENTO, HI, BYE
C
      dimension IX(*)
C
      call HI ('MATHENA')
C     !BEG
      goto (
C
C       IRLCOMP  IRKCOMP  QNL      LCH
     $  101,     102,     103,     104
C
     $ ), J
C
  101 continue
        call CILENTO (IX(JJRLS), NSL, QNAME)
        goto 400
  102 continue
        call CILENTO (IX(JJRKS), NSL, QNAME)
        goto 400
  103 continue
        call CILENTO (IX(JJNLE), NSL, QNAME)
        goto 400
  104 continue
        call CILENTO (IX(JJLCH), NSL, QNAME)
        goto 400
  400 continue
C     !END
      call BYE ('MATHENA')
C
      return
      end
