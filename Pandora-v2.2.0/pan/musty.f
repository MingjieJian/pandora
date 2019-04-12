      subroutine MUSTY
     $(IX,MRA)
C
C     Rudolf Loeser, 2004 Mar 17
C---- Sets MRA to the largest values of LIJ (aka KRATE).
C     !DASH
      save
C     !DASH
      integer IX, JJLIJ, MRA, MUL, NL, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  8),JJLIJ)
C     !DASH
      external MINMAXI, HI, BYE
C
      dimension IX(*)
C
      call HI ('MUSTY')
C     !BEG
      MUL = (NL*(NL-1))/2
      call MINMAXI (IX(JJLIJ), 1, MUL, jummy, MRA)
C     !END
      call BYE ('MUSTY')
C
      return
      end
