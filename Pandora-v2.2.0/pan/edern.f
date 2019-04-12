      subroutine EDERN
     $(X,BRNT,PRNT)
C
C     Rudolf Loeser, 2004 May 25
C---- Prints, for INGER.
C     (This is version 2 of EDERN.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer JJSET, N, NL, NO, jummy
      logical BRNT, PRNT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(253),JJSET)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external PRIAM, GARYM, LINER, SCRIBE, HI, BYE
C
      dimension X(*)
C
      call HI ('EDERN')
C     !BEG
      call PRIAM   (NO, 'START VALUES', 12)
C
      if(BRNT.or.PRNT) then
        write (NO,100)
  100   format(' ','Computed default values.')
        call GARYM (X, 'Starting')
      end if
C
      call LINER   (2, NO)
      write (NO,101)
  101 format(' ','Stimulated Emission Term: SET = [ b(u) / b(l) ] * ',
     $           'beta(u,l), where beta = exp[ (-h*nu) / (k*T) ];'/
     $       ' ','(see Section  ALPHA+BETA,  above ',
     $           '[note option STIMPRNT]).')
      call SCRIBE  (X(JJSET), 'UL', jummy, 1, N, N, NL, NO, jummy)
C     !END
      call BYE ('EDERN')
C
      return
      end
