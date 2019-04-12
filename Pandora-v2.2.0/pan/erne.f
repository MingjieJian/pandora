      subroutine ERNE
     $(I,START,G1,G2,NSUB,GOOD,LAB1,LAB2)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Dumps, for SKUA.
C     !DASH
      save
C     !DASH
      real*8 G1, G2, START
      integer I, LUEO, NSUB
      logical GOOD
      character LAB1*2, LAB2*5
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ERNE')
C     !BEG
      if(I.eq.1) then
        call LINER (2, LUEO)
        write (LUEO,100) LAB1,LAB2,LAB1
  100   format(' ','Integration details at each depth: '//
     $         ' ',11X,'GOOD',6X,'NSUB',17X,A2,'1',8X,A5,'(I)',
     $             13X,A2,'2')
      end if
C
      write (LUEO,101) I,GOOD,NSUB,G1,START,G2
  101 format(' ',I5,L10,I10,4X,1P3E16.8)
C     !END
      call BYE ('ERNE')
C
      return
      end
