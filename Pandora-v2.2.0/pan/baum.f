      subroutine BAUM
     $(NFL,WNRML,ONRML,PHZ,CHZ,FNRML,DLL,DLR,AB)
C
C     Rudolf Loeser, 2005 Nov 04
C---- Computes and prints a new set of H Ly normalization factors.
C     !DASH
      save
C     !DASH
      real*8 CHZ, DLL, DLR, FNRML, ONE, ONRML, PHZ, RAT, TWO, WNRML
      integer I, NFL, NO
      character AB*1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external DIVIDE, ABJECT, LINER, SHIM, RASPE, HI, BYE
C
C               WNRML(NFL), ONRML(NFL), FNRML(NFL), PHZ(NFL), CHZ(NFL)
      dimension WNRML(*),   ONRML(*),   FNRML(*),   PHZ(*),   CHZ(*)
C
      call HI ('BAUM')
C     !BEG
C---- Print header
      call ABJECT     (NO)
      write (NO,100) AB
  100 format(' ','Recalculation of FNRML'A//
     $       ' ',18X,'WVL',7X,'old FNRML',6X,'background',
     $           9X,'profile',11X,'ratio',3X,'raw new FNRML')
      call LINER      (1, NO)
C
C---- Compute, and print
      do 102 I = 1,NFL
        if((WNRML(I).lt.DLL).or.(WNRML(I).gt.DLR)) then
          RAT = ONE
        else
          call DIVIDE (PHZ(I), CHZ(I), RAT)
        end if
        FNRML(I) = ((ONE+RAT)/TWO)*ONRML(I)
C
        write (NO,101) I,WNRML(I),ONRML(I),CHZ(I),PHZ(I),RAT,FNRML(I)
  101   format(' ',I5,F16.2,1PE16.4,2E16.6,0PF16.6,1PE16.4)
        call SHIM     (I, 5, NO)
  102 continue
C
C---- Weight the ends (to force approach to 1), and print
      call RASPE      (NFL, FNRML, NO)
C     !END
      call BYE ('BAUM')
C
      return
      end
