      subroutine AJUGA
     $(LU,KODES,FACTS,KOUNT,QNAME)
C
C     Rudolf Loeser, 1988 Dec 09
C---- Prints an error message, and stops.
C     !DASH
      save
C     !DASH
      real*8 FACTS, XNINE
      integer I, KODES, KOUNT, LU
      character QNAME*(*)
C     !COM
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(10),XNINE )
C     !DASH
      external LINER, ABORT, HI, BYE
C
C               KODES(KOUNT), FACTS(KOUNT)
      dimension KODES(*),     FACTS(*)
C
C
      call HI ('AJUGA')
C     !BEG
      if(LU.gt.0) then
        call LINER   (2, LU)
        write (LU,100) QNAME,KOUNT
  100   format(' ','Error in data-packing for ',A,':'/
     $         ' ','KOUNT=',I3)
        if(KOUNT.gt.0) then
          call LINER (1, LU)
          write (LU,101) (I,KODES(I),(XNINE*FACTS(I)),GENLAB(I),
     $                    I=1,KOUNT)
  101     format(' ','KODES(',I2,') =',I12,
     $               ', it must be .ge. 0 and .lt. ',F8.0,1X,A60)
        end if
      end if
      call ABORT
C     !END
      call BYE ('AJUGA')
C
      return
      end
