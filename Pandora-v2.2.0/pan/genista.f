      subroutine GENISTA
     $(MODE,XNAME,KODES,FACTS,KOUNT,CALLER)
C
C     Rudolf Loeser, 1988 Dec 12
C---- Packing (MODE=1) and unpacking (MODE=2) routine.
C     Remember: with MODE=1, the labels in common block SALKA
C     must be initialized at the appropriate calling level!
C     !DASH
      save
C     !DASH
      real*8 FACTS, FAME, SAME, XLIM, XNAME, ZERO
      integer I, KODES, KOUNT, LUEO, MODE
      character CALLER*(*)
C     !COM
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  AJUGA, HALT, HI, BYE
      intrinsic mod
C
C               KODES(KOUNT), FACTS(KOUNT)
      dimension KODES(*),     FACTS(*)
C
      data XLIM /9.999D0/
C
      call HI ('GENISTA')
C     !BEG
      if(MODE.eq.1) then
C
        SAME = ZERO
        do 100 I = 1,KOUNT,+1
          FAME = KODES(I)
          if((FAME.lt.ZERO).or.(FAME.gt.(XLIM*FACTS(I)))) then
            call AJUGA (LUEO, KODES, FACTS, KOUNT, CALLER)
          end if
          SAME = (FACTS(I)*SAME)+FAME
  100   continue
        XNAME = SAME
C
      else if(MODE.eq.2) then
C
        SAME = XNAME
        do 101 I = KOUNT,1,-1
          FAME = mod(SAME,FACTS(I))
          SAME = (SAME-FAME)/FACTS(I)
          KODES(I) = FAME
  101   continue
C
      else
        write (MSSLIN(1),102) CALLER,MODE
  102   format('CALLER = ',A,'; MODE =',I12,', which is not 1 nor 2.')
        call HALT      ('GENISTA', 1)
      end if
C     !END
      call BYE ('GENISTA')
C
      return
      end
