      subroutine PING
     $(NO,KONFORM,LABEL,F,KNT)
C
C     Rudolf Loeser, 1974 Dec 30
C---- Prints a line for the detailed printout of contributors
C     to continuum absorption or emission.
C     !DASH
      save
C     !DASH
      real*8 F, ONE
      integer I, KNT, KON, KONFORM, NO
      character LABEL*27
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HALT, HI, BYE
      intrinsic abs
C
C               F(KNT)
      dimension F(*)
C     !EJECT
C
      call HI ('PING')
C     !BEG
      if((KONFORM.lt.1).or.(KONFORM.gt.2)) then
        write (MSSLIN(1),100) KONFORM
  100   format('KONFORM =',I12,', which is neither 1 nor 2.')
        call HALT ('PING', 1)
      end if
C
      KON = KONFORM
C
      if(KON.eq.2) then
        do 101 I = 1,KNT
          if(abs(F(I)).gt.ONE) then
            KON = 1
            goto 102
          end if
  101   continue
      end if
  102 continue
C
      if(KON.eq.1) then
        write (NO,103) LABEL,(F(I),I=1,KNT)
  103   format(' ',A27,1P10E10.3)
      else
        write (NO,104) LABEL,(F(I),I=1,KNT)
  104   format(' ',A27,10F10.7)
      end if
C     !END
      call BYE ('PING')
C
      return
      end
