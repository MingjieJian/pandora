      subroutine NUFU
     $(I,B,BS,EBS,BA,EBA,BF,EBF,S,ES)
C
C     Rudolf Loeser, 1989 Dec 21
C---- Inserts "comparators", for ORINOCO.
C     !DASH
      save
C     !DASH
      real*8 B, BA, BF, BS, S, ZERO
      integer I
      character BLANK*1, EBA*(*), EBF*(*), EBS*(*), ES*(*), MBA*1,
     $          MBF*1, MBS*1, MS*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external DARIEN, HI, BYE
C
      call HI ('NUFU')
C     !BEG
      if(I.eq.1) then
        MBS = BLANK
        MBA = BLANK
        MBF = BLANK
        MS  = BLANK
      end if
C     !EJECT
      call DARIEN       (B ,BS,EBS,MBS)
C
      if(BS.ne.ZERO) then
        call DARIEN     (BS,BA,EBA,MBA)
      else
        call DARIEN     (B ,BA,EBA,MBA)
      end if
C
      if(BA.ne.ZERO) then
        call DARIEN     (BA,BF,EBF,MBF)
      else
        if(BS.ne.ZERO) then
          call DARIEN   (BS,BF,EBF,MBF)
        else
          call DARIEN   (B ,BF,EBF,MBF)
        end if
      end if
C
      if(BF.ne.ZERO) then
        call DARIEN     (BF,S ,ES ,MS )
      else
        if(BA.ne.ZERO) then
          call DARIEN   (BA,S ,ES ,MS )
        else
          if(BS.ne.ZERO) then
            call DARIEN (BS,S ,ES ,MS )
          else
            call DARIEN (B ,S ,ES ,MS )
          end if
        end if
      end if
C
C     !END
      call BYE ('NUFU')
C
      return
      end
