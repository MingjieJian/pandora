      subroutine DAGGER
     $(AF,BF,CF,DF,EF,AH,BH,CH,DH,EH,A,B,C,D,E,DIV)
C
C     Rudolf Loeser, 1975 Nov 25
C---- Analyzes Damping Parameter components.
C     (This is version 2 of DAGGER.)
C     !DASH
      save
C     !DASH
      real*8 A, AF, B, BF, C, CF, D, DF, DIV, E, EF, Z
      character AH*1, BH*1, BLANK*1, CH*1, DH*1, EH*1, STAR*1
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
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external  MARKD, HI, BYE
      intrinsic max
C     !EJECT
C
      call HI ('DAGGER')
C     !BEG
      if(DIV.ne.ZERO) then
        AF = A/DIV
        BF = B/DIV
        CF = C/DIV
        DF = D/DIV
        EF = E/DIV
      else
        AF = ZERO
        BF = ZERO
        CF = ZERO
        DF = ZERO
        EF = ZERO
      end if
C
      Z = max(AF,BF,CF,DF,EF)
C
      if(Z.ne.ZERO) then
        call MARKD (AF,Z,AH,STAR,BLANK)
        call MARKD (BF,Z,BH,STAR,BLANK)
        call MARKD (CF,Z,CH,STAR,BLANK)
        call MARKD (DF,Z,DH,STAR,BLANK)
        call MARKD (EF,Z,EH,STAR,BLANK)
      else
        AH = BLANK
        BH = BLANK
        CH = BLANK
        DH = BLANK
        EH = BLANK
      end if
C     !END
      call BYE ('DAGGER')
C
      return
      end
