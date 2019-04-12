      subroutine DALBO
     $(L,TEMP,DENS,DUMP,CI)
C
C     Rudolf Loeser, 1990 Oct 11
C---- Computes default value of CII for Hydrogen.
C
C     Johnson, L.C.  1972, ApJ, 174, 227-236.
C
C     !DASH
      save
C     !DASH
      real*8 B, BL, BRACE, BRAK1, BRAK2, BRAK3, CI, CON55, CON56, CON57,
     $       CQL, DENS, E1YL, E1ZL, E2YL, E2ZL, ELL, ELL2, EXYL, EXZL,
     $       FIVE, FOUR, FYL, FZL, G0, G1, G2, GL, ONE, SUM, TELL2L,
     $       TEMP, THREE, TWO, TWTHRD, V, YL, ZERO, ZL, dummy
      integer L, LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(17),TWTHRD)
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 6),FIVE  )
C     !DASH
C     !EJECT
      external  CUEL, ORNIS, RIGEL, KOLLA, EXPINT, JENGU, LINER, HI, BYE
      intrinsic abs
C
      call HI ('DALBO')
C     !BEG
      ELL  = L
      ELL2 = L**2
      call CUEL    (L, TEMP, DENS, CQL)
      call ORNIS   (L, ONE, CQL, ONE, TEMP, YL, ZL)
      call RIGEL   (57, CON57)
      V = sqrt(CON57*TEMP)
      call KOLLA   (L, G0, G1, G2)
      SUM = G0/THREE+CQL*(G1/FOUR+(CQL*G2)/FIVE)
      call RIGEL   (56, CON56)
      GL = CON56*ELL*(CQL**3)*SUM
      call EXPINT  (1, YL, E1YL, EXYL)
      call EXPINT  (2, YL, E2YL, dummy)
      FYL = EXYL/YL-TWO*E1YL+E2YL
      call EXPINT  (1, ZL, E1ZL, EXZL)
      call EXPINT  (2, ZL, E2ZL, dummy)
      FZL    = EXZL/ZL-TWO*E1ZL+E2ZL
      TELL2L = log(TWO*ELL2*CQL)
      call JENGU   (L, B)
      BL = TWTHRD*ELL2*CQL*(THREE+CQL*(TWO+CQL*B))
      BRAK1 = E1YL/YL-E1ZL/ZL
      BRAK2 = BL-GL*TELL2L
      BRAK3 = FYL-FZL
      BRACE = GL*BRAK1+BRAK2*BRAK3
      call RIGEL   (55, CON55)
C
      CI = abs((CON55*ELL2*V*(YL**2)*CQL*BRACE)/EXYL)
C
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) L,TEMP,DENS,CQL,YL,ZL,V,G0,G1,G2,SUM,GL,EXYL,
     $                   E1YL,E2YL,FYL,EXZL,E1ZL,E2ZL,FZL,TELL2L,B,BL,
     $                   BRAK1,BRAK2,BRAK3,BRACE,CI
  100   format(' ','CII according to Johnson.',5X,'L =',I5,5X,'TEMP =',
     $             1PE16.8,5X,'DENS =',E16.8/
     $         ' ','QL =',E16.8,5X,'YL =',E16.8,5X,'ZL =',E16.8,5X,
     $             'V =',E16.8/
     $         ' ','G0 =',E16.8,5X,'G1 =',E16.8,5X,'G2 =',E16.8,5X,
     $             'SUM =',E16.8,5X,'GL =',E16.8/
     $         ' ','EXYL =',E16.8,5X,'E1YL =',E16.8,5X,'E2YL =',E16.8,
     $             5X,'FYL =',E16.8/
     $         ' ','EXZL =',E16.8,5X,'E1ZL =',E16.8,5X,'E2ZL =',E16.8,
     $              5X,'FZL =',E16.8/
     $         ' ','TELL2L =',E16.8,5X,'B =',E16.8,5X,'BL =',E16.8/
     $         ' ','BRAK1 =',E16.8,5X,'BRAK2 =',E16.8,5X,'BRAK3 =',
     $             E16.8/
     $         ' ','BRACE =',E16.8,5X,'CII =',E16.8)
      end if
C     !END
      call BYE ('DALBO')
C
      return
      end
