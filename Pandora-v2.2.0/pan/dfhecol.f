      subroutine DFHECOL
     $(CNOS,TE,T4,A4,PART,ENE,AMASS,XK,DUMP)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes collision cross-sections for DEEHEE (q.v.).
C     Adapted from HECOL, 7/3/90, written by
C
C     J u a n   F o n t e n l a .
C
C     !DASH
      save
C     !DASH
      real*8 A4, AMA, AMASS, AMHEH, AMHHE, AMRED, C1, C2, C3, C4, C5,
     $       C6, C7, C8, CNOS, DEN, ENE, F01, F012, F014, F02, F024,
     $       F04, FOUR, G1, G2, ONE, PART, R1, R2, R3, R4, R5, RATEF,
     $       RATEG, T4, T4P2, T4P24, T4P4, T4P5, TE, TWO, XK, XKTE, ZET
      integer I, J, LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DFCNOS, CONMUL, LINER, HI, BYE
C
      dimension CNOS(5,5), PART(5), ZET(5), AMA(5), DEN(5)
C
      data ZET /0.D0, 1.D0, 0.D0, 1.D0, 2.D0/
      data AMA /1.D0, 1.D0, 0.D0, 0.D0, 0.D0/
C
      data C1,C2,C3 /1.D-9, 2.5D0, 4.2D-9/
      data C4,C5,C6 /1.13D-9, 1.1D0, 4.D-10/
      data C7,C8    /4.D-9, 2.3D-7/
C
      data R1,R2,R3 /2.5613D-1, 2.8662D0, 1.25013D0/
      data R4,R5    /1.40843D-1, 1.31387D0/
C
      data G1,G2 /9.4D-8, 4.3D-1/
C
      data F01,F012,F014 /1.D-1, 1.2D-1, 1.4D-1/
      data F02,F024,F04  /2.D-1, 2.4D-1, 4.D-1/
      data ONE,TWO,FOUR  /1.D0, 2.D0, 4.D0/
C     !EJECT
C
      call HI ('DFHECOL')
C     !BEG
      AMA(3) = AMASS
      AMA(4) = AMASS
      AMA(5) = AMASS
C---- Compute some collision rates
      call DFCNOS (CNOS, TE, PART, ENE, XK, ZET, AMA, DUMP)
C
C---- Compute remaining collision rates
      T4P5  = sqrt(T4)
      T4P2  = T4**F02
      T4P24 = T4**F024
      T4P4  = T4**F04
C
      AMRED = (AMA(1)*AMA(3))/(AMA(1)+AMA(3))
      AMHHE = TWO*AMRED/AMA(1)
      AMHEH = TWO*AMRED/AMA(3)
C
C---- Janev, Langer, Evans & Post
      CNOS(5,3) = C1*T4P5/(C2+T4P2)
      CNOS(3,5) = CNOS(5,3)
      CNOS(4,3) = C1*(ONE-F01*A4)*T4P4
      CNOS(3,4) = CNOS(4,3)
      CNOS(2,1) = C3*(ONE-F014*A4)*T4P4
      CNOS(1,2) = CNOS(2,1)
C---- Geiss & Buergi (revised)
      CNOS(1,1) = C4*(ONE-(C5-T4P24)**2)
C---- Geiss & Buergi
      CNOS(3,3) = C6
C---- Kimura
      RATEF = C7*((A4*(-A4*R1+R2)-ONE)/(ONE-A4*R3+T4*R4-(R5/T4P5)))
      CNOS(3,2) = AMHEH*RATEF
      CNOS(2,3) = AMHHE*RATEF
C
      RATEG = G1*(T4P5/(ONE+G2*T4P5))
      CNOS(1,4) = AMHHE*RATEG
      CNOS(4,1) = AMHEH*RATEG
C---- Guessed
      CNOS(5,1) = CNOS(2,1)*FOUR*AMHEH
      CNOS(1,5) = CNOS(5,1)*(AMHHE/AMHEH)
      CNOS(3,1) = CNOS(3,3)*AMHEH
      CNOS(1,3) = CNOS(3,3)*AMHHE
C
      XKTE = XK*TE
      do 100 J = 1,5
        DEN(J) = PART(J)/XKTE
        call CONMUL (DEN(J), CNOS(1,J), 5)
  100 continue
C     !EJECT
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,101) ZET,AMA,AMRED,AMHHE,AMHEH,RATEF,RATEG
  101   format(' ','ZET=',1P5E11.3/
     $         ' ','AMA=',5E11.3/
     $         ' ','AMRED=',E11.3,' AMHHE=',E11.3,' AMHEH=',E11.3/
     $         ' ','RATEF=',E11.3,' RATEG=',E11.3)
        call LINER (1, LUEO)
        write (LUEO,102)
  102   format(' ','Particle densities')
        write (LUEO,104) DEN
        call LINER (1, LUEO)
        write (LUEO,103)
  103   format(' ','Collision rates')
        write (LUEO,104) ((CNOS(I,J),J=1,5),I=1,5)
  104   format(' ',1P5E11.3)
      end if
C     !END
      call BYE ('DFHECOL')
C
      return
      end
