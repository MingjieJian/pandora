      subroutine DFOCOF
     $(C,AMASS,T4,A4,ALFX,ALFY,ALFZ,CX10,CY10,CZ10,DUMP)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes intermediates for DEEHEE (q.v.).
C     Adapted from OCOF, written by
C
C     J u a n   F o n t e n l a .
C
C     !DASH
      save
C     !DASH
      real*8 A4, AKMI, AKPL, ALF0, ALF1, ALFX, ALFY, ALFZ, AMASS, C,
     $       C0P, C0PP, C1, C10, C11, C12, C1P, C1PP, C2, C3, C4, C5,
     $       C6, C7, C8, C9, CX10, CXP, CY10, CYP, CZ10, CZP, DETER,
     $       DIFK, F005, F04, F08, F13, F16, F20, F27, F30, FAC, FIVE,
     $       ONE, OPM, RM, RM2, T4, TEN, X10, X13, Z00PP, Z01, Z01P,
     $       Z01PP, Z11PP
      integer LUEO
      logical DUMP
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
C               C(5,5)
      dimension C(5,*)
C
      data Z01   /-1.2D-1/
      data Z01P  / 1.16D0/
      data Z00PP / 2.15D0/
      data Z11PP / 2.D0/
C
      data C1,C2,C3    /8.6D-1, 6.4D-1, 1.11D0/
      data C4,C5,C6    /9.7D-1, 5.D0, -5.D-1/
      data C7,C8,C9    /-1.5D1, 1.5D0, 3.6D-1/
      data C10,C11,C12 /-2.7D-1, 2.5D-1, 7.5D-1/
C
      data F005,F04,F08 /5.D-2, 4.D-1, 8.D-1/
      data F13,F16,F20  /1.3D1, 1.6D1, 2.D1/
      data F27,F30      /2.7D1, 3.D1/
      data ONE,FIVE,TEN /1.D0, 5.D0, 1.D1/
C     !EJECT
C
      call HI ('DFOCOF')
C     !BEG
      Z01PP = C9+C10*A4
      AKPL  = C3-F005*A4
      AKMI  = C4+F005*A4
C
      DETER = C1*(C(1,2)*C(2,2)+C(2,1)*C(1,1))+C2*(C(1,1)*C(2,2))
C
      FAC  = (C(2,2)-C(1,1))/(FIVE*DETER)
      ALF0 =  C(1,2)*FAC
      ALF1 = -C(2,1)*FAC
C
      X13 = F13+AMASS*(F16+AMASS*F30)
      X10 = TEN+AMASS*(F20+AMASS*F30)
      C0P = AKPL*C(2,1)+AKMI*C(1,2)+F08*C(2,2)
      C1P = AKPL*C(1,2)+AKMI*C(2,1)+F08*C(1,1)
      CXP = X13*C(3,1)+X10*C(3,2)
      CYP = X10*C(4,1)+X13*C(4,2)
      CZP = X10*C(5,1)+X13*C(5,2)
C
      OPM = ONE+AMASS
      RM  = AMASS/OPM
      RM2 = AMASS*RM
      FAC = RM2/DETER
C
      ALFX = C5*OPM*C(3,1)/CXP +C6*FAC*C0P*C(3,1)*
     $                   (ONE-F27*C(3,1)/CXP-F20*C1P*C(3,2)/(C0P*CXP))
C
      ALFY = C7*OPM*C(4,2)/CYP +C8*FAC*C1P*C(4,2)*
     $                   (ONE-F27*C(4,2)/CYP-F20*C0P*C(4,1)/(C1P*CYP))
C
      ALFZ = C7*OPM*C(5,2)/CZP +C8*FAC*C1P*C(5,2)*
     $                   (ONE-F27*C(5,2)/CZP-F20*C0P*C(5,1)/(C1P*CZP))
C
      DIFK = AKPL-AKMI
      FAC  = (Z01*RM)/DETER
      C0PP = C(1,2)*(C(2,1)*DIFK+F04*Z11PP*C(2,2))
      C1PP = C(2,1)*(C(1,2)*DIFK+F04*Z00PP*C(1,1))
C
      CX10 = C(3,1)*(C11*FAC*C0PP)*
     $                 (ONE-F27*C(3,1)/CXP+F20*C1PP*C(3,2)/(C0PP*CXP))
C
      CY10 = C(4,2)*(C12*FAC*C1PP)*
     $                 (ONE-F27*C(4,2)/CYP+F20*C0PP*C(4,1)/(C1PP*CYP))
C
      CZ10 = C(5,2)*(C12*FAC*C1PP)*
     $                 (ONE-F27*C(5,2)/CZP+F20*C0PP*C(5,1)/(C1PP*CZP))
C     !EJECT
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) DETER,C0P,C1P,ALF0,ALF1,ALFX,ALFY,ALFZ,
     $                   DIFK,C0PP,C1PP,CX10,CY10,CZ10
  100   format(' ','DETER=',1PE11.3,' C0P=',E11.3,' C1P=',E11.3/
     $         ' ','ALF0=',E11.3,' ALF1=',E11.3/
     $         ' ','ALFX=',E11.3,' ALFY=',E11.3,' ALFZ=',E11.3/
     $         ' ','DIFK=',E11.3,' C0PP=',E11.3,' C1PP=',E11.3/
     $         ' ','CX10=',E11.3,' CY10=',E11.3,' CZ10=',E11.3)
      end if
C     !END
      call BYE ('DFOCOF')
C
      return
      end
