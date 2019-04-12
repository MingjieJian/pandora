      subroutine FATE
     $(CPJ,XNUJ,RJ,R,X,UPJ,KOOL, RKM,F)
C
C     Rudolf Loeser, 1979 Nov 26
C---- Computes the 1/nu**3 contribution to RK.
C     !DASH
      save
C     !DASH
      real*8 CPJ, E1, F, R, RJ, RKM, UPJ, X, XNUJ, dummy
      logical KOOL
C     !DASH
      external FRIGOR, EXPINT, HI, BYE
C
      call HI ('FATE')
C     !BEG
      call FRIGOR (CPJ,XNUJ,KOOL,F)
      call EXPINT (1,(UPJ*X),E1,dummy)
      RKM = F*RJ*R*(X**3)*E1
C     !END
      call BYE ('FATE')
C
      return
      end
