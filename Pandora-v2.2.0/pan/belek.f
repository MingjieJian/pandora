      subroutine BELEK
     $(LU,EXEN,JDIM,JMX,KMX)
C
C     Rudolf Loeser, 1994 Aug 16
C---- Prints CO excitation energies.
C     !DASH
      save
C     !DASH
      real*8 EXEN
      integer J, JDIM, JMX, K, KE, KMX, KS, LU
C     !DASH
      external  LINER, HI, BYE
      intrinsic min, mod
C
C               EXEN(JDIM,KDIM)
      dimension EXEN(JDIM,*)
C
      call HI ('BELEK')
C     !BEG
      if((LU.gt.0).and.(KMX.gt.0)) then
        KE = 0
  100   continue
          KS = KE+1
          KE = min((KE+10),KMX)
          call LINER     (1,LU)
C
          do 103 J = 1,JMX
            if(mod(J,10).eq.1) then
              call LINER (1,LU)
              write (LU,101) ((K-1),K=KS,KE)
  101         format(' ','  j',7X,10(:,3X,'v: ',I2,3X))
              call LINER (1,LU)
            end if
C
            write (LU,102) (J-1),(EXEN(J,K),K=KS,KE)
  102       format(' ',I3,7X,1P10E11.4)
  103     continue
C
        if(KE.lt.KMX) go to 100
      end if
C     !END
      call BYE ('BELEK')
C
      return
      end
