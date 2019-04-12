      subroutine DISKO
     $(LU,WN,OSC,JDIM,JMX,KMX,KNC,KUD)
C
C     Rudolf Loeser, 1987 Nov 13
C---- Prints CO wavenumbers and oscillator strengths.
C               KUD =1 for UP  : J > J+1;
C                   =2     DOWN: J > J-1.
C     !DASH
      save
C     !DASH
      real*8 OSC, WN
      integer J, JDIM, JMX, K, KE, KMX, KNC, KS, KUD, LU
      character LAB*10
C     !DASH
      external  BRUNY, LINER, HI, BYE
      intrinsic min, mod
C
C               WN(JDIM,KDIM), OSC(JDIM,KDIM)
      dimension WN(JDIM,*),    OSC(JDIM,*)
C
      call HI ('DISKO')
C     !BEG
      if((LU.gt.0).and.(KMX.gt.0)) then
C
        KE = 0
  100   continue
          KS = KE+1
          KE = min((KE+10),KMX)
C
          call LINER     (1,LU)
          do 104 J = 1,JMX
            if(mod(J,5).eq.1) then
              call LINER (1,LU)
              write (LU,101) ((K-1),(K-1+KNC),K=KS,KE)
  101         format(' ','  j',7X,10(:,3X,'v: ',I2,'-',I2))
              call LINER (1,LU)
            end if
C
            call BRUNY   (J,KUD,LAB)
            write (LU,102) LAB,(WN(J,K),K=KS,KE)
  102       format(' ',A10,10F11.4)
            write (LU,103)     (OSC(J,K),K=KS,KE)
  103       format(' ',10X,1P10E11.4)
  104     continue
C
        if(KE.lt.KMX) go to 100
      end if
C     !END
      call BYE ('DISKO')
C
      return
      end
