      subroutine PEYTRE
     $(V,A,Z)
C     Computes the Voigt profile, fast.
C
C     Original routine written by   E r i c    P e y t r e m a n n.
C
C     (Minor revisions by R. Loeser, 1977 Oct 31.)
C     !DASH
      save
C     !DASH
      real*4 A, AA, F, H1, H2, HH1, HH2, HH3, HH4, HO, U, UU, V, VV, Z
      logical Q
C
C     !BEG
      Q = (A.lt.0.2)
C
      VV = V*V
      if(Q.and.(V.gt.5.0)) then
        F = ((2.12/VV+0.8463)/VV+0.5642)*A/VV
      else if(.not.Q.and.((A.gt.1.4).or.((A+V).gt.3.2))) then
        AA = A*A
        U  = (AA+VV)*1.4142
        UU = U*U
        F  = ((((AA-10.*VV)*3.*AA+15.*VV*VV)/UU+3.*VV-AA)/UU+1.)
     $       *A*0.79788/U
      else
        HO = exp(-VV)
        H2 = (1.-2.*VV)*HO
        if(V.le.1.3) then
          H1 = (0.42139*VV-2.34358*V+3.28868)*VV-0.15517*V-1.1247
        else if(V.le.2.4) then
          H1 = (-0.220416*VV+1.989196*V-6.61487)*VV+9.39456*V-4.4848
        else
          H1 = ((-0.0032783*VV+0.0429913*V-0.188326)*VV
     $         +0.278712*V+0.55415)/(VV-1.5)
        end if
        if(Q) then
          F = (H2*A+H1)*A+HO
        else
          HH1 = H1+HO*1.12838
          HH2 = H2+HH1*1.12838-HO
          HH3 = (1.-H2)*0.37613-HH1*0.66667*VV+HH2*1.12838
          HH4 = (3.*HH3-HH1)*0.37613+HO*0.66667*VV*VV
          F = ((((HH4*A+HH3)*A+HH2)*A+HH1)*A+HO)
     $        *(((-0.122727278*A+0.532770573)*A-0.96284325)*A
     $        +0.979895032)
        end if
      end if
      Z = 0.564189583547756*F
C     !END
C
      return
      end
