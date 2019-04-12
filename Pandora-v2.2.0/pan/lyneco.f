      subroutine LYNECO
     $(NP,NT,NV,TABP,TABT,TABV,N,PR,TE,V,C01,C02,C03,C04,C05,C06,
     $ IPJ,ITJ,IVJ)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Computes interpolation coefficients for Composite Line opacity.
C     Based on "LINOP" of R. Kurucz, May 1983.
C     !DASH
      save
C     !DASH
      real*8 C01, C02, C03, C04, C05, C06, OMX, OMY, ONE, PL, PR, TABP,
     $       TABT, TABV, TE, THSNTH, TL, V, VV, X, Y, Z, ZERO
      integer IP, IPJ, IT, ITJ, IV, IVJ, J, N, NP, NT, NV
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic min, max
C
C               C01(N), C02(N), C03(N), C04(N), C05(N), C06(N), IVJ(N),
      dimension C01(*), C02(*), C03(*), C04(*), C05(*), C06(*), IVJ(*),
C
C               IPJ(N), ITJ(N), TABV(NV), TABT(NT), PR(N), TE(N), V(N),
     $          IPJ(*), ITJ(*), TABV(*),  TABT(*),  PR(*), TE(*), V(*),
C
C               TABP(NP)
     $          TABP(*)
C
      data THSNTH /1.D-3/
C
      call HI ('LYNECO')
C     !BEG
      do 106 J = 1,N
C
C----   Get TL, the edited value of log(Temperature)
        TL = log10(TE(J))
        TL = max((min(TL,TABT(NT))),TABT(1))
C
C----   Compute index of first larger temperature gridpoint
        do 100 IT = 2,NT
          if(TL.lt.TABT(IT)) then
            goto 101
          end if
  100   continue
        IT = NT
  101   continue
C
        ITJ(J) = IT
C
C----   Get PL, the edited value of log(Pressure)
        PL = log10(PR(J))
        PL = max((min(PL,TABP(NP))),TABP(1))
C
C----   Compute index of first larger pressure gridpoint
        do 102 IP = 2,NP
          if(PL.lt.TABP(IP)) then
            goto 103
          end if
  102   continue
        IP = NP
  103   continue
C
        IPJ(J)=IP
C     !EJECT
C----   Get VV, the edited value of Velocity
        VV = V(J)
        VV = max((min(VV,TABV(NV))),TABV(1))
C
        if(NV.gt.1) then
C----     Compute index of first larger velocity gridpoint
          do 104 IV = 2,NV
            if(VV.lt.TABV(IV)) then
              goto 105
            end if
  104     continue
          IV = NV
  105     continue
        else
          IV = 1
        end if
C
        IVJ(J)=IV
C
C----   Compute interpolation coefficients
C       (Note: KAPSAMP values are log10s, scaled up by 1000)
        X   = (TL-TABT(IT-1))/(TABT(IT)-TABT(IT-1))
        Y   = (PL-TABP(IP-1))/(TABP(IP)-TABP(IP-1))
        OMX = ONE-X
        OMY = ONE-Y
C
        C01(J) = OMX*OMY*THSNTH
        C02(J) = OMX*  Y*THSNTH
        C03(J) =   X*OMY*THSNTH
        C04(J) =   X*  Y*THSNTH
C
        if(NV.gt.1) then
          Z = (VV-TABV(IV-1))/(TABV(IV)-TABV(IV-1))
          C05(J) = Z
          C06(J) = ONE-Z
        else
          C05(J) = ONE
          C06(J) = ZERO
        end if
C
  106 continue
C     !END
      call BYE ('LYNECO')
C
      return
      end
