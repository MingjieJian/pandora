      subroutine CORK
     $(ION,TE,LU,CI)
C
C     Rudolf Loeser, 2006 Aug 25
C---- Computes C I-V CI(1) according to Suno & Kato (2006).
C     !DASH
      save
C     !DASH
      real*8 CAN, CI, CON, DX, F, FAC, RT, SE, SIGMA, STEP, TE, XI, XNU,
     $       XNUK, Z, ZERO
      integer I, ION, LU, NT
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external CISCO, HART, HI, BYE
C
      parameter (NT=201)
      dimension XNU(NT), Z(NT), XNUK(5)
C
      data XNUK /2.724D0, 5.896D0, 11.579D0, 15.595D0, 94.806D0/
      data CAN,CON,STEP /1.4309D15, 47992.D0, 5.0D-2/
C
      call HI ('CORK')
C     !BEG
      CI = ZERO
C
      if(((ION.ge.1).and.(ION.le.5)).and.(TE.ne.ZERO)) then
        DUMP = LU.gt.0
        if(DUMP) then
          write (LU,100) ION,TE
  100     format(' ','Dump from Carbon-',I1,' CI1(T =',1PE12.4,')'//
     $           ' ',16X,'nu',11X,'exp',9X,'Sigma',5X,'integrand')
        end if
        XNU(1) = XNUK(ION)
        DX     = (TE/CON)*STEP
        do 101 I = 2,NT
          F      = I-1
          XNU(I) = XNU(1)+F*DX
  101   continue
        do 103 I = 1,NT
          call CISCO (ION, XNU(I), SIGMA)
          SE   = exp(-(CON*(XNU(I)-XNUK(ION)))/TE)
          Z(I) = XNU(I)*(SIGMA*SE)
          if(DUMP) then
            write (LU,102) I,XNU(I),SE,SIGMA,Z(I)
  102       format(' ',I4,1P4E14.6)
          end if
  103   continue
        call HART    (NT, XNU, Z, XI)
C
        RT  = sqrt(TE)
        FAC = CAN/(RT**3)
C
        CI = FAC*XI
        if(DUMP) then
          write (LU,104) XI,FAC,CI
  104     format(' ','integral =',1PE14.6,5X,'factor =',E14.6,5X,
     $               'CI(1) =',E14.6)
        end if
      end if
C     !END
      call BYE ('CORK')
C
      return
      end
