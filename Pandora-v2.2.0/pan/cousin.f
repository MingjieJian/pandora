      subroutine COUSIN
C
C     Rudolf Loeser, 1991 Dec 16
C---- Computes CSN, a Hydrogen Stark broadening term, for EULE.
C     !DASH
      save
C     !DASH
      real*4 AUL, AUL1, AUL2, EL2, EPOW, FAC, P32, POW, UL4, UU2
C     !COM
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
C     !DASH
      external HI, BYE
C
      data AUL1, AUL2 /6.42E-1, 1.E0/
      data FAC /1.66E-13/
      data POW /6.66666667E-1/
      data P32 /1.5E0/
C
      call HI ('COUSIN')
C     !BEG
      if((IU-IL).eq.1) then
        AUL = AUL1
      else
        AUL = AUL2
      end if
C
      EPOW = XNE**POW
      UU2  = IU**2
      EL2  = IL**2
      UL4  = (UU2*EL2)**2
      CSN  = (AUL*FAC*EPOW*(UL4/(UU2-EL2)))*SMSK
C
      CSN32 = CSN**P32
C     !END
      call BYE ('COUSIN')
C
      return
      end
