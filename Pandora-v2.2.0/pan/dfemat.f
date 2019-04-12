      subroutine DFEMAT
     $(EMT,CNOS,PART,VCM,TE,AMASS,PHYD,PHEL,D01,D01T,ALFX,ALFY,ALFZ,
     $ CX10,CY10,CZ10,DUMP)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes E-matrix for DEEHEE (q.v.).
C     !DASH
      save
C     !DASH
      real*8 ALFX, ALFY, ALFZ, AMASS, ARRAS, CAR, CNOS, CX10, CY10,
     $       CZ10, D01, D01T, EMT, H0T, H1T, HALF, PART, PHEL, PHYD, TE,
     $       VCM, VTERMS
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
      external LINER, HI, BYE
C
C               EMT(3,5), CNOS(5,5), PART(5)
      dimension EMT(3,*), CNOS(5,*), PART(*)
C
      dimension ARRAS(3), CAR(3)
C
      data HALF /5.D-1/
C     !EJECT
C
      call HI ('DFEMAT')
C     !BEG
      H0T = PART(1)/PHYD
      H1T = PART(2)/PHYD
      ARRAS(1) = H1T*CNOS(3,1)-H0T*CNOS(3,2)
      ARRAS(2) = H1T*CNOS(4,1)-H0T*CNOS(4,2)
      ARRAS(3) = H1T*CNOS(5,1)-H0T*CNOS(5,2)
      CAR(1) = -CX10-ARRAS(1)
      CAR(2) = -CY10-ARRAS(2)
      CAR(3) = -CZ10-ARRAS(3)
      VTERMS =  HALF*VCM*TE/AMASS
C
      EMT(1,1) =  D01*CAR(1)
      EMT(2,1) =  D01*CAR(2)
      EMT(3,1) =  D01*CAR(3)
C
      EMT(1,2) =  VTERMS
      EMT(2,2) =  VTERMS
      EMT(3,2) =  VTERMS
C
      EMT(1,3) = -VTERMS*((        PART(4)+PART(5))/PHEL)
      EMT(2,3) =  VTERMS*((PART(3)                )/PHEL)
      EMT(3,3) =  EMT(2,3)
C
      EMT(1,4) = -VTERMS*((                PART(5))/PHEL)
      EMT(2,4) =  EMT(1,4)
      EMT(3,4) =  VTERMS*((PART(3)+PART(4)        )/PHEL)
C
      EMT(1,5) = -VTERMS*ALFX+D01T*CAR(1)
      EMT(2,5) = -VTERMS*ALFY+D01T*CAR(2)
      EMT(3,5) = -VTERMS*ALFZ+D01T*CAR(3)
C
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,100) H0T,H1T,VTERMS,ARRAS,CAR
  100   format(' ','H0T=',1PE11.3,' H1T=',E11.3,' VTERMS=',E11.3/
     $         ' ','ARRAS=',10X,3E11.3/
     $         ' ','CAR=',10X,3E11.3)
        call LINER (1, LUEO)
        write (LUEO,101) ((EMT(I,J),J=1,5),I=1,3)
  101   format(' ','E-matrix',10X,1P5E11.3)
      end if
C     !END
      call BYE ('DFEMAT')
C
      return
      end
