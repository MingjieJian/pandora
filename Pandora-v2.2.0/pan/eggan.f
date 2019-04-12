      subroutine EGGAN
     $(INDX,XLM,N,NOPAC,HND,CONT)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Computes a set of X-Ray absorption values.
C     (This is version 3 of EGGAN.)
C     !DASH
      save
C     !DASH
      real*8 CONT, HND, XLM
      integer INDX, J, N, NOPAC
      logical YES
C     !COM
C---- XRAYLIM     as of 1986 Mar 06
      real*8      XRAYLO,XRAYHI
      common      /XRAYLIM/ XRAYLO,XRAYHI
C     Wavelength limits (Angstroms) for X-ray opacity.
C     .
C     !DASH
      external WITHIN, REFLECT, ZEROD, HI, BYE
C
C               HND(N), CONT(Nopac,N)
      dimension HND(*), CONT(NOPAC,*)
C
      call HI ('EGGAN')
C     !BEG
      call WITHIN      (XRAYLO, XLM, XRAYHI, 0, YES)
      if(YES) then
        do 100 J = 1,N
          call REFLECT (XLM, HND(J), CONT(INDX,J))
  100   continue
      else
        call ZEROD     (CONT(INDX,1), NOPAC, N)
      end if
C     !END
      call BYE ('EGGAN')
C
      return
      end
