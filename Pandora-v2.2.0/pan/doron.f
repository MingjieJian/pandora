      subroutine DORON
     $(IU,IL,TEMP,CE)
C
C     Rudolf Loeser, 1990 Nov 08
C---- Computes default values of CE for Hydrogen.
C     (Valid for 2 .le. IU .le. 15; used for IU = 3 only.)
C
C     Giovanardi, Natta and Palla (1987)
C                    Astron.Astrophys.Suppl.,70,269-280
C
C     Giovanardi and Palla (1989)
C                    Astron.Astrophys.Suppl.,77,157-160
C     !DASH
      save
C     !DASH
      real*8 CE, ELL2, FAC, RTE, SIGMA, TE, TEMP, TERM, TLIM1, TLIM2,
     $       ZERO
      integer IL, IU, M
      logical INOK, TEOK
      character LAB*7
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  SUMTERM, HI, BYE
      intrinsic max, min
C
      dimension TERM(12), LAB(12)
C
      data TLIM1,TLIM2,FAC /2.D3, 5.D5, 4.315D-6/
C
      call HI ('DORON')
C     !BEG
      CE = ZERO
C
      INOK = (IU.eq.3).and.(IL.lt.IU).and.(IL.gt.0)
      TEOK = TEMP.gt.ZERO
      if(INOK.and.TEOK) then
        TE = min(max(TEMP,TLIM1),TLIM2)
        call SUMTERM (IU,IL,TE,SIGMA,TERM,LAB,M)
        RTE  = sqrt(TE)
        ELL2 = IL**2
C
        CE = (FAC/(ELL2*RTE))*SIGMA
      end if
C     !END
      call BYE ('DORON')
C
      return
      end
