      subroutine SAHARA
     $(XNUK,TE,PF,SA)
C
C     Rudolf Loeser, 1981 Jun 30
C---- Computes the Saha-Boltzmann function.
C     (This is version 3 of SAHARA.)
C     !DASH
      save
C     !DASH
      real*8 CON23, CONST, HALF, HNUKT, PF, RT, SA, SE, TE, XNUK
      logical KILROY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external DIVIDE, RIGEL, PROD, HI, BYE
C
      data KILROY /.true./
C
      call HI ('SAHARA')
C     !BEG
      if(KILROY) then
        call RIGEL (23, CON23)
        CONST  = HALF*CON23
        KILROY = .false.
      end if
C
      call PROD    (TE, XNUK, 1, HNUKT, SE)
      RT = sqrt(TE)
      call DIVIDE  (CONST, (SE*PF*(RT**3)), SA)
C     !END
      call BYE ('SAHARA')
C
      return
      end
