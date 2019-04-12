      subroutine WINTRY
     $(NSL,XNUK,XNU,CP)
C
C     Rudolf Loeser, 2004 Nov 02
C---- Computes default values of CP (photoionization rate) if needed,
C     for non-H ions. Uses the hydrogenic approximation or,
C     for level 1 of some ions, the procedure of
C
C     Verner et al., 1996, ApJ, 465, 487.
C
C     !DASH
      save
C     !DASH
      real*8 CP, FAC, FE, SIG0, WVL, XNU, XNUK, ZERO
      integer INDEX, JDCP1, LS, NSL
      character INAME*3
C     !COM
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(23),JDCP1)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external PINA, FERMENT, VINIFY, HI, BYE
C
C               CP(NSL+1), XNU(NSL)
      dimension CP(*),     XNU(*)
C
      data FAC /1.0D-18/
C
      call HI ('WINTRY')
C     !BEG
      LS = 1
C
      if(CP(1).le.ZERO) then
        call PINA      (0, INAME, INDEX)
        if(INDEX.gt.0) then
          WVL = ZERO
          call FERMENT (INDEX, WVL, SIG0, FE)
          CP(1) = (SIG0*FE)*FAC
C
          JDCP1 = JDCP1+1
          LS = 2
        end if
      end if
C
      call VINIFY      (LS, NSL, XNUK, XNU, CP)
C     !END
      call BYE ('WINTRY')
C
      return
      end
