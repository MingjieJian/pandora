      subroutine HUGRA
     $(QELSM,NSL,MRJ,WRAT,XNU,XNUC,RRCP)
C
C     Rudolf Loeser, 1990 Oct 12
C---- Sets up default values of RRCP,
C     ratio of photoionization cross-section.
C     !DASH
      save
C     !DASH
      real*8 ONE, RCP, RRCP, WRAT, WRATH, XNU, XNUC, ZERO
      integer I, IR, J, JDRCP, MRJ, NSL, jummy
      logical GOOD, HYDR, ZRCP
      character QELSM*8
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(14),JDRCP)
C     !DASH
C     !EJECT
      external MINK, GUNNAR, NOMAD, HOG, HOY, NAUGHTD, HI, BYE
C
C               MRJ(NSL+1), WRAT(MRS), RRCP(MRS), XNU(NSL), XNUC(NSL)
      dimension MRJ(*),     WRAT(*),   RRCP(*),   XNU(*),   XNUC(*)
C
      call HI ('HUGRA')
C     !BEG
      HYDR = QELSM(:3).eq.'H  '
C
      do 101 J = 1,NSL
        call MINK           (J, MRJ, jummy, IR)
C
        if(HYDR) then
          WRATH = WRAT(IR)
          if(WRATH.eq.ONE) then
            call NOMAD      (ONE, XNU, XNUC, J, WRATH)
          end if
          if(RRCP(IR).eq.-ONE) then
            call GUNNAR     (J, WRATH, RRCP(IR))
            JDRCP = JDRCP+1
          end if
          if(MRJ(J).gt.0) then
            do 100 I = 1,MRJ(J)
              IR = IR+1
              if(RRCP(IR).eq.ZERO) then
                call GUNNAR (J, WRAT(IR), RCP)
                RRCP(IR) = RCP*((WRAT(IR)/WRATH)**3)
                JDRCP = JDRCP+1
              end if
  100       continue
          end if
        else
          if(RRCP(IR).eq.-ONE) then
            RRCP(IR) = ONE
          end if
          if(MRJ(J).gt.0) then
            call NAUGHTD    (RRCP(IR+1), 1, MRJ(J), ZRCP)
            if(ZRCP) then
              GOOD = .false.
              if(J.eq.1) then
                call HOG    (MRJ(1), WRAT(IR+1), RRCP(IR+1), GOOD)
              end if
              if(.not.GOOD) then
                WRATH = WRAT(IR)
                call HOY    (WRATH, MRJ(J), WRAT(IR+1), RRCP(IR+1))
              end if
            end if
          end if
        end if
C
  101 continue
C     !END
      call BYE ('HUGRA')
C
      return
      end
