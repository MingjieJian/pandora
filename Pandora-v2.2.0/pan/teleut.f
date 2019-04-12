      subroutine TELEUT
     $(NO,RKI,RLI,CIJAD,CKIAD,N,NL,NSL)
C
C     Rudolf Loeser, 1981 Oct 26
C---- Prints Rates input values, and related quantities.
C     !DASH
      save
C     !DASH
      real*8 CIJAD, CKIAD, RKI, RLI
      integer I, IJ, J, N, NL, NNSL, NO, NSL
      logical DOIT, KCAD, KKAD, KRKI, KRLI
C     !DASH
      external NAUGHTD, INDXIJ, PADMA, PRIVET, LINER, HI, BYE
C
C               RKI(N,NSL), RLI(N,NSL), CKIAD(N,NSL), CIJAD(N,NL**2)
      dimension RKI(N,*),   RLI(N,*),   CKIAD(N,*),   CIJAD(N,*)
C
      call HI ('TELEUT')
C     !BEG
      DOIT = NO.gt.0
      if(DOIT) then
        NNSL = N*NSL
        call NAUGHTD          (RKI  ,1,NNSL     ,KRKI)
        call NAUGHTD          (RLI  ,1,NNSL     ,KRLI)
        call NAUGHTD          (CKIAD,1,NNSL     ,KKAD)
        call NAUGHTD          (CIJAD,1,(N*NL*NL),KCAD)
        DOIT = (.not.KRKI).or.(.not.KRLI).or.(.not.KKAD).or.(.not.KCAD)
      end if
C     !EJECT
      if(DOIT) then
        call PADMA          (NO,'Input Values of Rates, etc.')
C
        do 102 J = 1,NSL
C
          call NAUGHTD      (RKI(1,J),1,N,KRKI)
          if(.not.KRKI) then
            call LINER      (1,NO)
            write (NO,100) 'RK    ',J
  100       format(' ',A6,2I3)
            call PRIVET     (NO,RKI(1,J),N)
          end if
C
          call NAUGHTD      (RLI(1,J),1,N,KRLI)
          if(.not.KRLI) then
            call LINER      (1,NO)
            write (NO,100) 'RL    ',J
            call PRIVET     (NO,RLI(1,J),N)
          end if
C
          call NAUGHTD      (CKIAD(1,J),1,N,KKAD)
          if(.not.KKAD) then
            call LINER      (1,NO)
            write (NO,100) 'CKADD ',J
            call PRIVET     (NO,CKIAD(1,J),N)
          end if
C
          if(J.le.NL) then
            do 101 I = 1,NL
              call INDXIJ   (I,J,IJ)
              call NAUGHTD  (CIJAD(1,IJ),1,N,KCAD)
              if(.not.KCAD) then
                call LINER  (1,NO)
                write (NO,100) 'CIJADD',I,J
                call PRIVET (NO,CIJAD(1,IJ),N)
              end if
  101       continue
          end if
C
  102   continue
C
      end if
C     !END
      call BYE ('TELEUT')
C
      return
      end
