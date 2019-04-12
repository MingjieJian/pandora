      subroutine LOST
     $(N,NSL,RKMLT,RKI,RLI,LU)
C
C     Rudolf Loeser, 2004 Dec 15
C---- Artificial RK enhancement.
C     !DASH
      save
C     !DASH
      real*8 FAC, HALF, ORKJ, RKI, RKMLT, RLI, TRMA, TRMB, ZERO
      integer I, J, LU, N, NSL
      logical PRNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
C     !EJECT
      external  LINER, SHIM, HI, BYE
      intrinsic min
C
C               RKMLT(NSL), RKI(N,NSL), RLI(N,NSL)
      dimension RKMLT(*),   RKI(N,*),   RLI(N,*)
C
      call HI ('LOST')
C     !BEG
      PRNT = LU.gt.0
C
      do 103 J = 1,NSL
        FAC = RKMLT(J)
        if(FAC.ne.ZERO) then
C
          do 102 I = 1,N
            ORKJ = RKI(I,J)
            TRMA = FAC*ORKJ
            TRMB = HALF*(RLI(I,J)+ORKJ)
C
            RKI(I,J) = min(TRMA,TRMB)
C
            if(PRNT) then
              if(I.eq.1) then
                call LINER (2, LU)
                write (LU,100) J,FAC
  100           format(' ','++++++++  Level ',I2,', RKMULT =',1PE12.4//
     $                 ' ',4X,'i',14X,'RL',5X,'computed RK',5X,
     $                    'enhanced RK')
                call LINER (1, LU)
              end if
              write (LU,101) I,RLI(I,J),ORKJ,RKI(I,J)
  101         format(' ',I5,1P3E16.8)
              call SHIM    (I, 5, LU)
            end if
  102     continue
C
        end if
  103 continue
C     !END
      call BYE ('LOST')
C
      return
      end
