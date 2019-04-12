      subroutine ELMER
     $(TB,W,WP,EI,EIP,TCA,KOUNTA,TCB,KOUNTB,MXKOUNT,CRIT,A)
C
C     Rudolf Loeser, 1980 Oct 01
C---- Computes color temperatures, TCA and TCB, for ELMO.
C     !DASH
      save
C     !DASH
      real*8 A, AA, BL, BU, CRIT, EI, EIP, F, FKL, FKM, FKU, FKZ, FM,
     $       ONE, TB, TBL, TBU, TCA, TCB, TCM, TCZ, W, WP, XM, XZ, ZERO
      integer KOUNTA, KOUNTB, MXKOUNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HUNK, BRIDE, PLEAT, HI, BYE
C
      data  BU,BL /1.D2, 1.D-2/
      data  FM,XZ,XM /2.0304959D-2, 2.8214394D0, 3.8300161D0/
C     !EJECT
C
      call HI ('ELMER')
C     !BEG
      TCA = -ONE
      TCB = -ONE
      TBU =  TB*BU
      TBL =  TB*BL
      call HUNK        (XZ,W,2,TCZ)
      call HUNK        (XM,W,2,TCM)
C
      call BRIDE       (W,EI,WP,EIP,TBU,A,F,FKU)
C
      if(A.gt.FM) then
        TCA = ZERO
      else
        if(A.le.ZERO) then
          if(FKU.gt.ZERO) then
            call BRIDE (W,EI,WP,EIP,TCZ,AA,F,FKZ)
            call PLEAT (FKZ,TCZ,FKU,TBU,W,EI,WP,EIP,TCA,KOUNTA,MXKOUNT,
     $                  CRIT)
          end if
        else
          call BRIDE   (W,EI,WP,EIP,TCM,AA,F,FKM)
          call BRIDE   (W,EI,WP,EIP,TCZ,AA,F,FKZ)
          call PLEAT   (FKZ,TCZ,FKM,TCM,W,EI,WP,EIP,TCA,KOUNTA,MXKOUNT,
     $                  CRIT)
          call BRIDE   (W,EI,WP,EIP,TBL,AA,F,FKL)
          if(FKL.gt.ZERO) then
            call PLEAT (FKL,TBL,FKM,TCM,W,EI,WP,EIP,TCB,KOUNTB,MXKOUNT,
     $                  CRIT)
          end if
        end if
      end if
C     !END
      call BYE ('ELMER')
C
      return
      end
