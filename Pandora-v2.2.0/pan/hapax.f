      subroutine HAPAX
     $(BLU,KB,RED,KR,FUL,KF,KODE)
C
C     Rudolf Loeser, 1985 Jul 02
C---- Sets up a full-profile table, from its two components.
C     KODE = 1 means: frequencies; and
C          = 2      : integration weights.
C     (This is version 3 of HAPAX.)
C     !DASH
      save
C     !DASH
      real*8 BLU, FAC, FUL, ONE, RED, ZERO
      integer I, IB, IR, KB, KF, KODE, KR, NB, NR
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  HALT, HI, BYE
      intrinsic max
C
C               BLU(KB), RED(KR), FUL(KF)
      dimension BLU(*),  RED(*),  FUL(*)
C     !EJECT
C
      call HI ('HAPAX')
C     !BEG
      if(KF.lt.max(KB,KR)) then
        write (MSSLIN(1),100) KF,KB,KR
  100   format('KF =',I12,', KB =',I12,', KR =',I12,'; this is ',
     $         'inappropriate.')
        call HALT ('HAPAX', 1)
      end if
C
C---- "Core" point
      if(KODE.eq.2) then
        FAC = +ONE
        FUL(KB) = BLU(1)+RED(1)
      else
        FAC = -ONE
        FUL(KB) = ZERO
      end if
C
      if(KB.gt.1) then
C----   Blue side
        IB = KB+1
        NB = KB-1
        do 101 I = 1,NB
          IB = IB-1
          FUL(I) = FAC*BLU(IB)
  101   continue
      end if
C
      if(KR.gt.1) then
C----   Red side
        IR = 1
        NR = KB+1
        do 102 I=NR,KF
          IR = IR+1
          FUL(I) = RED(IR)
  102   continue
      end if
C     !END
      call BYE ('HAPAX')
C
      return
      end
