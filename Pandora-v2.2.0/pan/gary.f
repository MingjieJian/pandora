      subroutine GARY
     $(Z,N,ZECL,IND,NZE)
C
C     Rudolf Loeser, 1993 Jun 15
C---- Computes Z-selection indices, for GRIFFIN.
C     !DASH
      save
C     !DASH
      real*8 DL, DR, Z, ZECL, ZERO
      integer I, IND, K, LOOK, N, NOTE, NZE
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  LOOKSD, HALT, HI, BYE
      intrinsic abs
C
C               Z(N), ZECL(NZE), IND(NZE)
      dimension Z(*), ZECL(*),   IND(*)
C     !EJECT
C
      call HI ('GARY')
C     !BEG
      do 101 I = 1,NZE
        call LOOKSD (Z,N,ZERO,ZECL(I), K,NOTE,LOOK)
C
        if(LOOK.eq.1) then
          DL = abs(ZECL(I)-Z(K  ))
          DR = abs(ZECL(I)-Z(K+1))
          if(DL.le.DR) then
            IND(I) = K
          else
            IND(I) = K+1
          end if
        else if((LOOK.eq.2).or.(LOOK.eq.3)) then
          IND(I) = N
        else if(LOOK.eq.4) then
          IND(I) = 1
        else
          write (MSSLIN(1),100) LOOK
  100     format('LOOK =',I12,', which is not 1, 2, 3, or 4.')
          call HALT ('GARY',1)
        end if
C
  101 continue
C     !END
      call BYE ('GARY')
C
      return
      end
