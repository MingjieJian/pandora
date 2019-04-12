      subroutine TRUCK
     $(ONL,KODE,QAR,KNT)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Converts "ONL" into alphabetic message regarding line profile
C     calculation, for LIZARD.
C     (This is version 3 of TRUCK.)
C     !DASH
      save
C     !DASH
      real*8 ARR, ONL, ZERO
      integer I, IONL, J, KNT, KODE
      character QAR*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  GRINDLE, HALT, HI, BYE
      intrinsic mod
C
C               ONL(8), QAR(16)
      dimension ONL(*), QAR(*)
C
      dimension ARR(8)
C     !EJECT
C
      call HI ('TRUCK')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1 nor 2.')
        call HALT  ('TRUCK',1)
      end if
C
      do 101 I = 1,KNT
        ARR(I) = ZERO
        IONL = ONL(I)
C
        if(IONL.gt.0) then
          IONL = IONL-1
          if(KODE.eq.1) then
            J = mod(IONL,2)
          else
            J = IONL/2
          end if
          ARR(I) = J+1
        end if
C
  101 continue
C
      call GRINDLE (ARR,QAR,KNT)
C     !END
      call BYE ('TRUCK')
C
      return
      end
